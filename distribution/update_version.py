#!/usr/bin/python

"""
Update files in this modflow6 repository according to release information.

This script is used to update several files in the modflow6 repository, including:

  ../version.txt
  ../meson.build
  ../doc/version.tex
  ../README.md
  ../DISCLAIMER.md
  ../code.json
  ../src/Utilities/version.f90

Information in these files include version number (major.minor.patch[label]), build timestamp,
whether or not the release is preliminary/provisional or official/approved, whether the source
code should be compiled in develop mode (IDEVELOPMODE = 1) or for release, and other metadata.

The version number is read from ../version.txt, which contains major, minor, and patch version
numbers, and an optional label. Version numbers are substituted into source code, latex files,
markdown files, etc. The version number can be provided explicitly using --version, short -v.

If the --releasemode flag is provided, IDEVELOPMODE is set to 0 in src/Utilities/version.f90.
Otherwise, IDEVELOPMODE is set to 1.

if the --approved flag (short -a) is provided, the disclaimer in src/Utilities/version.f90 and
the README/DISCLAIMER markdown files is modified to reflect review and approval. Otherwise the
language reflects preliminary/provisional status, and version strings contain "(preliminary)".
"""
import argparse
import json
import os
import textwrap
from collections import OrderedDict
from datetime import datetime
from packaging.version import Version
from pathlib import Path
from typing import Optional

import pytest
from filelock import FileLock
import yaml

from utils import get_modified_time

project_name = "USG-Transport"
project_root_path = Path(__file__).resolve().parent.parent
version_file_path = project_root_path / "version.txt"
touched_file_paths = [
    version_file_path,
    project_root_path / "meson.build",
    project_root_path / "README.md",
    project_root_path / "code.json",
]

def log_update(path, version: Version):
    print(f"Updated {path} with version {version}")


def update_meson_build(version: Version):
    path = project_root_path / "meson.build"
    lines = open(path, "r").read().splitlines()
    with open(path, "w") as f:
        for line in lines:
            if "version:" in line and "meson_version:" not in line:
                line = f"  version: '{version}',"
            f.write(f"{line}\n")
    log_update(path, version)


def update_codejson(version: Version, timestamp: datetime, approved: bool = False):
    path = project_root_path / "code.json"
    with open(path, "r") as f:
        data = json.load(f, object_pairs_hook=OrderedDict)

    data[0]["date"]["metadataLastUpdated"] = timestamp.strftime("%Y-%m-%d")
    data[0]["version"] = str(version)
    data[0]["status"] = "Release" if approved else "Preliminary"
    with open(path, "w") as f:
        json.dump(data, f, indent=4)
        f.write("\n")

    log_update(path, version)


def update_version(
    version: Version = None,
    timestamp: datetime = datetime.now(),
    approved: bool = False,
    developmode: bool = True,
):
    """
    Update version information stored in version.txt in the project root,
    as well as several other files in the repository. Version updates are
    performed by explicitly providing a version argument to this function
    and a lock is held on the version file to make sure that the state of
    the multiple files containing version information stays synchronized.
    If no version argument is provided, the version number isn't changed.
    """

    lock_path = Path(version_file_path.name + ".lock")
    try:
        lock = FileLock(lock_path)
        previous = Version(version_file_path.read_text().strip())
        version = (
            version
            if version
            else previous
        )

        with lock:
            update_meson_build(version)
            update_codejson(version, timestamp, approved)
    finally:
        lock_path.unlink(missing_ok=True)


_initial_version = Version("0.0.1")
_current_version = Version(version_file_path.read_text().strip())


@pytest.mark.skip(reason="reverts repo files on cleanup, tread carefully")
@pytest.mark.parametrize(
    "version",
    [
        None,
        _initial_version,
        Version(f"{_initial_version.major}.{_initial_version.minor}.dev{_initial_version.micro}"),
    ],
)
@pytest.mark.parametrize("approved", [True, False])
@pytest.mark.parametrize("developmode", [True, False])
def test_update_version(version, approved, developmode):
    m_times = [get_modified_time(file) for file in touched_file_paths]
    timestamp = datetime.now()

    try:
        update_version(
            timestamp=timestamp,
            version=version,
            approved=approved,
            developmode=developmode,
        )
        updated = Version(version_file_path.read_text().strip())

        # check files containing version info were modified
        for p, t in zip(touched_file_paths, m_times):
            assert p.stat().st_mtime > t

        # check version number and optional label are correct
        if version:
            # version should be auto-incremented
            assert updated == _initial_version
        else:
            # version should not have changed
            assert updated == _current_version

        # check IDEVELOPMODE was set correctly
        #version_f90_path = project_root_path / "src" / "Utilities" / "version.f90"
        #lines = version_f90_path.read_text().splitlines()
        #assert any(
        #    f"IDEVELOPMODE = {1 if developmode else 0}" in line for line in lines
        #)

    finally:
        for p in touched_file_paths:
            os.system(f"git restore {p}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="Update Modflow USG-Transport version",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent(
            """\
            Update version information stored in version.txt in the project root,
            as well as several other files in the repository. If --version is not
            provided, the version number will not be changed. A file lock is held
            to synchronize file access. To indicate a version is production-ready
            use --approve. This will change the disclaimer and version tag label,
            removing '(preliminary)' from the latter, and modifying the former to
            reflect approval The --releasemode flag controls whether IDEVELOPMODE
            is set to 0 instead of the default 1. The version tag must follow the
            '<major>.<minor>.<patch>' format conventions for semantic versioning.
            """
        ),
    )
    parser.add_argument(
        "-v",
        "--version",
        required=False,
        help="Specify the release version",
    )
    parser.add_argument(
        "-a",
        "--approved",
        required=False,
        action="store_true",
        help="Approve the release version (defaults to false for preliminary/development distributions)",
    )
    parser.add_argument(
        "-r",
        "--releasemode",
        required=False,
        action="store_true",
        help="Set IDEVELOPMODE to 0 for release mode (defaults to false for development distributions)",
    )
    parser.add_argument(
        "-g",
        "--get",
        required=False,
        action="store_true",
        help="Get the current version number, don't update anything (defaults to false)",
    )
    args = parser.parse_args()
    if args.get:
        print(Version((project_root_path / "version.txt").read_text().strip()))
    else:
        print(f"Updating to version {args.version} with options")
        print(f"    releasemode: {args.releasemode}")
        print(f"    approved: {args.approved}")
        version = Version(args.version) if args.version else _current_version
        update_version(
            version=version,
            timestamp=datetime.now(),
            approved=args.approved,
            developmode=not args.releasemode,
        )
