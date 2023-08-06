import os
import sys
from os import environ
from pathlib import Path

import pymake
import pytest
from flaky import flaky
from modflow_devtools.markers import requires_exe
from modflow_devtools.misc import set_dir

from utils import get_modified_time, get_project_root_path

_project_root_path = get_project_root_path()
_is_windows = sys.platform.lower() == "win32"
_ext = ".exe" if _is_windows else ""

FC = environ.get("FC")
_fc_reason = "make must be used with gfortran"


def run_makefile(target):
    assert Path("makefile").is_file(), f"makefile does not exist in {os.getcwd()}"

    base_target = os.path.basename(target)
    base_message = (
        f" Rerunning {os.path.basename(__file__)} in the distribution "
        "directory and recomitting modified makefiles will likely resolve "
        "CI failures."
    )

    # clean prior to make
    print(f"clean {base_target} with makefile")
    os.system("make clean")

    # build MODFLOW 6 with makefile
    print(f"build {base_target} with makefile")
    return_code = os.system(f"make FC={environ.get('FC', 'gfortran')}")

    assert return_code == 0, f"could not make '{base_target}'." + base_message
    assert os.path.isfile(target), f"{base_target} does not exist." + base_message


def build_usg_makefile():
    target = "mfusg"
    excludefiles = str(_project_root_path / "pymake" / "excludefiles.txt")
    print(f"Creating makefile for {target}")
    with set_dir(_project_root_path / "make"):
        pymake.main(
            srcdir=str(_project_root_path / "src"),
            target=target,
            appdir=str(_project_root_path / "bin"),
            include_subdirs=True,
            excludefiles=excludefiles,
            inplace=True,
            fflags = "-fdec",
            dryrun=True,
            makefile=True,
            networkx=True,
        )


@flaky
@pytest.mark.skipif(FC == "ifort", reason=_fc_reason)
def test_build_usg_makefile():
    makefile_paths = [
        _project_root_path / "make" / "makefile",
        _project_root_path / "make" / "makedefaults",
    ]
    makefile_mtimes = [p.stat().st_mtime for p in makefile_paths]

    try:
        build_usg_makefile()

        # check files were modified
        for p, t in zip(makefile_paths, makefile_mtimes):
            assert p.stat().st_mtime > t
    finally:
        for p in makefile_paths:
            os.system(f"git restore {p}")


@flaky
@requires_exe("make")
@pytest.mark.skipif(FC == "ifort", reason=_fc_reason)
def test_build_usg_with_make():
    target = _project_root_path / "bin" / f"usg-transport{_ext}"
    mtime = get_modified_time(target)

    try:
        with set_dir(_project_root_path / "make"):
            run_makefile(target)

        # check executable was modified
        assert target.stat().st_mtime > mtime
    finally:
        # clean after successful make
        print(f"clean {target} with makefile")
        os.system("make clean")


if __name__ == "__main__":
    build_usg_makefile()
