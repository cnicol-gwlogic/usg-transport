import argparse
import os
import platform
import shutil
import sys
import textwrap
from os import PathLike, environ
from pathlib import Path
from pprint import pprint
from shutil import copytree

import pytest
from modflow_devtools.build import meson_build
from modflow_devtools.download import download_and_unzip, get_release
from modflow_devtools.markers import requires_exe
from modflow_devtools.misc import get_model_paths

from build_makefiles import (build_mf6_makefile)
from utils import get_project_root_path, run_command

# default paths
_project_root_path = get_project_root_path()
#_examples_repo_path = _project_root_path.parent / "usg-transport-examples"
_build_path = _project_root_path / "builddir"

# OS-specific extensions
_system = platform.system()
_eext = ".exe" if _system == "Windows" else ""
_soext = ".dll" if _system == "Windows" else ".so" if _system == "Linux" else ".dylib"
_scext = ".bat" if _system == "Windows" else ".sh"
_executable = f"usg-transport{_eext}"

# Fortran and C compilers
FC = environ.get("FC", "gfortran")
CC = environ.get("CC", "gcc")


def copy_sources(output_path: PathLike):
    output_path = Path(output_path).expanduser().absolute()

    # make sure output directory exists
    output_path.mkdir(exist_ok=True)

    # Copy Visual Studio sln and project files
    """print("Copying msvs files to output directory")
    (output_path / "msvs").mkdir(exist_ok=True)
    source_msvs_path = _project_root_path / "msvs"
    for d in [
        str(source_msvs_path / "usg-transport.sln"),
        str(source_msvs_path / "usg-transport.vfproj"),
    ]:
        shutil.copy(d, output_path / "msvs")"""

    ignored = shutil.ignore_patterns(".DS_Store")

    # copy top-level meson.build and meson.options
    shutil.copy(_project_root_path / "meson.build", output_path)
    shutil.copy(_project_root_path / "meson.options", output_path)

    # copy source folder
    src_path = _project_root_path / "src"
    dst_path = output_path / "src"
    print(f"Copying {src_path} to {dst_path}")
    copytree(src_path, dst_path, ignore=ignored)
    

def test_copy_sources(tmp_path):
    copy_sources(tmp_path)

    assert (tmp_path / "src").is_dir()
    # assert (tmp_path / "msvs").is_dir()

    assert (tmp_path / "src" / "meson.build").is_file()
    # assert (tmp_path / "msvs" / "usg-transport.sln").is_file()


def build_programs_meson(
    build_path: PathLike, bin_path: PathLike, overwrite: bool = False
):
    build_path = Path(build_path).expanduser().absolute()
    bin_path = Path(bin_path).expanduser().absolute()

    exe_paths = [
        bin_path / f"usg-transport{_eext}",
    ]

    if (
        not overwrite
        and all(p.is_file() for p in exe_paths)
    ):
        print(f"Binaries already exist:")
        pprint(exe_paths)
    else:
        print(f"Building binaries in {build_path}, installing to {bin_path}")
        meson_build(
            project_path=_project_root_path, build_path=build_path, bin_path=bin_path
        )

    for target in exe_paths:
        assert target.is_file(), f"Failed to build {target}"
        target.chmod(target.stat().st_mode | 0o111)
        print(f"Execute permission set for {target}")


def test_build_programs_meson(tmp_path):
    build_programs_meson(tmp_path / "builddir", tmp_path / "bin")


def build_makefiles(output_path: PathLike):
    output_path = Path(output_path).expanduser().absolute()

    # create and copy mf6 makefile
    build_mf6_makefile()
    (output_path / "make").mkdir(parents=True, exist_ok=True)
    shutil.copyfile(
        _project_root_path / "make" / "makefile", output_path / "make" / "makefile"
    )
    shutil.copyfile(
        _project_root_path / "make" / "makedefaults",
        output_path / "make" / "makedefaults",
    )


def test_build_makefiles(tmp_path):
    build_makefiles(tmp_path)

    assert (tmp_path / "make" / "makefile").is_file()
    assert (tmp_path / "make" / "makedefaults").is_file()

    os.system(f"cd {tmp_path} && make -f make/makefile")


def build_distribution(
    build_path: PathLike,
    output_path: PathLike,
    full: bool = False,
    overwrite: bool = False,
):
    #examples_repo_path: PathLike,
    print(f"Building {'full' if full else 'minimal'} distribution")

    build_path = Path(build_path).expanduser().absolute()
    output_path = Path(output_path).expanduser().absolute()
    #examples_repo_path = Path(examples_repo_path).expanduser().absolute()

    # binaries
    build_programs_meson(
        build_path=build_path, bin_path=output_path / "bin", overwrite=overwrite
    )

    # code.json metadata
    shutil.copy(_project_root_path / "code.json", output_path)

    # full releases include examples, source code, makefiles and docs
    if full:

        # copy source code files
        copy_sources(output_path=output_path)

        # build and copy makefiles
        build_makefiles(output_path=output_path)


@pytest.mark.skip(reason="manual testing")
@pytest.mark.parametrize("full", [True, False])
def test_build_distribution(tmp_path, full):
    output_path = tmp_path / "dist"
    build_distribution(
        build_path=tmp_path / "builddir",
        output_path=output_path,
        full=full,
        overwrite=True,
    ) #         examples_repo_path=_examples_repo_path,

    if full:
        # todo
        pass
    else:
        # check binaries and libs
        system = platform.system()
        ext = ".exe" if system == "Windows" else ""
        for exe in ["mf6"]:
            assert (output_path / f"{exe}{ext}").is_file()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="Create a Modflow USG-Transport distribution directory for release",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent(
            """\
            Create a distribution folder. If no output path is provided
            distribution files are written to the distribution/ folder.
            By default a minimal distribution containing only binaries
            and metadata (code.json) is created. 
            """
        ),
    )
    parser.add_argument(
        "--build-path",
        required=False,
        default=str(_build_path),
        help="Path to the build workspace",
    )
    parser.add_argument(
        "-o",
        "--output-path",
        required=False,
        default=str(_project_root_path / "distribution"),
        help="Path to create distribution artifacts",
    )
    """    parser.add_argument(
        "-e",
        "--examples-repo-path",
        required=False,
        default=str(_examples_repo_path),
        help="Path to directory containing usg-transport example models",
    )"""
    parser.add_argument(
        "--full",
        required=False,
        default=False,
        action="store_true",
        help="Build a full rather than minimal distribution",
    )
    parser.add_argument(
        "-f",
        "--force",
        required=False,
        default=False,
        action="store_true",
        help="Recreate and overwrite existing artifacts",
    )
    args = parser.parse_args()
    build_path = Path(args.build_path)
    out_path = Path(args.output_path)
    """    examples_repo_path = (
        Path(args.examples_repo_path)
        if args.examples_repo_path
        else _examples_repo_path
    )
    assert (
        examples_repo_path.is_dir()
    ), f"Examples repo not found at path: {examples_repo_path}"""
    out_path.mkdir(parents=True, exist_ok=True)

    build_distribution(
        build_path=build_path,
        output_path=out_path,
        full=args.full,
        overwrite=args.force,
    ) # examples_repo_path=examples_repo_path,
