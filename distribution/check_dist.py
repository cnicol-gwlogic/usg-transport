import platform
import subprocess
from os import environ
from pathlib import Path
from pprint import pprint

import pytest


# OS-specific extensions
_system = platform.system()
_eext = ".exe" if _system == "Windows" else ""
_soext = ".dll" if _system == "Windows" else ".so" if _system == "Linux" else ".dylib"
_scext = ".bat" if _system == "Windows" else ".sh"

# fortran compiler
_fc = environ.get("FC", None)

# directories included in full distribution
_included_dir_paths = {
    "full": [
        "bin",
        "src",
        "make",
    ],
    "minimal": [
        "bin",
    ],
}


@pytest.fixture
def approved(request):
    return request.config.getoption("--approved")


@pytest.fixture
def releasemode(request):
    return request.config.getoption("--releasemode")


@pytest.fixture
def full(request):
    return request.config.getoption("--full")


@pytest.fixture
def dist_dir_path(request):
    def skip():
        pytest.skip(f"no distribution directory found at {path}")

    path = request.config.getoption("--path")
    if not path:
        skip()

    path = Path(path).expanduser().absolute()
    if not path.is_dir():
        skip()

    return path


def test_directories(dist_dir_path, full):
    for dir_path in _included_dir_paths["full" if full else "minimal"]:
        assert (dist_dir_path / dir_path).is_dir()


def test_sources(dist_dir_path, releasemode, full):
    if not full:
        pytest.skip(reason="sources not included in minimal distribution")

    assert (dist_dir_path / "src").is_dir()
    assert (dist_dir_path / "src" / "mfusg.f").is_file()


@pytest.mark.skipif(not _fc, reason="needs Fortran compiler")
def test_makefiles(dist_dir_path, full):
    if not full:
        pytest.skip(reason="makefiles not included in minimal distribution")

    assert (dist_dir_path / "make" / "makefile").is_file()
    assert (dist_dir_path / "make" / "makedefaults").is_file()

    # makefiles can't be used on Windows with ifort compiler
    if _system != "Windows" or _fc != "ifort":
        print(subprocess.check_output("make", cwd=dist_dir_path / "make", shell=True))
        

"""
def test_examples(dist_dir_path, full):
    if not full:
        pytest.skip(reason="examples not included in minimal distribution")

    examples_path = dist_dir_path / "examples"
    assert examples_path.is_dir()
    assert (examples_path / f"runall{_scext}").is_file()
    example_paths = [
        p for p in examples_path.glob("*") if p.is_dir() and p.stem.startswith("ex")
    ]
    print(f"{len(example_paths)} example models found:")
    pprint(example_paths)
    for p in example_paths:
        script_path = p / f"run{_scext}"
        if not script_path.is_file():
            continue
        pprint(subprocess.check_output([str(script_path)], cwd=p).decode().split())
        break
"""


def test_binaries(dist_dir_path, approved):
    bin_path = dist_dir_path / "bin"
    assert (bin_path / f"usg-transport{_eext}").is_file()

