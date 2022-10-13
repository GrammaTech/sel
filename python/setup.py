"""A library for programmatic software modification

See: https://grammatech.github.io/sel
"""
from importlib.util import module_from_spec, spec_from_file_location
from setuptools import find_packages, setup
from os import path

this_directory = path.abspath(path.dirname(__file__))
spec = spec_from_file_location(
    "pkginfo.version",
    path.join(this_directory, "asts/version.py"),
)
assert spec
assert spec.loader

pkginfo = module_from_spec(spec)
spec.loader.exec_module(pkginfo)

setup_requirements = ["wheel"]

with open(path.join(this_directory, "requirements.txt")) as fd:
    requirements = fd.readlines()

with open(path.join(this_directory, "requirements-dev.txt")) as fd:
    development_requirements = fd.readlines()

with open(path.join(this_directory, "README.md"), encoding="utf-8") as f:
    long_description = f.read()

setup(
    name=pkginfo.__packagename__,
    version=pkginfo.__version__,
    description=pkginfo.__description__,
    classifiers=pkginfo.__classifiers__,
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/grammatech/sel",
    author="Eric Schulte and GrammaTech",
    license="GPLv3+",
    keywords="software-engineering, source, program-synthesis",
    packages=find_packages(exclude=["test"]),
    package_data={"asts": ["lib*", "tree-sitter*"]},
    setup_requires=setup_requirements,
    install_requires=requirements,
    extras_require={"dev": development_requirements},
    python_requires=f">={pkginfo.__pythonbaseversion__}",
    project_urls={
        "Bug Reports": "https://github.com/grammatech/sel/issues",
        "Source": "https://github.com/grammatech/sel/",
    },
)
