from distutils.core import setup
from glob import glob


setup(
    name = "coas",
    version = "0.00.0dev",
    author = "asterick",
    modules = ["coas.py"],
    scripts = glob("scripts/*"),
)
