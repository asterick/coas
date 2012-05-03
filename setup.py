from distutils.core import setup
from glob import glob


setup(
    name = "coas",
    version = "0.00.0dev",
    author = "asterick",
    py_modules = ["coas"],
    scripts = glob("scripts/*"),
)
