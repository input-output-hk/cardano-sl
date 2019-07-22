import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="cardano-explorer-python-api",
    version="3.0.3",
    author="John Lotoski",
    author_email="john.lotoski@iohk.io",
    description="Explorer backend wrapper that dumps to PostgreSQL",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/input-output-hk/cardano-sl",
    packages=setuptools.find_packages(),
    entry_points={
        "console_scripts": [
            "explorer-python-api = explorer_python_api.app:main",
        ]
    },
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: Apache License",
        "Operating System :: OS Independent",
    ],
)
