from setuptools import setup, find_packages

setup(name='validate-json',
      version = '1.0.0',
      author = 'IOHK',
      license = 'MIT',
      packages = find_packages(),
      install_requires = [
        'docopt==0.6.2',
        'jsonschema==2.6.0'
      ],
      entry_points = """\
        [console_scripts]
        validate-json = validate_json:main
      """
     )
