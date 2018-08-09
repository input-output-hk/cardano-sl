#!/usr/bin/env python3

from docopt import docopt
from jsonschema import validate
from jsonschema.exceptions import ValidationError
from os import path
import json


__version__ = "1.0.0"

__doc__ = """validate-json

Validate a JSON file against a json-schema.

Usage:
  validate-json (-s | --schema SCHEMA) <FILENAME>
  validate-json -h | --help
  validate-json --version

Options:
  -s --schema   Meta json-schema to use for validation
  -h --help     Show this screen.
  --version     Show the software version.

Examples:
  validate-json --schema swagger-meta-v2.json v1.json
"""


def main():
  opts = docopt(__doc__, version=__version__)

  with open(opts["<FILENAME>"]) as f:
    schema = json.load(f)

  with open(opts["SCHEMA"]) as f:
    metaSchema = json.load(f)

  validatePretty(schema, metaSchema)


def validatePretty(schema, metaSchema):
  try:
    validate(schema, metaSchema)
  except ValidationError as e:
    print(e)
    exit(1)


if __name__ == '__main__':
  main()
