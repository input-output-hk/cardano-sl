# validate-json

> Validate JSON file against a json-schema. For example, to validate a Swagger 2.0 generated schema.

# How to Use

```
$ ./validate-json.py --help

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
```

# License

MIT - Copyright (c) 2018 IOHK
