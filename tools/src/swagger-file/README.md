# swagger-file

> Output Swagger spec (2.0) file corresponding to a given API


# How to Use

```
$ cardano-swagger-file --help

Usage: cardano-swagger-file (-t|--target API) [-o|--output-file FILEPATH]

Available options:
  -t,--target API           Target API with version (e.g. 'wallet@v1', 'wallet@v0', 'wallet@dev'...)
  -o,--output-file FILEPATH Output file, default to: swagger.json
  -h,--help                 Show this help text

Examples:
  cardano-swagger-file --target wallet@v1
  cardano-swagger-file -t wallet@dev -o dev.json
```


# License

MIT - Copyright (c) 2018 IOHK
