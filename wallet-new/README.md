# Cardano Wallet API

This is the Wallet Backend for a Cardano node.

## Installation

Installation follows the standard approach to installing Stack-based projects.

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install cardano-sl-wallet-new` from the project *root* to install this package.

## The API

We describe how to interact with our API via the popular [Swagger](https://swagger.io/) framework & format.
In order to do so, we export the full Swagger specification inside `spec/swagger.json`. Such JSON file must
be kept in sync with the current version of the project and is developer responsibility to do so upon
committing new work.
This will be made automatic as part of [this issue](https://iohk.myjetbrains.com/youtrack/issue/CSL-1939) but
for now requires self-enforced discipline.

Currently the only way to generate an updated `swagger.json` is to run the `wallet-new-server` node, so that
the updated Swagger file will be written on disk. For example:

```
stack exec wallet-new-server -- --topology=wallet-new/topology-examples/testnet.yaml \
  --configuration-key mainnet_staging_short_epoch_full --wallet-debug --rebuild-db
```

Running the command above *from the root of the Cardano project* will store an updated `swagger.json` into
`wallet-new/spec`.

### Playing with the API

Once you have your updated `swagger.json` file, the easiest way is to head over to [the online editor](https://editor.swagger.io),
click "Edit -> Import File" and import the file. Once done that, the API will be rendered in its full glory.

Alternatively (and *recommended*), is also possible to download the editor locally and play with it. This has the advantage
you can now also *try out* the API, because the `host` of the swagger API points to `localhost`, which won't work, of course,
in case of the online editor. We won't get too deep into how to setup the editor locally, but generally speaking it should be
as simple as:

- Downloading [the editor](https://github.com/swagger-api/swagger-editor/archive/v3.1.17.zip) online;
- Download the [http-server](https://www.npmjs.com/package/http-server) npm package and install it;
- Serve the editor with something like `http-server swagger-editor-folder` where `swagger-editor-folder` is the folder where
  you opened/decompressed the downloaded editor.

## Testing

Tests can be run by running `stack test cardano-sl-wallet-new` from the project *root* directory.
