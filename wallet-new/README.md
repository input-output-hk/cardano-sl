# Cardano Wallet API

The Cardano Wallet API is the wallet backend for a Cardano node.

## Installation

The installation procedure follows the standard approach to installing Stack-based projects:

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install cardano-sl-wallet-new` from the project *root* to install this package.

## About the Wallet API

We describe how to interact with our API via the popular [Swagger](https://swagger.io/) framework and format.
In order to do so, we export the full Swagger specification inside `spec/swagger.json`. Such JSON files must
be kept in sync with the current version of the project and it is the responsibility of the developer to do so upon
committing new work. This will be automated as part of [this issue](https://iohk.myjetbrains.com/youtrack/issue/CSL-1939) but
in the interim, developer discipline is required.

Currently the only way to generate an updated `swagger.json` is to run the `wallet-new-server` node, so that
the updated Swagger file will be written to disk. For example:

```
stack exec wallet-new-server -- --topology=wallet-new/topology-examples/testnet.yaml \
  --configuration-key mainnet_staging_short_epoch_full --wallet-debug --rebuild-db
```

Running the above command *from the root of the Cardano project* will store an updated `swagger.json` into
`wallet-new/spec`.

### Rendering the API

Once you have your updated `swagger.json` file, you should open the editor [the online editor](https://editor.swagger.io), and
click "Edit -> Import File" and import the file. Once the file has been imported, the full functionality of the API is rendered.

Alternatively (and *recommended*), is also possible to download the editor locally and use it. This has the advantage that
you can now also *try out* the API, because the `host` of the swagger API points to `localhost`, which will not work, of course,
in case of the online editor. Setting up the editor locally is a relatively simple process, as follows:

- Download [the editor](https://github.com/swagger-api/swagger-editor/archive/v3.1.17.zip) online;
- Download the [http-server](https://www.npmjs.com/package/http-server) npm package and install this package;
- Serve the editor with the following values `http-server swagger-editor-folder` where `swagger-editor-folder` is the folder where
  you opened or decompressed the downloaded editor.

## Testing the API

Tests can be performed by running `stack test cardano-sl-wallet-new` from the project *root* directory.
