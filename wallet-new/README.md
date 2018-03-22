# Cardano Wallet API

This is the Wallet Backend for a Cardano node.

## Installation

Installation follows the standard approach to installing Stack-based projects.

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install cardano-sl-wallet-new` from the project *root* to install this package.

## API

We describe how to interact with our API via the popular [Swagger](https://swagger.io/)
framework & format. Swagger relies on a single specifications file in a `json` format. From 
this file is derived a handful of tools. 

A cardano node exposes both a Swagger file corresponding to the wallet API and a visualization
tool that can be used for browsing the specification as well as playing with the API. As a
first step, start a `cardano-node` using the following command line:

```
stack exec cardano-node -- --topology=wallet-new/topology-examples/testnet.yaml --configuration-key mainnet_staging_short_epoch_full --wallet-debug --rebuild-db
```

From there, you can browse the documentation for V0 & V1 through the following URLs:

- http://localhost:8090/docs/v0/index/
- http://localhost:8090/docs/v1/index/

The visualization at those URLs lets you play with the API by the mean of a _Try it out_ button
made available for each endpoint. This will seemingly contact the node already running on your
local machine with actual HTTP requests augmented with the parameters you provide!

If needed, you can access the corresponding raw Swagger files via these URLs:

- http://localhost:8090/docs/v0/swagger.json
- http://localhost:8090/docs/v1/swagger.json

> **NOTE** If you run the wallet in debug mode (`--wallet-debug`), you'll have access to an
> extra set of endpoints, documented under: http://localhost:8090/docs/development/index

## Testing

Tests can be run by running `stack test cardano-sl-wallet-new` from the project *root* directory.
