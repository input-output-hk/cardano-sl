#!/bin/sh

# dev-mode : allow to generate custom genesis block
# CONFIG=.. selects section from config file (core/constants.yaml)

stack build --flag cardano-sl-core:dev-mode --ghc-options=-DCONFIG=benchmark --flag cardano-sl-core:-asserts cardano-sl cardano-sl-lwallet cardano-sl-wallet cardano-sl-explorer
