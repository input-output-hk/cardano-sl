#!/bin/sh

# dev-mode : allow to generate custom genesis block
# CONFIG=.. selects section from config file (core/constants.yaml)

stack build --flag cardano-sl-core:dev-mode --ghc-options=-DCONFIG=bench --flag cardano-sl-core:-asserts cardano-sl cardano-sl-auxx cardano-sl-wallet cardano-sl-explorer cardano-sl-tools:cardano-post-mortem
