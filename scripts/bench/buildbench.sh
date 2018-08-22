#!/bin/sh

stack build --flag cardano-sl-core:-asserts --ghc-options=-optl-Wl,-dead_strip_dylibs cardano-sl cardano-sl-auxx cardano-sl-wallet cardano-sl-explorer cardano-sl-tools
