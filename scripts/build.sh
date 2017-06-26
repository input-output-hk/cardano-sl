#!/usr/bin/env bash
stack --nix install happy --fast --ghc-options="-j +RTS -A128m -n2m -RTS"
stack --nix build --fast --ghc-options="-j +RTS -A128m -n2m -RTS"
