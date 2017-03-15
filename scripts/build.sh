#!/bin/bash

stack --nix build --fast --ghc-options="-j +RTS -A128m -n2m -RTS"

