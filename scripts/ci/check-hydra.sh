#!/usr/bin/env bash

eval "$(nix-build -A check-hydra lib.nix --no-out-link)"
