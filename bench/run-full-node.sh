#!/bin/sh

i=$1

# Run full node
stack --nix bench pos:pos-bench-remote --benchmark-arguments="full -n $i -c bench/config/fullnode.$i.yaml"
