#!/bin/sh

# Run supporters
nohup stack --nix bench pos:pos-bench-remote --benchmark-arguments="supporter -c bench/config/supporter.1.yaml" &
nohup stack --nix bench pos:pos-bench-remote --benchmark-arguments="supporter -c bench/config/supporter.2.yaml" &
