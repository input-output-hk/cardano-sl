#!/bin/bash

## needs 'gnused' from Nix on MacOSX

if [ $# -ne 1 ]; then
  echo "$0 <log file>"
  exit 1
fi

# BASE BLOCK -> NEW BLOCK
sed -ne '/^MainBlockHeader:/{
          n
          N
          s/hash: \([0-9a-f]\+\).*previous block: \([0-9a-f]\+\)/("\2", "\1")/p
         }'  $1
