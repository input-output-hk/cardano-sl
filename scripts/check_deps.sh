#!/usr/bin/env bash

# check and warn if `pkgs/default.nix` is out of date

set -xe

checksum_files=""

# Find the checksum of all stack files.
for stack_file in `find -name "stack.yaml" -not -path "./.stack-work/*" -type f`; do
    checksum_files+="$stack_file "
    echo $stack_file
done

# Find the checksum of all cabal files.
for cabal_file in `find -name "*.cabal" -not -path "./.stack-work/*" -type f`; do
    checksum_files+="$cabal_file "
    echo $checksum_files
done

total_checksum="$(md5sum $checksum_files)"
existing_checksum=`cat ./scripts/deps_checksum`

echo $total_checksum
#echo "The existing checksum is '$existing_checksum'"

# Ignore whitespaces
if [ "$(echo "$total_checksum" | tr -d '[:space:]')" != "$(echo "$existing_checksum" | tr -d '[:space:]')" ]; then
  echo "ERROR: you need to (run ./pkgs/generate.sh) and commit the changes."
fi
