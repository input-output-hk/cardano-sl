#!/usr/bin/env bash

# check and warn if `pkgs/default.nix` is out of date

#set -xe

checksum_files=()

# Find the checksum of all stack files.
while IFS= read -r -d '' stack_file
do
  checksum_files+=("$stack_file")
 done < <(find . -name "stack.yaml" -not -path "./.stack-work/*" -type f -print0)

# Find the checksum of all cabal files.
while IFS= read -r -d '' cabal_file
do
  checksum_files+=("$cabal_file")
done < <(find . -name "*.cabal" -not -path "./.stack-work/*" -type f -print0)

total_checksum="$(md5sum "${checksum_files[@]}")"
existing_checksum=$(cat ./scripts/deps_checksum)

echo "$total_checksum"
#echo "The existing checksum is '$existing_checksum'"

# Ignore whitespaces
if [ "$(echo "$total_checksum" | tr -d '[:space:]')" != "$(echo "$existing_checksum" | tr -d '[:space:]')" ]; then
  echo "ERROR: you need to (run ./pkgs/generate.sh) and commit the changes."
  echo "$total_checksum" > ./scripts/deps_checksum
fi
