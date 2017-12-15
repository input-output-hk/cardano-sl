#!/usr/bin/env bash

# Networking is not in here, because it has a very different codestyle (doesn't use universum).
# This is bad and should probably be fixed.
projects="util binary crypto core db lrc infra ssc txp update delegation node tools client generator auxx explorer wallet wallet-new"

# incpath can be used to specify custom include paths, e. g.:
# incpath=$(find $(stack path $@ --compiler-bin)/../lib -maxdepth 2 -path */include)
incpath=""

# Some people have tests and subprojects symlinked into src/, others don't
if [ -d "src/core" ]; then
  hlint -h HLint.hs \
  -X TypeApplications \
  --cpp-include=$incpath --cpp-define=CONFIG=dev \
  src
else
  hlint -h HLint.hs \
  -X TypeApplications \
  --cpp-include=$incpath --cpp-define=CONFIG=dev \
  lib/src lib/test lib/bench $projects
fi

ex=$?

if [ $ex != 0 ]; then
  echo ''
  echo '====================================================================='
  echo ''
  echo 'Note: to ignore a particular hint (e.g. "Reduce duplication"), write'
  echo 'this in the source file:'
  echo ''
  tput bold
  echo '{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}'
  tput sgr0
  echo ''
  echo 'You can also apply it just to a particular function, which is better:'
  echo ''
  tput bold
  echo '{-# ANN funcName ("HLint: ignore Reduce duplication" :: Text) #-}'
  tput sgr0
  echo ''

  exit $ex
fi
