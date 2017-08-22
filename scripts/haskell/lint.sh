#!/usr/bin/env bash
projects="core db lrc infra update ssc godtossing tools txp lwallet wallet"

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
  node/src node/test node/bench $projects
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
