#!/usr/bin/env bash
set -e

projects="core db lrc infra update"

incpath=$(find $(stack path --compiler-bin)/../lib -maxdepth 2 -path */include)

# Some people have tests and subprojects symlinked into src/, others don't
if [ -d "src/core" ]; then
  hlint -h HLint.hs -X TypeApplications --cpp-include=$incpath src
else
  hlint -h HLint.hs -X TypeApplications --cpp-include=$incpath src test bench $projects
fi
