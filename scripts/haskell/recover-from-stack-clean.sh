#!/usr/bin/env bash

# Useful if you're on Atom with haskell-ghc-mod since
# installation of cabal-helper with stack is broken. See also:
# * https://github.com/DanielG/cabal-helper/issues/28
# * https://github.com/commercialhaskell/stack/issues/3202

stack build cabal-helper

# At this point, the cabal-helper-wrapper binary exists,
# but Stack fails to locate it, so we copy it manually into
# the bin/ directory. Example:
# WRAPPER_PATH=./.stack-work/install/x86_64-linux/lts-8.13/8.0.2/libexec/x86_64-linux-ghc-8.0.2/cabal-helper-0.7.3.0/cabal-helper-wrapper
# DEST_PATH=./.stack-work/install/x86_64-linux/lts-8.13/8.0.2/bin/

WRAPPER_PATH=$(find -name 'cabal-helper-wrapper' | head -n 1)
DEST_PATH=$(echo $WRAPPER_PATH | sed 's/libexec.*/bin\//')

cp $WRAPPER_PATH $DEST_PATH
stack install cabal-helper hlint
