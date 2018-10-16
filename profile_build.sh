echo "compiling.."

stack build cardano-sl-wallet-new --ghc-options=-optl-Wl,-dead_strip_dylibs \
    --executable-profiling --library-profiling --no-executable-stripping \
    --ghc-options=-w
