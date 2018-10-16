echo "compiling.."

stack build cardano-sl-wallet-new --ghc-options=-optl-Wl,-dead_strip_dylibs \
    --executable-profiling --library-profiling --no-executable-stripping \
    --ghc-options=-w

rm -rf cardano-node.*
rm -rf ../staging-db-profiling
rm -rf wallet-db*
