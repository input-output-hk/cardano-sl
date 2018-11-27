## Easy client functions and CLI for Cardano Wallet API

This provides more util functions for setting up a `WalletClient` so
that you can quickly make API calls from `ghci`.

There is also a CLI for use in test scripts.

### How to use

Start a wallet using the connect scripts:

    nix-build -A connectScripts.testnet.wallet -o launch-testnet.sh
    ./launch-testnet.sh

In another terminal, fire up `ghci`.

    nix-shell --run "cabal new-repl cardano-sl-wallet-tool"
    λ> import Cardano.Wallet.Client.Easy
    λ> cfg = normalConnectConfig "../state-wallet-testnet" 8090
    λ> wc <- walletClientFromConfig cfg
    λ> getWallets wc

### How to use CLI

For a list of options, run:

    cardano-sl-wallet-tool --help
