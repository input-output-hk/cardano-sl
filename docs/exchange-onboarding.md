Table of Contents
=================

   * [Communication](#communication)
   * [Requirements](#requirements)
      * [Nix](#nix)
         * [Optional: Enable IOHK's binary cache](#optional-enable-iohks-binary-cache)
      * [Miscellaneous Utilities](#miscellaneous-utilities)
   * [Wallet](#wallet)
      * [Backup local state](#backup-local-state)
      * [Fetch latest code](#fetch-latest-code)
      * [Generate custom configuration](#generate-custom-configuration)
      * [Building and running](#building-and-running)
         * [Method 1: Build and run in the nix store](#method-1-build-and-run-in-the-nix-store)
         * [Method 2: Build and run a docker image](#method-2-build-and-run-a-docker-image)
   * [Migrating from V0 to V1 API](#migrating-from-v0-to-v1-api)
      * [Checking Sync Status](#checking-sync-status)
      * [Wallets](#wallets)
      * [Accounts](#accounts)
      * [Transactions](#transactions)
      * [Transaction Fees](#transaction-fees)
      * [Wallet Addresses](#wallet-addresses)
   * [Usage FAQs](#usage-faqs)
      * [What are recommended hardware/software requirements for exchange wallets?](#what-are-recommended-hardwaresoftware-requirements-for-exchange-wallets)
      * [How do I export the CA certificate for the API?](#how-do-i-export-the-ca-certificate-for-the-api)
      * [How do I know when the wallet has fetched all the blocks?](#how-do-i-know-when-the-wallet-has-fetched-all-the-blocks)
      * [Where can I find the API documentation?](#where-can-i-find-the-api-documentation)
      * [How can I inspect runtime metrics and statistics?](#how-can-i-inspect-runtime-metrics-and-statistics)

# Communication

* Exchanges must provide an email address, so IOHK can broadcast issue and update announcements.
* IOHK will create a guest Slack room to support the exchange.
* Exchanges must provide IOHK with a status page for their service/wallets.

# Requirements

## Nix

The wallet is built using [nix package manager](https://nixos.org/nix/). To install it on
most Linux distros download and run the installation script.

    curl https://nixos.org/nix/install > install-nix.sh
    . install-nix.sh

Follow the directions and then log out and back in.

### Optional: Enable IOHK's binary cache

Skip this section if you prefer to build all code from IOHK
locally. When the binary cache is enabled build steps will tend
go faster.

    sudo mkdir -p /etc/nix
    cat <<EOF | sudo tee /etc/nix/nix.conf
    binary-caches            = https://cache.nixos.org https://hydra.iohk.io
    binary-cache-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
    EOF

## Miscellaneous Utilities

Use `nix` to install essential utilities.

    nix-env -iA nixpkgs.git

# Wallet

## Backup local state

Skip to the next section if this is target machine doesn't yet have
`cardano-sl` set up.

To avoid catastrophic data loss, stop the wallet and backup the
local databases, keys, certificates, and logs. By default, the
local state will be in `./state-wallet-mainnet`, but may be
elsewhere (see `stateDir` attribute in `./custom-wallet-config.nix`).

## Fetch latest code

Clone the [cardano-sl repository](https://github.com/input-output-hk/cardano-sl) or `cd` into a preexisting copy.

    git clone https://github.com/input-output-hk/cardano-sl.git
    cd cardano-sl

Switch to the `master` branch and pull the latest code.

    git checkout master
    git pull

Dump the current revision and confirm with IOHK whether it is as
expected.

    git rev-parse HEAD

## Generate custom configuration

The `cardano-sl` repo defaults to using end user topology and settings.

Before building the wallet copy `./sample-wallet-config.nix` to
`./custom-wallet-config.nix` and edit as needed.

Supported options include:

-   **`walletListen`:** Wallet API server
-   **`walletDocListen`:** Wallet doc API server
-   **`ekgListen`:** Runtime metrics server
-   **`stateDir`:** Directory for the wallet's local state. Must be
    enclosed in double quotes.
-   **`topologyFile`:** Used to connect to a custom set of nodes on
    the network. When unspecified an appropriate
    default topology is generated.

For exchanges we recommend creating the following `custom-wallet-config.nix`:

    # If any customization is required, copy this file to
    # ./custom-wallet-config.nix and make edits there.
    {
      ## Wallet API server.
      #walletListen = "127.0.0.1:8090";

      ## Wallet doc API server.
      #walletDocListen = "127.0.0.1:8091";
    
      ## Runtime metrics server.
      #ekgListen = "127.0.0.1:8000";
    
      ## Directory for the wallet's local state. Must be set BEFORE
      ## running nix-build to have any effect, and it must be enclosed in
      ## double quotes.
      stateDir = "./state-wallet-mainnet";
      topologyFile = ./exchange-topology.yaml;
    
      ## See https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/runtime_control.html#running-a-compiled-program
      #ghcRuntimeArgs = "-N2 -qg -A1m -I0 -T";
    
      ## Primarily used for troubleshooting.
      #additionalNodeArgs = "";
    }


The rest of this document will assume the above configuration file. Please
alter any commands for example related to `stateDir` path to reflect your
setup.

You will also need to add the following `exchange-topology.yaml` file to use the
private relays:

    TODO: private relays should be inserted here in document given to exchanges

## Building and running

We support two different ways to build and run the wallet. You can run
it standalone on any linux system built in the nix store or you can generate
a docker container that can be ran on any docker container orchestrator like
docker swarm or kubernetes.

### Method 1: Build and run in the nix store

By default the wallet's local state goes in
`./state-wallet-mainnet`.

Build the wallet and generate the shell script to connect to
mainnet (use `connectScripts.stagingWallet` for testnet)

    nix-build -A connectScripts.mainnetWallet -o "./launch_$(date -I)_$(git rev-parse --short HEAD)"

After the build finishes the generated connection script is
available as a symlink called `./launch_2018-01-30_0d4f79eea`, or
similar. Run that symlink as a script to start the wallet.

### Method 2: Build and run a docker image

Follow the above instructions for customization and dependencies. To build a docker
container and import the image run
(use `connectScripts.stagingWallet` for testnet):

    docker load < $(nix-build --no-out-link -A dockerImages.mainnetWallet)

This will create an image `cardano-container-mainnet:latest`
(or `cardano-container-staging:latest` for testnet)

After this image is built, it can be used like any other docker image being pushed
into a registry and pulled down using your preferred docker orchestration tool.

The image can be ran using the following:

    docker run --name cardano-mainnet-wallet --rm -it -p 127.0.0.1:8090:8090 -p 127.0.0.1:8000:8000 -v state-wallet-mainnet:/wallet cardano-container-mainnet:latest

The above command will create a docker volume named `state-wallet-mainnet` and will mount
that to /wallet. Note: if no volume is mounted to `/wallet` the container startup
script will refuse to execute `cardano-node` and the container will exit.

The location of `/wallet` cannot be changed, but you can mount any kind of volume
you want in that directory that docker supports.

Note that if you give this a different name than is specified above, use the
name you used for any future docker commands in examples in the document.

# Migrating from V0 to V1 API

## Checking Sync Status

With v0, to check the sync status required comparing number of blocks on the network and locally.
With v1, the sync status `/api/v1/node-info` outputs `data.syncProgress.quantity`
attribute that shows the current sync progress.

## Wallets

With v0, there were two separate POST API calls, `/api/wallets/new` and `/api/wallets/restore`.
With v1, these have been combined into a POST to `/api/v1/wallets` that passes an
attribute `operation` that can be `create` or `restore`.

With v0, list wallets was done using a GET to `/api/wallets`. This is now a GET to
`/api/v1/wallets` and supports filtering by `id` and `balance` as well as specifying
`sort_by`.

With v0, actions against a specific wallet were done using a GET, PUT or DELETE to
`/api/wallets/{walletId}` This is now `/api/v1/wallets/{walletId}`. The parameters
in the body and responses have changed slightly, so check the API docs, but the
general usage is the same as before.

With v0, the endpoint to update a wallet password was a POST to
`/api/wallets/password/{walletId}`. With v1 this is now `/api/v1/wallets/{walletId}/password`.

For more details see the [API documentation](#where-can-i-find-the-api-documentation).

## Accounts

With v0, accounts didn't include the `walletId` in it's API queries. With v1,
accounts in the API are logically underneath a wallet. The calls in v0 were:

* GET, PUT, DELETE `/api/accounts/{accountId}`
* GET, POST `/api/accounts`

In v1 these are at:

* GET, PUT, DELETE `/api/v1/wallets/{walletId}/accounts/{accountId}`
* GET, POST `/api/v1/wallets/{walletId}/accounts`

For more details see the [API documentation](#where-can-i-find-the-api-documentation).

## Transactions

How to send and receive ADA has changed significantly with the new API. In v0 there
were 2 separate API calls to send money one with parameters, and a batch version:

* `/api/txs/payments/{from}/{to}/{amount}`
* `/api/txs/payments/batch`

In v1 this has been significantly simplified:

* POST to `/api/v1/transactions`

This POST takes an array of `destinations` where each one contains a `address` and
`amount` and a single `source`.

To view transactions the `/api/txs/histories` has been replaced with a GET to the
same endpoint used to send: `/api/v1/transactions`.

For more details see the [API documentation](#where-can-i-find-the-api-documentation).

## Transaction Fees

`/api/txs/fee/{from}/{to}/{amount}` has been replaced with `/api/v1/transactions/fees`
Similar to sending money, this takes an array of `destinations` and a single `source`
making it much easier to estimate transaction fees for transactions with multiple
`destinations`.

For more details see the [API documentation](#where-can-i-find-the-api-documentation).

## Wallet Addresses

With v0, addresses could be created using a POST to `/api/addresses` or could be
validated using a GET to `/api/addresses/{address}`. With v1 a new GET to
`/api/v1/addresses` has been introduced that gets all addresses. A new address can
be created using a POST to the same URL.

Also a GET to `/api/v1/addresses/{address}` returns detailed information about the
address instead of just a boolean `true` or `false`.

For more details see the [API documentation](#where-can-i-find-the-api-documentation).

# Usage FAQs

## What are recommended hardware/software requirements for exchange wallets?

RAM: 8 GB for building, 4 GB for running

CPU: Modern x86_64 processor

Disk: SSD recommended of 100 GB size with option to allocate more in future
years.

Operating System: NixOS or CentOS 7 recommended, although any linux
distribution should work with nix package manager used for building
the binaries

Software Requirements: Nix package manager for building standalone
and docker containers. On systems using docker, docker > 17.12 required.

## How do I export the CA certificate for the API?

The certificate is generated inside the wallet in the file `tls/server/server.crt`

If you are using the docker container, this can be output using the command:

`docker exec -it cardano-mainnet-wallet cat /wallet/state-wallet-mainnet/tls/server/server.crt`

Please refer to your OS or browser documentation for how to import the CA
certificate into your trusted `ca-certificates` file. The rest of this
document will assume the certificate is trusted.

## How do I know when the wallet has fetched all the blocks?

You can check the sync progress using the API to get detailed json output:

    curl -X GET "https://127.0.0.1:8090/api/v1/node-info" -H "accept: application/json;charset=utf-8"
    {"data":{"syncProgress":{"quantity":100,"unit":"percent"},"blockchainHeight":{"quantity":738268,"unit":"blocks"},"localBlockchainHeight":{"quantity":738268,"unit":"blocks"},"localTimeDifference":{"quantity":0,"unit":"microseconds"}},"status":"success","meta":{"pagination":{"totalPages":1,"page":1,"perPage":1,"totalEntries":1}}}

The following command can be used to see the percentage completion of the sync only:

    nix-shell -p jq curl --run 'curl -X GET "https://127.0.0.1:8090/api/v1/node-info" -H "accept: application/json;charset=utf-8" | jq .data.syncProgress.quantity'
    100

## Where can I find the API documentation?

Run the latest wallet and go to <https://127.0.0.1:8090/docs/v1/index>.

For the v0 API documentation (deprecated), go to <https://127.0.0.1:8090/docs/v0/index>.

The domain and port can be customized with the `walletListen` attribute in `./custom-wallet-configuration.nix`.

Examples of tasks that can be done via the API that an exchange would need to do:

* Create/Restore a wallet
* Make a single transaction
* Make a batch transaction
* Estimate transaction fees
* Generate new addresses to receive funds
* Get balances of a wallet
* Get transaction history for a specific wallet
* Get transaction history for a specific address

## How can I inspect runtime metrics and statistics?

Current metrics and stats are available at <http://127.0.0.1:8000/>.

The domain and port can be customized with the `ekgListen` attribute in `./custom-wallet-configuration.nix`.
