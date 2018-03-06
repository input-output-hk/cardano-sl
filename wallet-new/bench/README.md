# Wallet Web API Benchmarking Tool

This is a tool for benchmarking of Wallet Web API (currently - API v0).

## How Do We Measure

We treat Wallet Web API as a blackbox: we're sending request for particular endpoint,
waiting for a response and measure the time. Technically, this time period includes
following actions:

1. Prepare arguments for request as a Haskell-values.
2. Establish TLS-connection with wallet backend.
3. Send serialized request.
4. Get the response from wallet backend.
5. Deserialize it into Haskell-values.
6. Analyze it (if required).

We use [gauge](https://hackage.haskell.org/package/gauge) package as a benchmark tool.

## Benchmarking Tool Structure

1. Module `Bench.Cardano.Wallet.Run` contains a function we use to send request and measure it as a complete IO-action.
2. Module `Bench.Cardano.Wallet.Random` contains functions we use to pick (pseudo)random values from the config (see below).
3. Modules `Bench.Cardano.Wallet.Config` contain functions we use to work with command-line arguments and two configuration files.
4. Module `Client.Cardano.Wallet.Web.Run` contains a function we use for actual sending of request (via `servant-client`).
5. Module `Client.Cardano.Wallet.Web.Api` contains Servant-based API which includes all endpoints we can benchmark.
6. Modules `Client.Cardano.Wallet.Web.Endpoint` contain functions we use to run benchmarking for particular endpoints.

## Configuration Files

Directory `config` contains two configuration files we need for benchmarking:

1. `Endpoints.csv`,
2. `Wallets.yaml`.

### Endpoints Configuration File

Example:

```
BenchName,NumberOfMeasures,MinDelayBetweenCalls,MaxDelayBetweenCalls,PathToReportFile,PathToResponseReportsFile
NewWalletBench,10,0.0,0.0,/home/denis/Code/cardano-sl/wallet/bench/results/NewWalletBenchReport.csv,/home/denis/Code/cardano-sl/wallet/bench/results/NewWalletResponseReports.txt
```

This file contains settings we use to benchmark particular endpoint (one row for one endpoint). We can choose which
endpoint will be benchmark by adding it in (or by removing it from) this file. Thus, in this example only `NewWallet`
endpoint will be benchmarked.

Supported `BenchName`s are defined [here](https://github.com/input-output-hk/cardano-sl/blob/feature/cbr23-wallet-bench/wallet/bench/Bench/Pos/Wallet/Types.hs).

`NumberOfMeasures` corresponds to number of iterations `gauge` makes to measure time. For example, if we set 10 measurements,
it means that actually ~56 iterations will be performed, if 20 measurements - ~211 iterations, if 30 measurements - ~460 iterations.

`MinDelayBetweenCalls` and `MaxDelayBetweenCalls` are the values (in seconds) for (pseudo)random delay between calls.
For example, if we want to send `NewWallet` request once in ~2 seconds, we can define `MinDelayBetweenCalls` as `1.8`,
and `MaxDelayBetweenCalls` as `2.1`. In this case actual delay will be picked randomly from this range, for example, `1.91`.

`PathToReportFile` is a path to the file with benchmark report. It is recommended to use absolute path.

`PathToResponseReportsFile` is a path to the file with responses from the wallet backend. It is recommended
to use absolute path.

### Wallets Configuration File

Example:

```
---
Wallets:
  -
    Accounts:
      -
        Addresses:
          - DdzFFzCqrhsxJU8JeDDS7T7rjw4chfMR3p98y89zTNE4gPeyveRccWhJknr5eoeQAqrRjAXNPH5L7q39fFpA8SoETRD8bZn6RSDm2BR4
          - DdzFFzCqrhspW7t61NskUMMgVfA4Ru8CwpnpkPumrXiMn6TanduU2tZnc2mE46bsy1sPLScxnqh2KQyYqCHhueY5SVnLsbS1STTYJ5vX
          - DdzFFzCqrhsvjq72kpopPyxQzZZDPfnYegbUeQeqX1CHec9dheFK4hTHi331RKE6MDH9FLgNjEjRS8gwyCGjLi9W7zpYmGYzAVnSLMd4
          - DdzFFzCqrht6p7oRHgCaXxKsmZwvGN8syWqcyKRpA99AS134N7WgviTSACA4wXdbKpw78DpZcpyyshmvJvt2VBPeBXFGzWzyPykufq5c
          - DdzFFzCqrhsjRHnqeMzuyUJVWhv2ebpu1vupamkvfpqizShoRxVBm8FqWff674p115fZUHLFZ9jrBA2edhPnv5KQedQDEBK99NVQdE4t
        AccountId: Ae2tdPwUPEZEK5DvxPMtnTnUfQg8coWAAbNfLEvQ4GqWTe9h8d6AEkBDMce@2147483648
    WalletId: Ae2tdPwUPEZEK5DvxPMtnTnUfQg8coWAAbNfLEvQ4GqWTe9h8d6AEkBDMce
```

This file contains wallets, accounts and addresses we will use during benchmarking.

**IMPORTANT**: It is assumed that wallets/accounts/addresses defined in this file are actually exist in the wallet we
benchmark.

Please note that each wallet contains at least one account, and each account contains at least one address.

## Command Line Options

Please run:

```
$ stack bench cardano-sl-wallet-new --benchmark-arguments "--help"
```

to see all supported options.

## Run Benchmarking

Example of the full command:

```
$ stack bench cardano-sl-wallet-new --benchmark-arguments "--tls-pub-cert=$PWD/scripts/tls-files/ca.crt \
                                                           --tls-priv-key=$PWD/scripts/tls-files/server.key \
                                                           --wal-conf=$PWD/wallet/bench/config/Wallets.yaml \
                                                           --ep-conf=$PWD/wallet/bench/config/Endpoints.csv \
                                                           --analyze \
                                                           --async"
```
