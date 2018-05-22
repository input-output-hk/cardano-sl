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
                                                           --wal-conf=$PWD/wallet-new/bench/config/Wallets.yaml \
                                                           --ep-conf=$PWD/wallet-new/bench/config/Endpoints.csv \
                                                           --analyze \
                                                           --async"
```

# Complete Usage Example

## Clarifications

* Cardano node (or just "node") is a program which acts as an atom of the Cardano network, so we can think of Cardano
  network as a huge amount of nodes. Every node stores a local copy of the Cardano blockchain.
* Cardano node in the Mainnet is a real node, because it works with the real ADA. Of course, we don't need it during
  benchmarking, so we should run local "demo-cluster" of nodes. You can think of this cluster as a tiny Cardano network
  which contains 4 nodes by default and exists on the local computer only. So all wallets/accounts/addresses/transactions
  that will be created during benchmarking exist only on that computer.
* We don't need Daedalus for benchmarking, because we will work with the node directly, via its "wallet web API".
* There are two versions of "wallet web API": `v0` (deprecated) and `v1`. Currently this benchmarking tool tests `v0`
  only.
* To run benchmarking, node should contain at least one wallet. You can think of a wallet as a node's tool for operations
  with ADA.
* Every regular Cardano wallet contains at least one account, and each account contains at least one address (please see
  examples below).

## Tmux

We are using [Tmux](https://github.com/tmux/tmux/wiki) as an environment for the demo cluster of nodes. Please make sure
you have `tmux` command available in your `PATH`, and the version of `tmux` is 2.5 or higher. You can check it by:

```
$ tmux -V
```

If you don't have `tmux`, you can install it from your packages repository (or from `brew`/`macports` on macOS).

## Stable Branch

Please checkout to some stable `cardano-sl` branch, for example, `release/1.2.0`.

## Launch Demo Cluster

Launch `tmux` and run a script from the root of `cardano-sl` repository:

```
$ tmux
$ ./scripts/launch/demo-with-wallet-api.sh
```

You will see a new terminal window with 4 panels, each of them will contain a running node. This is demo cluster.
Now you can send requests to the "wallet web API".

## Check It Works

The one of the simplest requests is asking for a list of available wallets. Run this command from the root of
`cardano-sl` repository:

```
$ curl --cacert ./scripts/tls-files/ca.crt https://localhost:8090/api/wallets
```

You should see this response:

```
{"Right":[]}
```

It works, and you see an empty list of wallets. Now it's time to create new one.

## Create a New Empty Wallet

Run this command from the root of `cardano-sl` repository:

```
$ curl -X POST -H "Content-Type: application/json" -d '{"cwBackupPhrase": {"bpToList": ["squirrel","material","silly","twice","direct","slush","pistol","razor","become","junk","kingdom","flee"]}, "cwInitMeta":{"cwName": "Personal Empty Wallet 1", "cwAssurance": "CWANormal", "cwUnit": 0}}' --cacert ./scripts/tls-files/ca.crt https://localhost:8090/api/wallets/new
```

You will get this response:

```
{"Right":{"cwId":"Ae2tdPwUPEZMeiDfNHZ45V7RoaSqd4oSMuG4jo7asvmNHS193EEad1tUkeT","cwMeta":{"cwName":"Personal Empty Wallet 1","cwAssurance":"CWANormal","cwUnit":0},"cwAccountsNumber":1,"cwAmount":{"getCCoin":"0"},"cwHasPassphrase":false,"cwPassphraseLU":1.526971738682733246e9}}
```

It means that new wallet is created. As you can see, it doesn't contain any ADA: `{"getCCoin":"0"}`. But it's
possible to start benchmarking of some endpoints.

## Fill Wallet Config

Ask information about current accounts/addresses:

```
$ curl --cacert ./scripts/tls-files/ca.crt https://localhost:8090/api/accounts
```

You will get this response (in pretty view):

```
{
	"Right": [{
		"caId": "Ae2tdPwUPEZMeiDfNHZ45V7RoaSqd4oSMuG4jo7asvmNHS193EEad1tUkeT@2147483648",
		"caMeta": {
			"caName": "Initial account"
		},
		"caAddresses": [{
			"cadId": "DdzFFzCqrhtCiiLVZtX8Ed8DFriGni43h8chAAoz9wCKwiQbzdLXCDGfPRY2ANavaEDDEK7DUVAHxR2oxfr9k7LwyachbkBP2FmyE47P",
			"cadAmount": {
				"getCCoin": "0"
			},
			"cadIsUsed": false,
			"cadIsChange": false
		}],
		"caAmount": {
			"getCCoin": "0"
		}
	}]
}
```

Value of `caId` is a full index of account, `Ae2tdPwUPEZMeiDfNHZ45V7RoaSqd4oSMuG4jo7asvmNHS193EEad1tUkeT@2147483648`.
Full index of account is formed as `walletId@accountIndex`, in this case index is `2147483648`. Each account has unique
index, because wallet can contain a lot of accounts.

Value of `caAddresses` contains a list of addresses in this account. Currently there's one address, `DdzFFzCqrhtCiiLVZtX8Ed8DFriGni43h8chAAoz9wCKwiQbzdLXCDGfPRY2ANavaEDDEK7DUVAHxR2oxfr9k7LwyachbkBP2FmyE47P`.

Now we can fill `Wallets.yaml` configuration. Open `wallet-new/bench/config/Wallets.yaml` and paste this text in it:

```
---
Wallets:
  -
    Accounts:
      -
        Addresses:
          - DdzFFzCqrhtCiiLVZtX8Ed8DFriGni43h8chAAoz9wCKwiQbzdLXCDGfPRY2ANavaEDDEK7DUVAHxR2oxfr9k7LwyachbkBP2FmyE47P
        AccountId: Ae2tdPwUPEZMeiDfNHZ45V7RoaSqd4oSMuG4jo7asvmNHS193EEad1tUkeT@2147483648
    WalletId: Ae2tdPwUPEZMeiDfNHZ45V7RoaSqd4oSMuG4jo7asvmNHS193EEad1tUkeT
```

So, one wallet, one account and one address.

## Fill Endpoints Config

Open `wallet-new/bench/config/Endpoints.csv` and specify one endpoint for benchmarking:

```
BenchName,NumberOfMeasures,MinDelayBetweenCalls,MaxDelayBetweenCalls,PathToReportFile,PathToResponseReportsFile
GetWalletsBench,10,0.0,0.0,PATH_TO_CARDANO_SL_REPO/wallet-new/bench/results/GetWalletsBenchReport.csv,PATH_TO_CARDANO_SL_REPO/wallet-new/bench/results/GetWalletsResponseReports.txt
```

In this case we will benchmark one single endpoint, `api/wallets`. Please change `PATH_TO_CARDANO_SL_REPO` to
correct path to your `cardano-sl` repository.

## Launch Benchmarking

If you don't have `stack` command, please [install it](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
Afte that run this command from the root of `cardano-sl` repository:

```
$ stack bench cardano-sl-wallet-new --benchmark-arguments "--tls-pub-cert=$PWD/scripts/tls-files/ca.crt \
                                                           --tls-priv-key=$PWD/scripts/tls-files/server.key \
                                                           --wal-conf=$PWD/wallet-new/bench/config/Wallets.yaml \
                                                           --ep-conf=$PWD/wallet-new/bench/config/Endpoints.csv \
                                                           --analyze"
```

Benchmarking will start. You will see something like this:

```
cardano-sl-wallet-new-1.2.0: benchmarks
Running 1 benchmarks...
Benchmark cardano-sl-wallet-new-bench: RUNNING...
benchmarking GetWalletsBench ... took 6.548 s, total 56 iterations
benchmarked GetWalletsBench
time                 121.0 ms   (118.2 ms .. 125.4 ms)
                     0.998 R²   (0.993 R² .. 1.000 R²)
mean                 117.8 ms   (115.6 ms .. 120.0 ms)
std dev              3.669 ms   (2.605 ms .. 5.151 ms)

Benchmark cardano-sl-wallet-new-bench: FINISH
```

## Create a New Wallet with ADA

Now we have to create another wallet with some ADA, in this case we will be able to benchmark transactions.
Of course, this ADA is not a real money.

First of all, [download default secret key](https://github.com/input-output-hk/daedalus/blob/develop/features/support/default-wallet.key).
Afte that run this command from the root of `cardano-sl` repository:

```
$ curl -X POST -H "Content-Type: application/json" -d '"PATH_TO_SECRET_KEY_FILE"' \
    --cacert ./scripts/tls-files/ca.crt https://localhost:8090/api/wallets/keys
```

where `PATH_TO_SECRET_KEY_FILE` is a full path to downloaded `default-wallet.key`.

You will see this response (in pretty view):

```
{
	"Right": {
		"cwId": "Ae2tdPwUPEZEK5DvxPMtnTnUfQg8coWAAbNfLEvQ4GqWTe9h8d6AEkBDMce",
		"cwMeta": {
			"cwName": "Genesis wallet",
			"cwAssurance": "CWANormal",
			"cwUnit": 0
		},
		"cwAccountsNumber": 1,
		"cwAmount": {
			"getCCoin": "37499999999166"
		},
		"cwHasPassphrase": false,
		"cwPassphraseLU": 1.526974815374545635e9
	}
}
```

As you can see, value of `getCCoin` is `37499999999166`, it is an amount of money in this wallet.
Please note that this is an amount in Lovelaces, not in ADA. 1 ADA = 1000000 Lovelaces, so 37499999999166
Lovelaces is ~37499999 ADA.

Now you can ask an information about accounts/addresses again:

```
$ curl --cacert ./scripts/tls-files/ca.crt https://localhost:8090/api/accounts
```

Response is (in pretty view):

```
{
	"Right": [{
		"caId": "Ae2tdPwUPEZMeiDfNHZ45V7RoaSqd4oSMuG4jo7asvmNHS193EEad1tUkeT@2147483648",
		"caMeta": {
			"caName": "Initial account"
		},
		"caAddresses": [{
			"cadId": "DdzFFzCqrhtCiiLVZtX8Ed8DFriGni43h8chAAoz9wCKwiQbzdLXCDGfPRY2ANavaEDDEK7DUVAHxR2oxfr9k7LwyachbkBP2FmyE47P",
			"cadAmount": {
				"getCCoin": "0"
			},
			"cadIsUsed": false,
			"cadIsChange": false
		}],
		"caAmount": {
			"getCCoin": "0"
		}
	}, {
		"caId": "Ae2tdPwUPEZEK5DvxPMtnTnUfQg8coWAAbNfLEvQ4GqWTe9h8d6AEkBDMce@2147483648",
		"caMeta": {
			"caName": "Genesis account"
		},
		"caAddresses": [{
			"cadId": "DdzFFzCqrhsg9ngNRhHEa49se7qEMKyubT9tcE13Fkvh8QC82trpTDsNvdQV7mg9SCZiuENkf77zrtwPXrTyGMNznUsSinPC1gb2ZCqK",
			"cadAmount": {
				"getCCoin": "37499999999166"
			},
			"cadIsUsed": false,
			"cadIsChange": false
		}],
		"caAmount": {
			"getCCoin": "37499999999166"
		}
	}]
}
```

As you can see, there're two accounts from two wallets, and account `Ae2tdPwUPEZEK5DvxPMtnTnUfQg8coWAAbNfLEvQ4GqWTe9h8d6AEkBDMce@2147483648`
contains an address `DdzFFzCqrhsg9ngNRhHEa49se7qEMKyubT9tcE13Fkvh8QC82trpTDsNvdQV7mg9SCZiuENkf77zrtwPXrTyGMNznUsSinPC1gb2ZCqK`
with fake 37499999999166 Lovelaces.

## Benchmark Transactions

There're two ways to test transactions:

1. Send money from one wallet to another one.
2. Send money from one wallet to the **same** wallet.

Please note that if you want to send money from one wallet to the same one, you have to create one
more account in this wallet. In this case you will send ADA from one account to the address in another
account.

Now you already have two wallets, so add an information about them in your `Wallets.yaml`:

```
---
Wallets:
  -
    Accounts:
      -
        Addresses:
          - DdzFFzCqrhsg9ngNRhHEa49se7qEMKyubT9tcE13Fkvh8QC82trpTDsNvdQV7mg9SCZiuENkf77zrtwPXrTyGMNznUsSinPC1gb2ZCqK
        AccountId: Ae2tdPwUPEZEK5DvxPMtnTnUfQg8coWAAbNfLEvQ4GqWTe9h8d6AEkBDMce@2147483648
    WalletId: Ae2tdPwUPEZEK5DvxPMtnTnUfQg8coWAAbNfLEvQ4GqWTe9h8d6AEkBDMce
  -
    Accounts:
      -
        Addresses:
          - DdzFFzCqrhtCiiLVZtX8Ed8DFriGni43h8chAAoz9wCKwiQbzdLXCDGfPRY2ANavaEDDEK7DUVAHxR2oxfr9k7LwyachbkBP2FmyE47P
        AccountId: Ae2tdPwUPEZMeiDfNHZ45V7RoaSqd4oSMuG4jo7asvmNHS193EEad1tUkeT@2147483648
    WalletId: Ae2tdPwUPEZMeiDfNHZ45V7RoaSqd4oSMuG4jo7asvmNHS193EEad1tUkeT
```

Now it is possible to send ADA from one wallet to another one.

### Caveats

During transaction benchmarking the tool picks source wallet (technically - source account in some wallet) randomly.
But currently only one of your two wallets contains money, so if tool will pick the wallet without money, transaction
will fail.

Let's try it. Change your `Endpoints.csv` with something like this:

```
BenchName,NumberOfMeasures,MinDelayBetweenCalls,MaxDelayBetweenCalls,PathToReportFile,PathToResponseReportsFile
NewPaymentBench,10,0.0,0.0,/home/denis/Code/cardano-sl/wallet-new/bench/results/NewPaymentBenchReport.csv,/home/denis/Code/cardano-sl/wallet-new/bench/results/NewPaymentResponseReports.txt
```

And run benchmarking. You will see some result:

```
cardano-sl-wallet-new-1.2.0: benchmarks
Running 1 benchmarks...
Benchmark cardano-sl-wallet-new-bench: RUNNING...
benchmarking NewPaymentBench ... took 6.566 s, total 56 iterations
benchmarked NewPaymentBench
time                 120.8 ms   (116.2 ms .. 125.0 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 119.0 ms   (116.9 ms .. 121.3 ms)
std dev              3.946 ms   (2.984 ms .. 5.139 ms)

Benchmark cardano-sl-wallet-new-bench: FINISH
```

But this result is not a valid. Open `wallet-new/bench/results/NewPaymentResponseReports.txt`:

```
Server returned an error: Request error (Cannot send transaction: Transaction creation error: not enough money on addresses which are not included in output addresses set, need 814300 coin(s) more)
S
```

Message `not enough money on addresses` tells us that source wallet is an empty one.

## Automatic Generation of Wallets/Accounts/Addresses/Transactions

Benchmarking results are depend on actual number of wallets/accounts/addresses/transactions. For example,
if one node contains 2 wallets and another one - 2000 wallets, the time of response for request to `/api/wallets`
endpoint will differ. Or if we ask history of transactions on particular wallet, response with 10 transactions
in history will be much faster than if there're 10 millions transactions.

To get more realistic benchmarking, we have to work with a big number of addresses/transactions etc. There's
a tool `dbgen` we can use to generate a lot of addresses with fake money and a lot of transactions. Please
see its [README](https://github.com/input-output-hk/cardano-sl/blob/develop/tools/src/dbgen/README.md) for more
details.
