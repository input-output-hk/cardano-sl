# dbgen

This is a simple program to generate valid `wallet-db` databases, starting from a specification
stored in a `Dhall` config file.

In order for the program to work correctly, it must be provided with a non-empty RocksDB generated
with the same configuration this program is invoked with, or the DB-content deserialisation will
fail with the "wrong magic" error.

## Usage

First compile `dbgen` against the same revision of Cardano you want it to generate the `wallet-db` for.
At the moment the program has been tested with `release/1.0.4`
(SHA: `6e53bf599097aa0b55738a454115f76f69c9489e`). Then edit the `config.dhall` file to specify the number
of wallets, accounts and addresses to generate. For example, the following config will generate 10 wallets,
each wallet with 1 account and 100 addresses underneath:

```
{ wallet_spec = { account_spec = { addresses = 100 }, accounts = 1 }
, wallets     = 10
}
```

Once you are ready, you can invoke `dbgen`:

```
Usage: dbgen [--config CONFIG.DHALL] [--dbPath rocksdb-path]
```

If all is well, the program will generate a new `wallet-db` filled with synthetic but valid data,
together with stats about how much time it took to generate those:

```
dbgen --dbPath ../cardano-sl/run/node-db0
Faking StateLock syncing...
Generating 1 wallets...
Action took 0.13194 seconds.
Generating 1 accounts for Wallet 1...
Action took 0.083039 seconds.
Generating 10 addresses for Account CAccountId "Ae2tdPwUPEZGwUVR8meJ6mgbHeiC16TNyM3wTDYRYwDBMeCHUXdMVLHKupC@793406314"...
Action took 0.739177 seconds.
OK.
```

## Printing statistics

If `dbgen` is invoked with the `--stats` option, it will print a bunch of useful stats, namely the wallets,
their accounts and the number of addresses:

<img width="1440" alt="screen shot 2018-01-01 at 16 36 22" src="https://user-images.githubusercontent.com/29383371/34468858-1756d7e8-ef12-11e7-9d15-6b615adc24fb.png">

## Adding Addresses to an existing wallet

If the `--add-to` option is passed, along with a valid string of the form `walletId@accountIndex`, then
instead of rebuild the DB from scratch, new addresses (which number can be controlled from the `config.dhall`
file) will be added. For example, the following two commands first create a DB with a single wallet, a single
account and 10 addresses, then it appends extra 100 to it:

```
stack exec dbgen -- --nodeDB ../cardano-sl/run/node-db0 --walletDB wallet-db
# now grab the account id by printing --stats
stack exec dbgen -- --nodeDB ../cardano-sl/run/node-db0 --walletDB wallet-db --stats               
Wallets: 1

- Wallet #1, ready, Synced[AbstractHash 6e5377d2c78b8ba69644bc17caa8c05131d9dd32ed95e8da7a2c3babcad90e6e], PendingTxs: 0

Accounts: 2

- Initial account, Ae2tdPwUPEZGwUVR8meJ6mgbHeiC16TNyM3wTDYRYwDBMeCHUXdMVLHKupC@2147483648, addresses: 1, removed: 0
- Account Number #1, Ae2tdPwUPEZGwUVR8meJ6mgbHeiC16TNyM3wTDYRYwDBMeCHUXdMVLHKupC@75378469, addresses: 111, removed: 0

Number of used addresses: 0
Number of change addresses: 0

# Now add more addresses..
stack exec dbgen -- --nodeDB ../cardano-sl/run/node-db0 --walletDB wallet-db --add-to Ae2tdPwUPEZGwUVR8meJ6mgbHeiC16TNyM3wTDYRYwDBMeCHUXdMVLHKupC@75378469
```

## Pitfalls

- Not passing an external RocksDB path would result in a segmentation fault (due to invalid FFI code
  usage in the `rocksdb-haskell` library)

- Faking `StakeLock` sync to avoid the relevant `MVar` from deadlocking.

- By default, the current version of the wallet creates with each new wallet a default account and a default
  address.

## To implement

- @Ilya Peresadin - probably we also should add option for `dbgen` which generates ton of history entries (https://input-output-rnd.slack.com/archives/C7DRYGPT8/p1515507443000578)

- @Ilya Peresadin and @George Agapov - add fake utxo entries to local db (https://input-output-rnd.slack.com/archives/C7DRYGPT8/p1515493184000042)
