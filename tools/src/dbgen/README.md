# dbgen

Original version from https://github.com/adinapoli-iohk/dbgen, written by Alfredo Di Napoli.

This is a simple program to generate valid `wallet-db` databases, starting from a specification
stored in a `json` config file.

In order for the program to work correctly, it must be provided with a non-empty RocksDB generated
with the same configuration this program is invoked with, or the DB-content deserialisation will
fail with the "wrong magic" error.

## Usage

A typical usage is to run the tool from the branch/commit your wallet is on. 

The properties file should look something like this:
```
{
    "wallets":1,
    "walletSpec":{
        "accounts":1,
        "accountSpec":{"addresses":100},
        "fakeUtxoCoinDistr":{"type":"none"},
        "fakeTxsHistory":{"type":"none"}
    }
}
```

You should run the program using something like (from say `cardano-sl` root):
```
stack exec dbgen -- --config ./tools/src/dbgen/config.json --nodeDB db-mainnet --walletDB wdb-mainnet --configPath node/configuration.yaml --secretKey secret-mainnet.key --configProf mainnet_full
```

## Printing statistics

If `dbgen` is invoked with the `--stats` option, it will print a bunch of useful stats, namely the wallets,
their accounts and the number of addresses:

<img width="1440" alt="screen shot 2018-01-01 at 16 36 22" src="https://user-images.githubusercontent.com/29383371/34468858-1756d7e8-ef12-11e7-9d15-6b615adc24fb.png">

## Adding Addresses to an existing wallet

If the `--add-to` option is passed, along with a valid string of the form `walletId@accountIndex`, then
instead of rebuild the DB from scratch, new addresses (which number can be controlled from the `config.json`
file) will be added. For example, the following two commands first create a DB with a single wallet, a single
account and 10 addresses, then it appends extra 100 to it:

## Generate fake UTXO

If we want to generate fake utxo in order to test how the wallet behaves like a "real" wallet, you can modify the 
configuration and call `dbgen` with something like(we presume that the `add-to` account exists?):
```
stack exec dbgen -- --config ./tools/src/dbgen/config.json --nodeDB db-mainnet --walletDB wdb-mainnet --configPath node/configuration.yaml --secretKey secret-mainnet.key --configProf mainnet_full --add-to Ae2tdPwUPEZJHA8wEbVWoT4zgDGuWkXT9vLW6RzLvMt8kYCefkBQ1nixzpX@2147483648
```

For example you can use this configuration:
```
{
   "wallets":1,
   "walletSpec":{
      "accounts":1,
      "accountSpec":{"addresses":100},
      "fakeUtxoCoinDistr":{"type":"range","range":100,"amount":1000},
      "fakeTxsHistory":{"type":"none"}
   }
}
```

## Generate fake txs history

If we want to generate fake txs history in order to test how the wallet behaves like a "real" wallet, you can modify the 
configuration and call `dbgen` with something like(we presume that the `add-to` account exists?):
```
stack exec dbgen -- --config ./tools/src/dbgen/config.json --nodeDB db-mainnet --walletDB wdb-mainnet --configPath node/configuration.yaml --secretKey secret-mainnet.key --configProf mainnet_full --add-to Ae2tdPwUPEZJHA8wEbVWoT4zgDGuWkXT9vLW6RzLvMt8kYCefkBQ1nixzpX@2147483648
```

For example, you can use this configuration:
```
{
   "wallets":1,
   "walletSpec":{
      "accounts":1,
      "accountSpec":{"addresses":100},
      "fakeUtxoCoinDistr":{"type":"range","range":100,"amount":1000},
      "fakeTxsHistory":{"type":"simple","txsCount":10015,"numOutgoingAddress":3}
   }
}
```


## Pitfalls

- Not passing an external RocksDB path would result in a segmentation fault (due to invalid FFI code
  usage in the `rocksdb-haskell` library)

- Faking `StakeLock` sync to avoid the relevant `MVar` from deadlocking.

- By default, the current version of the wallet creates with each new wallet a default account and a default
  address.

## To implement

- https://iohk.myjetbrains.com/youtrack/issue/CSL-2210

