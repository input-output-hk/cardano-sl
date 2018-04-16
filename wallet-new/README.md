# Cardano Wallet API

This is the Wallet backend for a Cardano node.

## Installation

Installation follows the standard approach to installing Stack-based projects:

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install cardano-sl-wallet-new` from the project *root* to install this package.

## Launching

A wallet is a part of Cardano node, so from technical point of view if we launch a node -
we launch a wallet as well. But please note that when we are talking about a node, we mean one
from two different executables:

1. `cardano-node`
2. `cardano-node-simple`

When we start `cardano-node`, wallet functionality is activated automatically. But when we
start `cardano-node-simple`, wallet functionality is disabled, in this case node's log will
contain the following line:

```
Wallet is disabled, because software is built w/o it
```

## Flushing Logs to Disk

Note that by default, logs are only sent to stdout/stderr. If you want to enable flushing on
disk in rotated log files, use the `--log-config` option and specify a logging configuration 
yaml file to define what to log and where to log it. 

For instance:

```yaml
rotation:
    logLimit: 104857600 # 100 MB
    keepFiles: 20
loggerTree:
  severity: Debug+
  files:
    - node.log
```

You can find more examples and working configurations in _../log-configs_. Using stack, you
may use _../log-configs/cluster.yaml_ to start a node and dump all logs to a file _node.log_:

```
$ stack exec cardano-node -- --topology=wallet-new/topology-examples/testnet.yaml \
                             --configuration-key=mainnet_staging_short_epoch_full \
                             --log-config=log-configs/cluster.yaml
```


## API

We describe how to interact with our API via the popular [Swagger](https://swagger.io/)
framework and format. Swagger relies on a single specifications file in a `json` format. From
this file is derived a handful of tools.

A Cardano node exposes both a Swagger file corresponding to the wallet API and a visualization
tool that can be used for browsing the specification as well as playing with the API. As a
first step, start a `cardano-node` using the following command:

```
$ stack exec cardano-node -- --topology=wallet-new/topology-examples/testnet.yaml \
                             --configuration-key=mainnet_staging_short_epoch_full \
                             --wallet-debug                                       \
                             --rebuild-db
```

From there, you can browse the API documentation for V0 and V1 through the following URLs:

- http://localhost:8090/docs/v0/index/
- http://localhost:8090/docs/v1/index/

The visualization at those URLs lets you play with the API by the mean of a _Try it out_ button
made available for each endpoint. This will seemingly contact the node already running on your
local machine with actual HTTP requests augmented with the parameters you provide!

### HTTPS

By default, wallet backend only accepts HTTPS connections:

```
$ curl localhost:8090/docs/v1/index/index.html
This server only accepts secure HTTPS connections.
```

We should provide our `ca.crt`:

```
$ curl --cacert scripts/tls-files/ca.crt https://localhost:8090/docs/v1/index/index.html
```

But if we launch a node with `--wallet-debug` option, we can send simple `http`-requests.

### Swagger Specification

If needed, you can access the corresponding raw Swagger specification files via these URLs:

- http://localhost:8090/docs/v0/swagger.json
- http://localhost:8090/docs/v1/swagger.json

### Development Endpoints

If you run the wallet in debug mode (with `--wallet-debug` option), you'll have an access to
an extra set of endpoints, documented under this URL:

- http://localhost:8090/docs/development/index

### Online API Documentation

Swagger documentation for API V0 and V1 is published on [cardanodocs.com](https://cardanodocs.com/)
under these URLs:

- https://cardanodocs.com/technical/wallet/api/v0/
- https://cardanodocs.com/technical/wallet/api/v1/

## Testing

Wallet tests can be run using this command (from the project *root* directory):

```
$ stack test cardano-sl-wallet-new
```

## Import Genesis Wallet

It is possible to import Devnet Genesis wallet from Daedalus.

First of all, clone [Daedalus](https://github.com/input-output-hk/daedalus) repository:

```
$ git clone git@github.com:input-output-hk/daedalus.git
```

Now use following command (from the `cardano-sl` *root* directory):

```
$ curl -X POST                                  \
       -H "Content-Type: application/json"      \
       -d '"PATH_TO_SECRET_KEY"'                \
       --cacert scripts/tls-files/ca.crt
       https://localhost:8090/api/wallets/keys
```

where `PATH_TO_SECRET_KEY` is path to `daedalus/features/support/default-wallet.key`.

Please note that `daedalus` is a root directory of Daedalus' repository.

Response:

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
			"getCCoin": "0"
		},
		"cwHasPassphrase": false,
		"cwPassphraseLU": 1.52232831369479818e9
	}
}
```
