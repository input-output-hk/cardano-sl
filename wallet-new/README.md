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

- https://localhost:8091/docs/v0/index/
- https://localhost:8091/docs/v1/index/


You may also run a simple cURL command to check whether the node is up-and-running:

```
$ curl https://localhost:8090/api/v1/node-info \
   --cacert scripts/tls-files/ca.crt \
   --cert scripts/tls-files/client.pem
```

> *NOTE* 
>
> Every node running a wallet API needs x509 certificates for enabling TLS support. By default,
> those certificates are located in `./scripts/tls-files`. Use them if you need a CA or a
> client certificate. 


## Local Cluster

Running a node against `mainnet_staging` may not be ideal for testing. The node will also need
time to synchronize and won't run the full API capabilities until having done so. To cope with
this, one may run a local cluster of nodes, acting upon a fresh database, speeding up most of
the operations. To run a local cluster, _nix_ is your friend:

```
$ nix-build -A demoCluster -o run-demo --arg useStackBinaries true && ./run-demo
```

This will run a local cluster after having set up a fresh environment for it in `./state-demo`.
There are some files of interest in this folder you may need like the tls certificates or the
logging configurations. 


### HTTPS

By default, wallet backend only accepts HTTPS connections:

```
$ curl localhost:8090/api/v1/node-info
This server only accepts secure HTTPS connections.
```

Read the documentation about TLS authentication in [docs/tls-authentication.md](../docs/tls-authentication.md)
for details about how to contact a wallet node with TLS. 


### Swagger Specification

If needed, you can access the corresponding raw Swagger specification files via these URLs:

- https://localhost:8091/docs/v0/swagger.json
- https://localhost:8091/docs/v1/swagger.json

### Development Endpoints

If you run the wallet in debug mode (with `--wallet-debug` option), you'll have an access to
an extra set of endpoints, documented under this URL:

- https://localhost:8091/docs/development/index

### Online API Documentation

Swagger documentation for API V0 and V1 is published on [cardanodocs.com](https://cardanodocs.com/)
under these URLs:

- https://cardanodocs.com/technical/wallet/api/v0/
- https://cardanodocs.com/technical/wallet/api/v1/

## Testing

Wallet unit tests can be run using this command (from the project *root* directory):

```
$ stack test cardano-sl-wallet-new
```

Wallet integration tests can be run using this command (from the project *root* directory):

```
$ nix-build release.nix -A walletIntegrationTests --arg useStackBinaries true 
```

## Developing

We have a [`Makefile`](./Makefile) with some helpful commands for development.
`make ghcid` runs a GHCid daemon with the project, reloading quickly on every save.
This gives you fast feedback on your changes.
`make ghcid-test` runs GHCid daemon, which will also run tests if there are no compile errors.

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
       --cacert scripts/tls-files/ca.crt        \
       --cert scripts/tls-files/client.pem      \
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

## Troubleshooting

##### commitAndReleaseBuffer: invalid argument (invalid character)

When running a node directly with stack, you may encounter an unexpected runtime error
`commitAndReleaseBuffer` if your machine's locale aren't well suitable for managing unicode
characters.

On a _*nix_ system, you can view your current locale by doing:

```
$ locale
LANG=en_US.UTF-8
LANGUAGE=en_US
LC_CTYPE="en_US.UTF-8"
LC_NUMERIC=nl_NL.UTF-8
LC_TIME=nl_NL.UTF-8
LC_COLLATE="en_US.UTF-8"
LC_MONETARY=nl_NL.UTF-8
LC_MESSAGES="en_US.UTF-8"
LC_PAPER=nl_NL.UTF-8
LC_NAME=nl_NL.UTF-8
LC_ADDRESS=nl_NL.UTF-8
LC_TELEPHONE=nl_NL.UTF-8
LC_MEASUREMENT=nl_NL.UTF-8
LC_IDENTIFICATION=nl_NL.UTF-8
LC_ALL=
```

One way to cope with this is to force different (UTF-8 compatible) locales when starting a node
using environment variables as follows:

```
LANG=en_GB.UTF-8 LC_ALL=en_GB.UTF-8 stack exec -- ...
```


##### API returns `415  Unsupported Media Type`

The wallet's API can be quite picky about media-types and expect both a given type and an
associated charset. You'll likely get this error when

- The request doesn't provide any `Content-Type` or `Accept` header
- The base mime-type isn't `application/json`
- The associated charset is missing or different from `utf-8`

To fix, make sure to provide both a `Content-Type` and `Accept` headers with the following
value:

```
application/json;charset=utf-8
```


##### API returns `error:14094416:SSL routines:ssl3_read_bytes:sslv3 alert certificate unknown`

You're missing a valid client certificate to contact the node. For development, you may run the
node with `--no-client-auth` or provide a valid corresponding client x509 certificates. More
information in [docs/tls-authentication.md](../docs/tls-authentication.md).
