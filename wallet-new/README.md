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

### HTTPS

By default, wallet backend only accepts HTTPS connections:

```
$ curl localhost:8090/docs/v1/index/
This server only accepts secure HTTPS connections.
```

We should provide our `ca.crt`:

```
$ curl --cacert scripts/tls-files/ca.crt https://localhost:8090/docs/v1/index/
```

But if we launch a node with `--wallet-debug` option, we can send simple `http`-requests.

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
$ nix-build release.nix -A walletIntegrationTests
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

## Demo (resp. Integration Tests)

### Getting Started

This module contains a few helpers to quickly start a demo cluster of wallet nodes. It is
designed to remove all the overhead of setting up a configuration and an environment and to
_just work_, out-of-the-box. Minor configuration adjustments are however possible via
environment variables. A few things to know:

- The demo executable (`stack test cardano-sl-demo`) runs a cluster of three _identical_ wallet
  nodes. Any configuration change via ENV vars is thereby applied to each node. 

- ENV variables define "base" arguments for a node. When it makes sense (.e.g `DB_PATH`, 
  `LOG_CONFIG`), these "base" arguments are postfixedwith an index (`0`, `1` or `2`).

- Nodes are named respectively: "node0", "node1" and "node2".

- Nodes are configured to not output anything to the console. This can't be changed via any ENV
  var. To consult logs, simply run a `tail -f` on one of the log file (by default,
  `demo/state-demo/logs/node*.log`).

- The demo executable outputs two messages, one upon starting the cluster, and one once the
  cluster is ready to be used (_"Cluster ready!"_).


### Configuring Nodes

A few things can be configured via ENV vars. Each env var is prefixed by `DEMO_` to make them
recognizable. Similarly, anything related to running the cluster is stored within `state-demo`.
This includes tls certificates, logging configurations, the blockchain itself and databases. 

Anything from the _normal_ CLI arguments of a node or a wallet node can be configured via an
ENV variable using an `UPPER_SNAKE_CASE` naming, prefixed with `DEMO_`. For instance, one can
disable TLS client authentication by running:

```
DEMO_NO_CLIENT_AUTH=True stack test cardano-sl-demo
```

Beside nodes have successive ports which depends on the `DEMO_LISTEN`ENV variable defining 
port for the first node. For instance, if its value is set to 3000, nodes will have the
following topology:

    - node0: localhost@3000
    - node1: localhost@3001
    - node2: localhost@3003

In a similar fashion, nodes expose their wallet API on the following addresses:

    - node0: localhost@8090
    - node1: localhost@8091
    - node2: localhost@8092

Note that the documentation server listens on the wallet's port plus 100, e.g. 8190 (resp. 8191 and 8192).

#### Flag arguments

A few arguments of the CLIs are actually flags. For those, a stringified Haskell boolean is
expected as a value (i.e. `True` or `False`, capitalized). See the excerpt above with
`DEMO_NO_CLIENT_AUTH` for an example.


### Configuring Http Client

The Http client talking to nodes may be configured using 4 ENV variables:

    - `DEMO_TLSCERT_CLIENT`: Path to a client TLS certificate
    - `DEMO_TLSKEY_CLIENT`:  Path to a client TLS Key
    - `DEMO_TLSCA`:          Path to a CA TLS certificate
    - `DEMO_WALLET_ADDRESS`: Network address of the target wallet node

By default, it will use certificates generated in 'demo/state-demo'
and talk to `127.0.0.1:8090'.


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
