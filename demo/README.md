# Demo

## Getting Started

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


## Configuring Nodes

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


## Configuring Http Client

The Http client talking to nodes may be configured using 4 ENV variables:

    - `DEMO_TLSCERT_CLIENT`: Path to a client TLS certificate
    - `DEMO_TLSKEY_CLIENT`:  Path to a client TLS Key
    - `DEMO_TLSCA`:          Path to a CA TLS certificate
    - `DEMO_WALLET_ADDRESS`: Network address of the target wallet node

By default, it will use certificates generated in 'demo/state-demo'
and talk to `127.0.0.1:8090'.
