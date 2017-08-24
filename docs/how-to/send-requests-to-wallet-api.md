# How to send requests to Wallet API

## Preparing

Clone [`cardano-sl`](https://github.com/input-output-hk/cardano-sl/) repository:

```
$ git clone git@github.com:input-output-hk/cardano-sl.git
$ cd cardano-sl
```

Build it:

```
$ ./scripts/build/cardano-sl.sh
```

Run [`tmux`](https://github.com/tmux/tmux):

```
$ tmux
```

Then launch nodes:

```
$ ./scripts/launch/demo-with-wallet-api.sh
```

By default 3 nodes will be started.

## Send requests via Node.js

Please make sure you have installed [`nodejs`](https://nodejs.org/) and [`npm`](https://www.npmjs.com/) programs. Tested with `nodejs 4.2.6` and `npm 3.10.3`.

Build Daedalus bridge for wallet API:

```
$ ./scripts/clean/daedalus-bridge.sh
$ ./scripts/build/daedalus-bridge.sh
```

Run `node` and activate an API:

```
$ node
> var api = require('./daedalus/output/Daedalus.ClientApi')
```

Now you can send requests, for example:

```
> api.getWallets().then(console.log).catch(console.log)
```

Please see [Usage Scenario](https://cardanodocs.com/technical/wallet-frontend/#wallet-frontend-api-usage-scenario) for more examples.

## Send requests via curl

You can send requests via [`curl`](https://curl.haxx.se/) as well. Default port for the wallet API is `8090`.

Please note that since `cardano-sl-0.6` we are using TLS. This is an example of request (via SSL connection without certificate):

```
$ curl -k https://localhost:8090/api/settings/sync/progress
```

Possible response:

```
{"Right":{"_spLocalCD":{"getChainDifficulty":19273},"_spNetworkCD":{"getChainDifficulty":19273},"_spPeers":0}}
```

Please see [online documentation for wallet API](https://cardanodocs.com/technical/wallet/api/) for complete information.

## Send requests via Postman

You can send requests using [Postman](https://www.getpostman.com/) program as well.
