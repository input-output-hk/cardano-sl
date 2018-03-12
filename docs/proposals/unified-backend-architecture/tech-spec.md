# Technical Specification for Unified Backend Architecture

## Author and Document Owner

[Denis Shevchenko](https://iohk.io/team/denis-shevchenko/)

## Status

DRAFT

## Proposal

We propose to create a lightweight Daedalus client. And it is possible to unify the different use cases under a
single architecture, starting from the crucial observation that both interfacing with an hardware wallet or
provide a mobile client can be seen as a specialisation of the general case of having a "keyless" wallet backend
operating with the private keys and cryptography "hosted" on the client side. It will allow to keep blockchain
on a backend only.

## Motivation

Currently Daedalus can be treated as a heavyweight client: it works in a pair with Cardano node which stores
full copy of the blockchain. It leads three problems:

1. Such a heavyweight client must have enough disk space to store growing blockchain.
2. Cardano node requires quite a lot system resources.
3. During (first) start heavyweight client must sync (new) blocks and this is long process, even with good
   internet connection.

These problems make impossible to install such a heavyweight client on a mobile device. The purpose of Unified
Backend Architecture is to solve these problems, while preserving a common architecture with the heavyweight
desktop client and heavyweight desktop client with hardware devices.

## Prerequisites

The reader should be familiar with following concepts:

1. Daedalus frontend,
2. wallet backend web API,
3. work with wallets,
4. transaction processing.

Also it is assumed that the reader knows basic terminology from [Cardano Docs](https://cardanodocs.com/),
for example, UTXO.

## Assumptions

N/A, see **Prerequisites**.

## Requirements

Unification of the wallet backend architecture should allow to:

1. work with different hardware wallets in the same manner,
2. create lightweight Daedalus mobile clients (iOS/Android),
3. create lightweight Daedalus desktop clients (Windows/macOS/Linux).

The main benefit of such a lightweight client is independence from the growing blockchain: the client does not
store blockchain locally and does not wait for a "Syncing blocks".

Moreover, the unified architecture can also cover the existing desktop heavyweight solution, running the wallet
backend on the same machine. This is what will give us maximum sharing within the codebase.

## Key Idea

As mentioned before, the key idea is to unify the different use cases under a single architecture, we mean
hardware wallets, mobile clients, lightweight desktop clients and heavyweight desktop clients.

In the case of [Ledger](https://www.ledgerwallet.com/) (or any other hardware wallet) the privte keys and the
cryptography are hosted on the device, whereas in a mobile client they are held on a smartphone. And this schema
fits for "featherweight Daedalus desktops" that do not want to host the growing blockchain on the computer's
storage.

More specifically, a Daedalus mobile client will consist of an iOS or Android application which will communicate
with a cluster of nodes that will keep the global UTXO state for the blockchain and that will be capable
to reconstruct each mobile wallet state without hosting any user-specific data.

## Multi-Tenant Architecture

Mobile clients will communicate with a clusters of nodes, so the basic requirement is an ability of one node to work
with particular number of clients simultaneously. For example, if 1 cluster has to serve 10000 mobile clients and
this cluster consists of 100 nodes, each node has to be able to work with 100 clients simultaneously and reliably.
Thus, the longest procedure is a new payment (creating new transaction), and even if all these 100 clients will
sign their transactions and send it for publishing at the same time, the node must be able to handle these
transactions reliably and as fast as possible.

To achieve this goal the node must implement robust concurrent model. More specifically, all requests to the wallet
web API endpoints must be handled concurrently. In addition, it is possible to use queues/buffers, it will allow
to keep sustained transactions submission rate.

In case of correct design decisions, even the slowest endpoints, like `/api/v1/transactions`, can be handled fully
concurrently (publishing to the network, storing in the `wallet-db`, etc).

_Alfredo said: AFAIK that's possible, `acid-state` supports atomic transactions and all the rest of it. Provided we
write the right `Update` transactions we should be fine._

## Scalability

Initially clusters of nodes will be hosted by IOHK (on AWS EC2). And we need a mechanism of auto-scaling of the
cluster: it must be able to grow proportionally to growing number of clients.

**TODO**: To think about auto-scale supervisor. Suppose we have 1 cluster that consists of 100 nodes and each of node
can reliably serve 100 clients. If supervisor sees that number of clients connected to this cluster is growing (and
becomes more than 10000), it launches additional nodes. But exact resource and tuning to do to be decided during
later benchmarking.

_Domen said: We might not base our autoscaling on number of users, but another resource. It's hard to say how it
would scale before we run some benchmarks. It could be that bottleneck is CPU and you can't correlate that with number
of subscribers._

**TODO 2**: Launching additional node is relatively expensive action (in terms of time and memory). So it is better
if one single node will be able to serve more clients than just increase a number of nodes in the cluster.
We have to understand what the real bottlnecks are. E.g. for relays, the number of clients syncing concurrently
_is_ the bottleneck, not the number of client just keeping up with the blockchain.

## User Stories

These are typical user stories (workflows), with description of API calls.

### Just after Installation

After installation of mobile client application user does not see any wallets, and he must create at least one to
work with a client application.

During the first connection with the node the new session is establishing, please read an explanation below.

### «I want new wallet to use ADA»

First of all, user has to create a new wallet. It produces a `POST`-request to `/api/v1/wallets/external` (with
information about client's public key), node creates a new wallet in `wallet-db` and sends response with information
about new wallet.

Even if node `N` which serves the client `C` will die, another node `M` (the client `C` will reconnect to) will be able
to handle information about `C`'s wallet(s), because `wallet-db` is storing in the cluster.

### «I want to have an access to my money from another device»

Wallet can be imported using client's secret key.

Since secret keys remain only on the client, import is a frontend-only operation: just turning the multi-word
passphrase (or backup phrase) into the root keypair.

### «I lost my hardware wallet and want to restore an access to my money»

If user changed a smartphone or lost hardware wallet, it is possible to install mobile Daedalus client (or buy
new hardware wallet device) and restore user's wallet using backup phrase (for example, 12 words) generated
during this wallet creation.

Wallet restoring produces a `POST`-request to `/api/v1/wallets/external`, and node checks if such a wallet
exists. If so, response contains an information about the wallet.

### «I want to see how much money I have»

User can see its current balance (by default, the balance should always be shown in the client application).

After client application launching and after transactions client is asking for current balance. It produces
`GET`(?)-request to `/api/v1/?`, and response contains an information about current balance on particular wallet.

Please note that user can have few wallets with different balances, so balance for current active wallet will be
asked and shown.

**TODO**: To clarify endpoints.

### «I want to receive money from other user»

User can receive a payment from other user. To do it he must provide a receive address.

**TODO**: To clarify details of receive address generation. Will it be generated on the client side on on the node
side? If latter - we need additional API endpoint `/api/v1/addresses/external`, to obtain new receive address.

### «I want to send money to other user»

There are 3 steps to make a new payment:

1. create transaction,
2. sign transaction,
3. publish transaction.

When user creates a new payment (by defining recipient's address, amount and fee), it produces a `POST`-request
to `/api/v1/txs/unsigned`, and response will contain a new transaction without a witness (signature).

After that user signs new transaction using its private keys (hosted on the client side).

Then user send a payment. It produces a `POST`-request to `/api/v1/transactions/external`, node receives transaction
with a signature, forms transaction witness and publish this transaction to the Cardano network as usually.

## API Extension

API v1 must be extended to support both the mobile clients and the hardware wallets.

### Create Unsigned Transaction

When user wants to send money to other user, he creates a new payment. It produces a `POST`-request
to `/api/v1/transactions/unsigned` endpoint, and response will contain a new transaction without a
witness (signature). In addition, response will contain the HD wallet indexes of the keys, this
information is needed to sign new transaction.

### Send Signed Transaction

After user signs new transaction using its private keys (hosted on the client side) he sends it with
a signature. It produces a `POST`-request to `/api/v1/transactions/external`, node receives transaction
with a signature, forms transaction witness and publish this transaction as usually.

## Wallet State

An analysis of the computational complexity of rebuilding the state of a wallet (e.g. its balance and
payments history) on the server in a way as much stateless as possible.

**TODO**: To think about it.

## Authentication Scheme

### HTTPS

Client must be sure that server is not a malicious one, so we use HTTPS-connection with a standard check
of TLS certs on the client side.

### Session

After successful TLS checking client and server establish a new session.

There will be a server-side encrypted token generated when a session is established between a client and a
generic server. To generate this token the user will feed to the server some form of secret, probably a user
generated password or an app/system generated password.

This token will allow a single client to handle and perform operations on multiple wallets at the same time.

This token will be held on the server side and probably used as a key for a `ClientStateMap` which will allow
to cache client-specific info, including a list of addresses known to the server and that belongs to a given
client.

Sessions can expire and the entry is removed from the `ClientStateMap`, which requires the user session to be
recreated again.

Sticky sessions either at the load balancer' level or at the RESTful HTTP location' level to ensure that a
client talks as much as possible with the same server.

There will be a Proof-Of-Ownership between the client and the server. This is needed to ensure that a (malicious)
client will not "peek" into a portion of the server's state it does not control. One practical example would be
a client asking about a balance of an address he does not own.

Some messaging will be done to accommodate the fact standard HD-wallet derivation uses elliptic curves public/private
addresses, but we are dealing with Cardano addresses and we need to take into account things like delegation
information, but these should not affect how authentication is carried out.

## Crypto Operations

A description of how the main operations on a crypto-wallet will be implemented, according to the final
index derivation mechanism chosen (sequential vs random).

We need to use deterministic (sequential or deterministically seeded PRNG).

## Security and Possible Attacks

### MITM

To mitigate the impact of MITM attacks we have to include a fee in transaction explicitly. This will also
helps the client in case of malicious/compromised wallet backend.

Currently it is impossible, because transaction contains just inputs, outputs and attributes, and we have
to know current UTXO to calculate a fee. So we must change transaction's format for explicit including a
fee in it.

**TODO**: To think about it: should the fee be a part of transaction's attributes, or it should be a
separate field? Probably it will be defined in a separate technical specification focusing on that issue
alone.

### Other Attacks

This should eventually be reviewed by the security internal auditors.
