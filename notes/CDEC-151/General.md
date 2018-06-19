# CDEC-151: inter-node communication protocol

## Why

  1. The current protocol is over-complicated:
     - Message codes and SYN/ACK for bidirectional channels multiplexed on a
       TCP socket
     - Peer data (`VerInfo`) handshake
     - Separate lightweight connection to indicate a subscription and send
       keepalive messages
     We should use something simpler if possible: asynchronous message passing
     rather than RPC.
  2. Current outbound queueing and subscription systems rely upon a
     classification of peers as core, relay, or edge. Hegemony is built-in.
     The new systems must allow subscribers to choose which messages types they
     wish to receive (currently the sender choose what to send, based on their
     own choice of classification).
  3. Current blockchain selection and download scheme is bad in general.

## Requirements

  1. Relaying of all types of data:
     - Blocks and block headers
     - Transactions
     - SSC
     - Update proposals and votes
     - Delegation certificates
  2. Download of blockchains without inviting denial of service attacks
     (easy in PoW, hard in PoS, since a well-formed blockchain can be created
     cheaply).
  3. Subscription topics: not all data needs to reach every node, so we can
     save some traffic by opting-in/out of topics.
  4. Support for planned yet unspecified features like the core DIF.

## Asynchronous message I/O

The current inter-node communication protocol is factored into 3 layers above
TCP:

  - network-transport-tcp
  - time-warp, for lack of a better name
  - the cardano-sl protocol, with message codes and listeners with send and
    receive types

The multiplexed unidirectional channels of network-transport(-tcp) are wrangled
into duplex channels by time-warp, and used to invoke routines according to a
`Word16` message code. It's amazing that a third party has reverse engineered a
JavaScript client for this protocol.

network-transport's multiplexed unidirectional channels are unnecessary, and
its single-receive-queue model makes it difficult to get backpressure. One
reliable and ordered duplex channel between two peers will suffice. The
synchronous request/response model is also unnecessary: besides block-related
communication, everything follows an announce/request/data model, in which
short keys are announced, and the corresponding data can be requested. These
are independent and require no local state: something can be requested at any
time, so long as the key is known. See [./Types.hs](./Types.hs).

What is required of the protocol in order to do blockchain selection is not
obvious. See [./ChainSelection.hs](./ChainSelection.hs).

## Subscription topics

Currently the subscribee decides which types of message to send to the
subscriber (according to the outbound queue policy). Since the core/relay/edge
classification must go, this cannot work. Instead, the subscriber will opt-in
to topics. The obvious choice for topics is

  - Block
  - Delegation
  - Ssc
  - Txp
  - Update

This is meant to control which announcements will be sent to the subscriber.
Even if not subscribed to a topic, the subscriber could still explicitly
request a datum of that topic. The motivation is that some nodes (those of
end-users who have delegated all of their stake) will be interested only in
blocks. It's not clear whether there's any value in allowing a node to
subscribe to arbitrary combinations of these topics. It could be that only two
modes are needed: everything, or just blocks.

## Message size limits

The current wire protocol is blockchain-aware, because byte limits on update
proposals, transactions, blocks, and block headers are part of the block
version data. This is tricky: it means that two peers may not be able to
communicate if they disagree on the block version data, for instance because
they are working with different forks. Supposing `A` has a better chain than
`B`, but which includes an update to the block version data that increases the
block header size limit, `A` may not be able to successfully communicate a
header to `B`, because `B` will reject it due to excess size!

  1. We cannot download blocks or block headers newest-to-oldest, nor can we
     download headers oldest-to-newest without getting the bodies of each, in
     order.
     When do updates take effect? If they only happen at epoch boundaries then
     we can do newest-to-oldest and header-only downloads within an epoch.
  2. Related to the above point: it's possible that a header announcement for a
     better chain is rejected locally, because the update has not been seen.

This wouldn't be an issue if we did not use the block version data for byte
limits on network data. The only alternative is to have fixed limits, else
we open up to attacks.

Possible solutions:

  1. Always announce fixed size things like epoch, slot, genesis block hash for
     that epoch, and always include a hash of the block version data for which
     the thing is valid, and the epoch/slot/hash of the block in which it was
     adopted. Only send messages to clients when it's known that they can
     figure out the limits.
     This is quite complicated. Ideally we could have a protocol that wasn't
     aware of / dependent upon blockchain state.
  2. Define a fixed limit, or at least a maximum limit, for block headers,
     blocks, transactions, and update proposals. The MPC threshold, which is
     used to derive limits on existing SSC-related data by determining the
     maximum number of participants, would remain. Instead of taking potentially
     unbounded-size things (`HashMap`s of `VssCertificates`) we would accept
     individual bounded key/value pairs.

## Version information

The current protocol ensures that both ends of a channel know each-others'
`VerInfo` record, which contains a `ProtocolMagic` and `BlockVersion`, but is
sadly missing any mention of the *software* version (`ProtocolMagic` and
`BlockVersion` refer to the *blockchain* not the software). Use of this thing
should be discontinued.

Instead of integrating version negotiation in the protocol, we may want to
establish it via the peer discovery system. Part of a `FIND_NODE` response
could be a list of supported protcol versions and the relevant IP/PORT of the
server which implements it.

## Repurposing of the OutboundQueue

The `OutboundQueue` is responsible for 2 things:

  1. Outbound traffic shaping policies.
  2. Keeping a manifest of known peers and classification by type.

With this information it can determine where to put a message. As such, the
`send*` members of the full diffusion simply construct the relevant
"conversation" and defer to its `OutboundQueue`.

Classification by type must be removed (core, relay, edge make no sense in a
distributed setting), but some sort of outbound queueing mechanism will remain.
It's not clear whether parts of the current implementation, like the
`ConcurrentMultiQueue`, will be suitable for re-use.
