# Communication patterns (pub/sub, request/response)

## Overview

For relaying: instead of choosing where to forward messages (choosing the 'out'
edges), nodes will publish to their subscribers (choose the 'in' edges).
For initiating broadcasts: nodes will still choose the destination peers.

A subscription will be organized into topics (blocks, transactions, MPC, etc.).

Pub/sub will be part of the diffusion layer. It will use the peer roster
(discovery with QoS estimation and core DIF feature) to choose which peers to
subscribe to. Publishers do not necessary have subscribers in their peer roster.
That's to say, a subscriber isn't necessarily judged to be a candidate for
subscribing to. The judgement "I should subscribe to this peer" will not be
symmetric.

The subscription mechanism must be reactive: it must be possible to subscribe
in response to a subscription. The motivating application is a relay program
which subscribes to a topic only if at least one other node has subscribed
to it for that same topic. Call it lazy subscription.
This shouldn't be a problem, because the same system which observes new
subscriptions is responsible for making subscriptions to others.

### Hybrid request/repsponse and pub/sub.

Functions like `getBlocks` which bring data up from the diffusion layer may
need to request that data from peers, so a hybrid request/response and pub/sub
model is needed. Transaction, delegation, and update submission also require
the active request/response approach, because edge nodes behind a firewall
and/or NAT must be able to do this, but such a node cannot have any
subscribers. Even publishing a new block (slot leader) must work by
request/response, for edge nodes must be capable of doing this as well.

### Porting the Inv/Req/Data relay system

We believe the key/value distinction is still important: you can send a key
identifying the data and the receiver can request or reject the payload. This
will probably save a lot of wasted traffic as a piece of data can reach a node
by multiple paths.

When a piece of data is being published (like a new block or a transaction,
see request/response) the key/value can be skipped and the whole payload can
be sent at once, because we know the receiver won't have it. The key/value
distinction is only useful when relaying, and so is relevant only to pub/sub.

When a datum is to be published (that's to say, republished, or relayed), its
key will be sent to all subscribers, and the subscribers can asynchronously
request the data for it or indicate that it will not request it. This could
permit a TCP-style flow control algorithm: if the publisher notices too much
"un-ACKed" data (keys for which the particular subscriber has not returned a
yes or no answer) then it can attenuate the flow.

To avoid cycles in relaying, a node must remember keys which have been
published to it for a certain amout of time. If such a key comes up again,
it will send a negative ACK meaning "I don't want the payload" and it will not
be relayed. A datum is relayed only if the payload was requested.
The current implementation uses the blockchain and transaction mempool to
determine whether something should be requested and relayed; the diffusion
layer will use its own cache.

Here's a crude state machine for the relay cache.

States:

  1. `k` and `v` are unknown
  2. `k` is known but `v` is unknown
  3. `k` and `v` are known

Transitions:

  - `k` is published to us
    If in state 1, ask the publisher for `v`, enter state 2.
    If in state 2, reject the offer from the publisher, remain in state 2.
    If in state 3, reject the offer from the publisher, remain in state 3.
  - `v` is published to us
    If in state 1, remain in state 1. This should not happen.
    If in state 2, remember `v` and relay `k` and `v` to subscribers, enter
      state 3.
    If in state 3, remain in state 3. This should not happen.
  - TTL on the key expires.
    If in state 1, this makes no sense, since we don't have the key.
    If in state 2, forget `v` and enter state 1.

Comment: Marcin

> I think it should be "forget k", v is unknown at this state.

      This is dodgy. It means the publisher failed to deliver before TTL.
    If in state 3, forget `k` and `v` and enter state 1.

Comment: Marcin

> We could also relay `k` not yet knowing `v` and then queue the requests for `v` until we know it. But this would complicate matters when we fail to get `v`, so it's better to start with the proposed machine.
> 
> There should be a fallback when the request for `v` fails, anyway. So maybe we should have three states for `v`: `unknown`, `failed` and `known`?


### Stretch: privacy considerations

There are known deanonymization schemes effective against cardano (and other
cryptocurrencies including bitcoin) which exploit predictable patterns of
broadcast. It's not a requirement for Shelley, but eventually we may want to
use a mitigating broadcast scheme. Such a solution could include bouncing
new transactions through other edge nodes, by way of an intermediate core node.

Comment: Marcin

> Do you foresee something like cardano on top of tor-like network?

## Implementation

### Request/response

  - Nothing new is needed. time-warp conversations can already do this, and
    already are being used to do this in cardano-sl.

  - What's required is to move these conversations into the diffusion layer.
    That will be a part of the work described [here](./Interface.md).

### Pub/sub and reusing existing implementation

The outbound queue in time-warp serves the same role that the pub half of
pub/sub will: getting data out to peers. It has some features we no longer need,
and is missing some that we will need.

  - It already has a notion of subscription, but it's topicless (or rather,
    there's one topic called "everything"), so we'll have to introduce topics.
    Each topic subscription is controlled by its own conversation, just as the
    "everything" subscription is at the moment.
    This way, the subscription protocol can remain mostly as is. The subscriber
    sends an empty subscribe message and periodically sends empty keepalive
    messages if the channel has been quiet for long enough.
    The publisher will send messages (start conversations) of a type determined
    by the topic, to every subscriber of the topic.

Comment: Duncan

> It's not obvious to me if we want to independently subscribe to each topic we are interested in, or to do one subscription that says in the opening message which topics we are interested in. We should consider both I think and pick the best.
> 
> Note also that we are not bound by the previous approach of having a subscription conversation that simply sits idle while publish events take place completely independently as separate individual conversations. If publish messages are small (individual, block headers, transactions, mpc etc) then its plausible to have these messages arrive as part of the subscription conversation itself. Indeed I think that would be the more traditional design. It's an approach we should consider.


  - It decides where to route messages according to a policy and peer
    classification (`NodeType`) based on a topology configuration.
    We'll get rid of this and replace it with a simple policy in which every
    message is delivered to every subscriber for the relevant topic.
    The `Peers` type will be simplified, since there's no longer a notion of
    alternatives (`EnqueueOne` versus `EnqueueAll`), and renamed to
    `Subscribers` to avoid confusion with the peer roster.

  - Subscription buckets won't be necessary. These are used to organize a
    set of mutable `Peers` values so that they can be modified concurrently
    by independent users. With pub/sub there will only be one `Peers`
    (`Subscribers`) value and one modifier: the subscription listener.

Comment: Duncan

> So if we want to have non-kademlia based links, e.g. I have a number of nodes that I want to talk to each other directly, how will I do it? I think the answer is easy actually: we tell nodes by static configuration to also subscribe to these other nodes. So then yes, on the side responding to subscribers there is still only one set of subscribers. It's just on the side deciding who to subscribe to that we can add in extra nodes we want to subscribe to.
> 
> So I think this is fine, but we should mention this, that we will need private peering arrangements, but this can work by subscribing to additional peers, and there's no need to make bucket distinctions when looking at the set of subscribers.

  - Rate limiting, precedence, max ahead will probably still be useful, both
    for publishing and for requesting.

  - Enqueue and dequeue policies aren't useful as is, because there is no
    `NodeType`.

Comment: Duncan

> Hmm, doesn't it just mean there's only one policy rather than lots? And actually we'll have to think about that carefully, whether we will still need some distinctions. For example a relay on the edge of the core DIF, will it not need to treat the code node it knows about differently from other nodes?

  - Failure policy isn't relevant for for pub/sub. If the peer fails then the
    subscription is gone and won't return unless the subscriber tries again.
    We'll probably want to keep the notion of a failure policy around, because
    it is relevant for request/reponse. However, it won't be a part of pub/sub,
    but rather the QoS/delta-Q system and peer roster, where failures will
    lower the estimate and eventually cause the peer to fall out of favour.

Comment: Duncan

> Hmm, I'll have to think about that. Not obvious to me yet.

  - The mechanism for enqueueing requests (conversations) can remain as is
    besides changing the form of enqueue and dequeue policies, and making sure
    to source the potential peers from the peer roster rather than from the set
    of current subscribers (at the moment these are easily confused in the
    buckets of `Peers`).

## Subscription conversation

There's one conversation per subscription. It's opened by the subscriber and
initially is subscribed to no topics.

### Subscriber sends

  - AddTopic (Topic msg) : subscribe to a topic.
  - RemoveTopic (Topic msg) : unsubscribe from a topic.
  - Request key : get the payload for a key.
  - Ignore key : signal that we don't want the payload for a key.
  - Keepalive : send after a certain amount of inactivity. Useful for
      detecting half-open TCP connections (intermittent lower-layer failures).

### Publisher sends

  - Announce key : indicate that a payload for this key is available.
  - Payload value : give the payload for a key.

Should the publisher acknowledge add/remove topic?
If a subscriber adds a topic and then removes it, there may be announces for
that topic inbound.

Comment: Marcin

> We could send them back with the ack message for remove, or if we don't want them, remove the subscriber only after the queue was cleared up to remove request. If we have all the requests in a FIFO this property will hold.

## Interface

```Haskell
data PubSub (topic :: * -> *) d

-- Every topic, with key and value types given.
data Topic k v where
  Block :: Topic BlockHeader Block
  Txp :: Topic TxId TxAux
  Mpc :: Topic StakeholderId MpcMsg
  ...

-- Subscriber can send values of this type.
data Subscription topic where
  AddTopic :: topic k v -> Subscription topic
  RemoveTopic :: topic k v -> Subscription topic
  Request :: topic k v -> k -> Subscription topic
  Ignore :: topic k v -> k -> Subscription topic
  Keepalive :: Subscription topic

-- Publisher can send values of this type.
data Publication topic where
  Announce :: topic k v -> k -> Publication topic
  Payload :: topic k v -> v -> Publication topic

-- Publish a message. Asynchronous. Returns right away.
-- The program which publishes is not informed of publication failure.
-- The pub/sub system can retry or drop according to some policy.
publish :: PubSub topic d -> Publication topic -> d ()

-- Does nothing if already subscribed and returns False.
-- The callback is a mechanism for backpressure. The dequeueing thread of the
-- pub/sub system will wait for the callback to finish before continuing.
subscribe :: PubSub topic d -> topic k v -> d Bool

-- Does nothing if not currently subscribed.
unsubscribe :: PubSub topic d -> topic k v -> d ()

-- What to do with a publication.
-- Announcements must indicate whether to request or ignore.
-- Payloads must be processed somehow.
data Next topic d = Next
  { announce :: NodeId -> topic k v -> k -> d Bool
  , payload :: NodeId -> topic k v -> v -> d ()
  }

-- Take the next publication (from any subscriber).
next :: PubSub topic d -> Next topic d -> d ()
```

## Questions, caveats, risks

### Publish over subscription channel

It would make a lot of sense to send the published data over the same
conversation which is used to establish the subscription, rather than (as we
do now) have one conversation control the lifetime of the connection, and
starting other conversations to publish each datum.

It's not clear whether this has any big advantages or disadvantages.

Decided: we'll use one time-warp conversation for the whole subscription.
The subscriber can add/remove topics, ACK published keys, and send keepalive
messages. The publisher can send keys and values.

### Changes to time-warp

The goal should be to do minimal changes to time-warp. It already enables
what we need to do, but not in the most efficient or straightforward way.

Comment: Duncan

> There will be a new requirement (eventually, not necessarily top priority) to be able to limit the bandwidth that all other peers can force this node into providing. Consider running a node at home without a firewall so that other nodes can contact you. That's fine so long as it does not consume all your home DSL upload bandwidth, otherwise you'd be annoyed. So we would need to be able to cap that. Timewarp does know (in principle) the amount of data sent to peers as responses to conversations, so it could in principle impose an aggregate bandwidth cap.

### Topic filtering

Should this be done at publisher or subscriber?
Doing it at the publisher will reduce network traffic but the publisher will
have to do more work. Doing it at the subscriber offloads the filtering work
to the subscriber. Filtering won't be expensive (a pattern match) so it's
probably best to do it at the publisher.

Decided: do it publisher side.

Comment: Duncan

> Agree. Lets do it at the publisher side. The most expensive resource is the network traffic.

### Dead subscriptions due to half-open connections

cardano-sl pull request
[#1852](https://github.com/input-output-hk/cardano-sl/pull/1852) was made to fix
[CSL-1676] and is relevant to pub/sub. We'll keep this solution or something
like it in order to detect dead subscriptions.

Comment: Duncan

> Yes we'll need something like this. It may become simpler by using the same subscription channel for the notification messages.

### Number of subscribers

Each subscription will be served by one TCP connection, so the OS imposes a
limit on the number of subscribers. It's plausible that even far below this
limit performance will start to suffer. But it's also plausible that the number
of subscriptions actually needed per node is low enough (not more than a
hundred) to avoid any such problems.
There's already a maximum subscribers configuration in the outbound queue that
can be reused.

This concern is misplaced because at this level we have no control over
accepting or rejecting TCP connections. This issue must be solved at the level
of network-transport(-tcp). There are very old issues proposing the ability to
drop or reject a "heavyweight connection" (TCP connection) but none have been
implemented:

https://github.com/haskell-distributed/network-transport/issues/25
https://github.com/haskell-distributed/network-transport/issues/17

The `ConcurrentMultiQueue` used now to enqueue conversations may not be
suitable for use with a large number of subscribers.

Comment: Duncan

> We may need something on the inbound side, at the network-transport-tcp level to reject incoming tcp connections, and/or at the time-warp layer to reject after doing the initial peer data handshake. It depends on what basis we need to reject.

> Obviously there's a cost per tcp connection (not just in the app but also the kernel) and rejecting at this stage is cheaper. We will probably need something simple like an absolute limit at the network-transport-tcp level, so that we simply do not accept more connections once we hit a limit. But if we also want to filter on the basis of info then we would need to accept, do the peer data handshake and then close the connection if we are choosing to reject that peer. That would be implemented at the time-warp layer, but we would need a network-transport extension to drop an existing heavyweight connection to a peer.

