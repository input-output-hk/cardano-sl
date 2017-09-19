# Cardano networking policy for initial mainnet launch

We have three classifications of nodes:

  * Core
     * can become slot leader
     * never create currency transactions

  * Relay
     * cannot become slot leader
     * never create currency transactions
     * can communicate with core nodes

  * Edge:
     * cannot become slot leader
     * creates currency transactions,
     * cannot communicate with core nodes
     * may or may not be behind NAT/firewalls
     * three sub-types:
       * ordinary end user nodes behind NAT/firewall
       * ordinary end user nodes without NAT/firewall using p2p
       * exchange nodes (special users that run currency exchanges)

For the initial mainnet launch the core and relay nodes are fully under the
control of the IOHK devops team. These nodes are hosted in AWS.

The purpose of the core/relay distinction is to enable some degree of DoS
protection and to enable the system to scale to larger numbers of users
behind NAT.

The DoS protection idea is that core nodes are not directly accessible from
the public internet and so cannot be directly attacked. The relay nodes serve
as a kind of proxy. The relay nodes can be attacked but they are in principle
stateless and can be moved or their number increased. If relays are taken down,
this does deny service, but the integrity of the core nodes and the blockchain
should not be compromised.

The other purpose of relay nodes is to scale to larger numbers of edge nodes
that are behind NAT. In this design, edge nodes behind NAT do not talk to
other edge nodes, they must all talk to relay nodes. This means we need enough
capacity within the relay nodes to support all users. This is assumed to
require more nodes than we would want to have acting as core nodes. Current
benchmarks indicate that we cannot have more than around 20 core nodes, but
we assume that we need more than that number of nodes to support all the edge
nodes. Relays fill that gap. They forward messages between core nodes and edge
nodes.


Given the core/relay split, the combinations of node interactions are:

Core -> Core
Core -> Relay

Relay -> Core
Relay -> Relay
Relay -> Edge

Edge -> Relay

We have a few classifications of message/conversation:
  * transaction
  * mpc
  * block header announcement
  * block header requests
  * block requests


## Unmet requirements

These requirements on the network layer are, by deliberate choice, not met
in the current policy for the initial mainnet deployment:

 * Sending transactions to edge nodes. Doing so massively increases the number
   of conversations between relays and edge nodes (because each one is sent as
   a separate conversation) and the concern is that this will exceed the
   capacity of the relay nodes.

 * Sending confirmations for lightweight delegation certificates to the edge
   node that submitted it. We have no facility to send a confirmation to an
   individual edge node. So the only way to make this work would be to
   broadcast the confirmations to all edge nodes, which we want to avoid for
   the same reason we would like to avoid sending transactions to edge nodes.

Neither of these IERs are critical. Their lack just results in degraded
functionality. Notably, transaction recipients will not get notified of
the transaction until it turns up in a block. This is judged to be tolerable
for the initial deployment as the time to inclusion in a block should be
20-40 seconds.


## Outbound conversations

The sending mechanism works at the level of conversations (not messages) and
only for conversations that are initiated in the outbound direction. The
mechanisms and policies described here do not apply to conversation handlers
/ listeners.


## Known peers

The sending mechanism maintains a set of known peers. This set includes their
network address and their node classification (ie core/relay/edge). When
enqueuing a conversation the policy determines which of these known peers are
the target of the conversation(s). In some cases (detailed below) the policy
involves selecting from a subset of the known peers, and that subset is
supplied as part of the enqueuing.

The set of known peers is based on combinations the following:
  * statically known peers based on initial configuration
  * peers discovered by DNS
  * peers discovered by kademlia
  * currently subscribed peers

The particular combination depends on the overall networking arrangement and
individual node configuration.


## Subscriptions, nodes behind NAT and P2P nodes using kademlia

In cardano up to and including testnet 5, wallet nodes would use kademlia to
determine a selection of peers to send messages to. For example that means they
would send block announcements and transactions to all those peers.

Wallet nodes would also poll peers every few seconds to retrieve block headers
and transactions. This was to cope with the situation that kademlia finds no
peers due to it being behind NAT and thus other nodes not being able to
initiate conversations to it.

In testnet 6 and mainnet the design is different, as follows:

There is a concept of subscriptions. This is where a node initiates an outbound
conversation to a known peer and the protocol of the conversation is to keep
the conversation open until the underlying network connection fails. While this
conversation is open, the target peer adds the initiating node as a known peer.
This means that the known peer will be sent updates such as block announcements
and transactions (subject to policy). When the conversation closes then the
peer is removed as a known peer.

There is also a transitional mode (described below) that works in a very 
similar way to the testnet5 system.


## Known peers vs subscriptions

Known peers are only used for determining which nodes outbound conversations
go to. So nodes can receive connections for conversations from nodes that are
not in the known peers set.

From the point of view of one node, all other nodes that have subscribed to it
*must* be in this node's set of known peers. So there is a subset
relationship there.

From the point of view of one node, all nodes that it has subscribed to *must*
also be in its set of known peers (either as a primary or an alternative). So
there is a subset relationship there.

There can certainly be more nodes in the known peers set than a node has
subscribed to. This happens in several cases: nodes with statically known
peers that do not subscribe to any nodes; a normal edge node that only
subscribes to one other node at once but maintains a larger set of known
nodes for outbound conversations. The rationale here is that the fallover
detection for subscriptions is not the same as that for outbound conversations
so we may detect problems on outbound traffic which are not detected promptly
for subscriptions. Thus it is useful to have a set of alternatives for
outbound conversations.


## Enqueuing to specific known or unknown peers

As mentioned above, there are cases where the application layer logic dictates
that we must enqueue a conversation to one of a specific set of peers.

In this case we need to be able to establish the type of each peer. When the
peer appears in the set of known peers then this is easy, however this does
not cover all cases.

In particular for core and relay nodes with static routing if the routing
tables are asymmetric then while A may have B in its known peer set, the
reverse may not be true. This means that if A sends B a header announcement
then B will want to respond with a request for the block and it is this
response where B needs to know the type of the peer A.

Similar cases occur in the p2p case where A might know about B and C but only
subscribe to B, which means that only B is guaranteed to know about A, while C
might not.

In each situation there are fallback methods for classifying the peer type.
The exact method is described in the section below as part of the various
configurations.


## Scaling relays: privileged and unprivileged relays

Relays serve two purposes:

 1. to act as the public proxy for core nodes that are otherwise hidden from
    the public network and support connectivity of the core network; and
 2. to support large numbers of edge node subscribers behind NAT

For the first purpose, only a small number are needed but for the second we
may require a large number, and a number that will vary with the load.

We choose to divide relays into two kinds, one serving the first purpose and
others the second:

 1. The first class, the "privileged" relays are the ones that will use static
    routing and the core nodes will know about them and communicate with them
    directly.

 2. The second class, the "unprivileged" relays will only talk to other
    privileged relays and to edge node subscribers, and not directly to core
    nodes.

To make the unprivileged relays scalable, such that we can easily add or remove
them without making other changes, the privileged relays need to not know about
them statically, so that static configuration does not need to be updated. So
unprivileged relays will statically know about privileged ones, but not the
other way around and unprivileged relays will dynamically subscribe to
privileged relays, using the normal subscription mechanism (using simple DNS
based discovery).

In principle a group of unprivileged relays could be made to scale
automatically based on their load. The obvious load metric is the number of
subscribers on each relay.

Thus relays can be configured in two ways: to use static routing, or to
subscribe to one or more other relays. Unprivileged relays should subscribe
to two or three privileged relays to maintains redundancy, and have multiple
other fallbacks.

Only unprivileged relays should be listed in the public DNS for edge nodes,
so that privileged relays are not overloaded. If this proves insufficient
then it may be helpful to also firewall the privileged relays.

It is worth noting that (as described below) relays consider all subscribers
to be edge nodes, which means that privileged relays will consider their
unprivileged relay subscribers to be edge nodes. This is odd at first glance
the policy works fine. Study the Relay -> Relay and Edge -> Relay enqueuing
policies below to convince yourself. The intuitive argument is that a
privileged relay need not tell the difference between an individual edge node
and an unprivileged relay that is proxying for many other edge nodes. Only the
privileged relays need to send transactions to each other to improve
connectivity of the core network, unprivileged relays do not need to do this
as their only purpose is to scale the number of edge nodes that can be
supported.


## Configuring network nodes

At a global network level there are two main arrangements: transitional and
the mainnet production deployment. The transitional arrangement closely
emulates the testnet5 arrangement whereas the other is used for testnet6
and mainnet.

Within the mainnet arrangement there are different configurations for core,
relay and edge nodes, including the variety of edge node configurations.

All of the arrangements use the same node classifications (core, relay, edge)
and broadly the same outbound policy, with some minor differences for
different varieties of edge nodes.

The configuration is done via a combination of config files and command line
arguments.

In the transitional arrangement all nodes:

  * Consider themselves to be core nodes
  * Join the kademlia network
  * Run a worker to discover peers via kademlia and adds a specific number of
    them as known peers, with any others as backups. A typical number would be
    3 but the exact number is configurable.
  * Do not (or at least need not) listen for subscribers.
  * Unknown peer fallback type: Core.

This arrangement uses kademlia to establish a push style communication network
much like testnet5. It does not use pull-style subscriptions.

In the mainnet arrangement the configuration is different for different nodes.

In the mainnet arrangement, core and relay nodes use config files:

  * Nodes share a static configuration file that specifies the config for each
    node identified by name.
  * Nodes are started with a command line flag to indicate which named node
    they are.
  * The configuration for a node includes:
    * the node name (which is assumed to be resolvable by the network layer)
    * the node type: core or relay
    * if it should run kademlia or not
    * if it should be listed in public DNS for edge nodes or not
    * static routing tables compatible with the routing policy described
      below. This includes the name statically known peers (from which their
      type can be inferred).
    * or as an alternative to static routing tables, a set of DNS names or IP
      addresses for peers to subscribe to
    * port, address, host
    * region

The "static" configuration need not in fact be completely static. During a
redeployment or whenreconfiguring the network there will be times when not all
nodes share the same configuration. Indeed it is useful to be able to alter the
configuration and reload without having to restart processes. So the "static"
configuration is in fact merely slowly changing.

Then based on the config file, their network configuration is such that they:

  * Consider themselves to be of the type indicated in the config
  * Join the kademlia network iff the config indicates it should
  * If specified in the config, use statically known peers (core and/or relay)
  * If specified in the config, use DNS to discover nodes and adds them as
    known peers considering them as relay nodes.
  * If specified in the config, use the same DNS discovery to select N relays
    to subscribe to, with other relays as backup choices.
  * Relay nodes allow subscribers and consider those subscribers to be
    edge nodes
  * Unknown peer fallback type: use the entire static configuration to give an
    additional mapping from node address to node type. Final fallback is Relay.

Note that running kademlia makes sense here even though it is not used
directly by these nodes. It is to seed the kademlia network for other users.

Note that privileged relays will use configuration for static routing, while
unprivileged relays will use configuration for dynamic subscription.

At this stage it is worth recalling the kinds of edge nodes:

  * ordinary end user nodes behind NAT/firewall
  * ordinary end user nodes without NAT/firewall using p2p
  * exchange nodes (special users that run currency exchanges)

In the mainnet configuration for ordinary end user nodes behind NAT/firewall:

  * The node considers itself to be an edge node
  * Does not join the kademlia network
  * Uses DNS to discover nodes and adds them as known peers considering them
    as relay nodes. To be compatible with the routing policy below this will
    be a single list of alternatives.
  * Uses the same DNS discovery to select one relay to subscribe to, with
    other relays as backup choices.
  * Does not allow subscribers
  * Unknown peer fallback type: Relay.

In the mainnet configuration for ordinary end user nodes using p2p:

  * The node considers itself to be an edge node
  * Joins the kademlia network
  * Uses kademlia to discover nodes and adds them as known peers considering
    them as relay nodes. To be compatible with the routing policy below this
    will be a three lists of alternatives.
  * Uses the same kademlia discovery to select three relays to subscribe to,
    with other relays as backup choices.
  * Allows subscribers and considers those subscribers to be relay nodes.
  * Unknown peer fallback type: Relay.

TODO: This arrangement will result, in many cases, with a node knowing about
another peer by two different methods and this needs to be handled sanely.
In particular we can add a node as a known peer because kademlia selects it
as a preferred peer, but that same node can also subscribe to us.


In the mainnet configuration for exchange nodes:

  * The node considers itself to be an edge node
  * Does not join the kademlia network
  * Uses DNS to discover nodes and adds them as known peers considering them
    as relay nodes. To be compatible with the routing policy below this will
    be three lists of alternatives.
  * Uses the same DNS discovery to select three relays to subscribe to, with
    other relays as backup choices.
  * Unknown peer fallback type: Relay.

Finally, there is a network configuration for test/development tools that
are used to query or prod a running cluster (e.g. to submit transactions).

The "auxx" configuration:

  * Node is an edge node
  * Does not join the kademlia network
  * Does not do relay discovery (ie does not use DNS or kademlia)
  * Uses static configuration of known peers. All such peers are
    considered as relays.
  * Unknown peer fallback type: N/A


## Discovering peers using DNS

Whether a node uses this mechanism depends on its configuration as described
above. When this mechanism is active, a node will use DNS to look up a set of
nodes to use as known peers and as a basis for subscribing.

The mechanism is configured with a list of alternatives which are themselves
lists of DNS names. The purpose of the inner lists is that a result list of
network addresses is collected by concatenating the results of a number of
DNS lookups. It is concatenation because DNS lookups are allowed to return
multiple results and the order of the DNS names is significant. The fact that
order is significant allows configuration where a DNS name for some relays are
preferred over other alternative. For example this may be able to take
advantage of DNS-based geo location or load balancing.

The purpose of multiple alternative combinations is to avoid the DNS servers
being a single point of failure by allowing for multiple such sets.

Having discovered peers, these are used for updating the nodes set of known
peers and to subscribe to, according to the configuration described above.


## Discovering peers using Kademlia

Whether a node uses this mechanism depends on its configuration as described
below. When this mechanism is active, a node will use kademlia to discover a
set of nodes to use as known peers and as a basis for subscribing.

The mechanism relies on the node running kademlia and having joined the
kademlia network. The local kademlia state is monitored to notice when
the set of nodes that kademlia knows about changes.

The current kademlia state is used to select some set of address to be the
node's known peers. It is also used to select some set of address to subscribe
to.

The address to subscribe to should be somewhat or fully "sticky" so that
subscriptions are not changed too frequently.

Note that even in P2P kademlia mode, this mechanism is different than what was
used previously. In the testnet 5 and older approach, nodes using kademlia
relied on the assumption that they would be a selected node of at least one
peer. This assumption was inherent in the design that each node would decide
based on its own kademlia view which peers to push updates to. And kademlia
prefers nodes that have been alive and reliable for long periods. This will
tend to exclude new nodes and nodes that are frequently offline. Consider
the initial situation where we have N relay nodes that are using kademlia and
the firs P2P peer joins the kademlia network. There is a significant chance
that no existing relay node will consider this new node to be a preferred
node, since each of the existing relay nodes using kademlia will have each
other as their most preferred nodes. In the new approach, the new P2P node
will determine on its own the peers it wishes to talk to, and then establishes
a subscription to as subset of them. Thus the new P2P node will, subject to
network and resource availability, always be able to receive updates, and to
do so promptly upon joining the kademlia network.


## Outbound policy descriptions

For each source node we provide two policy tables: the enqueuing policy and
the dequeueing policy.

The enqueuing policy consists of an unordered set of entries.

Each entry in the enqueuing policy has a template like:

  * {msg type} to {peer type}, {precedence}, {routing style}, {fallback}
  * {msg type} to {peer type} not sent!

With the following interpretation:

  * {msg type} is block header, mpc message or transaction

  * {peer type} is Core, Relay or Edge (for the possible connectivity)

  * "not sent!" means this message type is not sent to these peer types at all.

  * {precedence} is a precedence level P1..P5 where P1 is the highest
    precedence.

  * {routing style} one of

     * "to all" which means send to all known peers of the appropriate type

     * "{N} route lists max ahead {M}" which is a policy where there are {N}
       lists of peers and we send to the first peer in each list that is
       considered not busy.

     * "to one of" which means send to one of the peers supplied in the
       enqueue call (these peers must be a subset of the known peers).
       In this case the policy's peer type can be a list, and peers are
       chosen based on these peer types and in the order that they appear.
       For example, Relay+Core means that out of the supplied set of peers
       we pick one that is a relay (and available) and otherwise select one
       that is a core node.

    A peer is considered busy if the number of in-flight plus outstanding
    messages with equal or higher precedence is equal or greater than M.

    For example "3 route lists, max ahead 1" means we have 3 lists of peers.
    In the ideal case we send to the 3 peers appearing at the head of the 3
    lists. However, for each list, if the first is busy then we try to send to
    the next, and so on, picking the first non-busy peer. With "max ahead 1"
    this means that if there is even a single message in-flight or waiting to
    be sent, that is of an equal or higher precedence than the message in
    question, then we consider the node to be busy.

  * {fallback} this describes what to do when all peers in a list are busy.

    The drop policy means the message is dropped rather than enqueued to a
    peer.


When forwarding a message received from another peer, irrespective of the
peers selected by the policy, the message will not be sent back to the peer
that sent it to us, as that would be redundant.


The dequeueing policy has an entry peer peer type

 * to each {peer type} peer, concurrency {N}, {rate limiting}

The concurrency level specifies the maximum number of concurrent in-flight
messages to a single peer.

The rate limiting policy specifies how many messages per second of all types
may be sent to a single peer (or inversely, how long to wait between sends).

These policies apply per peer (across message types and precedence levels).


## Core -> Core, Core -> Relay

Enqueuing:
 * header announce: to Core,  P1, 3 route lists, max ahead 0, fallback drop
 * header announce: to Relay, P2, 2 route lists, max ahead 0, fallback drop
 * header request : to Core,  P2, 3 route lists, max ahead 2, fallback drop
 * header request : to Relay, P2, 2 route lists, max ahead 2, fallback drop
 * block  request : to Relay+Core, P2, one of,   max ahead 3, fallback drop
 * mpc            : to Core,  P3, 3 route lists, max ahead 2,  fallback drop
 * transaction    : to Core,  P4, 3 route lists, max ahead 20, fallback drop
 * mpc            : to Relay not sent!
 * transaction    : to Relay not sent!

Dequeueing:
 * to each Core peer, concurrency 3, no rate limiting
 * to each Relay peer, concurrency 2, no rate limiting

There is an important assumption here: that the lists of alternative routes
is long enough (e.g. 3) so that if all of them are congested or unavailable
that we are already in deep trouble and no further fallback polcy is useful.

In principle if further fallback were useful then for block headers we
would want to enqueue to some random peer and if necessary drop the oldest
message, while for transactions we would drop the newest. So in both cases
our policy of just dropping is ok: it's the right thing for transactions
while for block headers and MPC messages we can rely on the "already broken"
assumption.


## Relay -> Core, Relay -> Relay, Relay -> Edge

Enqueuing:
 * header announce: to Relay, P1, 3 route lists, max ahead 0, fallback drop
 * header announce: to Core,  P2, 2 route lists, max ahead 0, fallback drop
 * header announce: to Edge,  P3, to all,        max ahead 0, fallback drop
 * header request : to Relay, P2, 3 route lists, max ahead 2, fallback drop
 * header request : to Core,  P2, 2 route lists, max ahead 2, fallback drop
 * header request : to Edge not sent!
 * block  request : to Relay+Core, P2, one of,   max ahead 3, fallback drop
 * block  request : to Edge not sent!
 * transaction    : to Core,  P4, 2 route lists max ahead 20, fallback drop
 * transaction    : to Relay, P4, 3 route lists max ahead 20, fallback drop
 * transaction    : to Edge  not sent!
 * mpc            : to Core  not sent!
 * mpc            : to Relay not sent!
 * mpc            : to Edge  not sent!

Dequeueing:
 * to each Core peer,  concurrency 2, rate limit 1 msg/sec
 * to each Relay peer, concurrency 2, rate limit 3 msg/sec
 * to each Edge peer,  concurrency 2, rate limit 1 msg/sec


TODO: there is work to do to determine experimentally how the rate limit
from relay to core nodes determines the aggregate number of transactions
per second that reach the core nodes, since ultimately this is what must
be kept within the capacity of the core nodes.

There is a risk of cascading failure in the situation where a set of nearby
core nodes are heavily loaded and one node gets behind and goes into recovery
mode. If this node starts requesting headers and blocks from its peers then by
placing more load on them it may push them into recovery mode too. To try to
avoid this failure case the intention is to request blocks from one of a set of
known peers and for the policy to pick a relay node over a core node in that
set if possible.


## Edge -> Relay

For ordinary end user nodes behind NAT/firewall

Enqueuing:
 * header announce : to Relay not sent!
 * header request  : to Relay, P2, 1 route list, max ahead 1, fallback drop
 * block  request  : to Relay, P2, one of,       max ahead 2, fallback drop
 * transaction     : to Relay, P4, 1 route list, max ahead 2, fallback drop
 * mpc             : to Relay not sent!

Dequeueing:
 * to each Relay peer, concurrency 2, rate limit 1 msg/sec


For exchange nodes:

Enqueuing:
 * header announce : to Relay not sent!
 * header request  : to Relay, P2, 3 route list, max ahead 0, fallback drop
 * block  request  : to Relay, P2, one of,       max ahead 0, fallback drop
 * transaction     : to Relay, P4, 3 route list, max ahead 6, fallback drop
 * mpc             : to Relay not sent!

Dequeueing:
 * to each Relay peer, concurrency 3, rate limit 5 msg/sec


For ordinary end user nodes using p2p

Enqueuing:
 * header announce : to Relay, P1, 3 route list, max ahead 0, fallback drop
 * header request  : to Relay, P2, 3 route list, max ahead 2, fallback drop
 * block  request  : to Relay, P2, one of,       max ahead 3, fallback drop
 * transaction     : to Relay, P4, 3 route list, max ahead 3, fallback drop
 * mpc             : to Relay not sent!

Dequeueing:
 * to each Relay peer, concurrency 2, rate limit 1 msg/sec


In addition to the enqueuing policy for transactions from edge nodes, emitting
transactions from edge nodes will use a synchronous API with a retry policy:
if after dequeuing the conversation fails, the conversation will be
re-enqueued. Only if all alternative relays are unavailable or at maximum 4
retries will the send be deemed to have failed.


## EKG monitoring measures

Each node will have the following EKG metrics associated with the networking
layer:

 * Current number of total known peers and current number of subscribers.
   These are an important metric for relay nodes because it is an indicator of
   the network resource load imposed by having lots of edge nodes subscribing.
   For core nodes the total number of known peers should remain static in a
   properly configured system.

 * Event counter for the event of a subscription being rejected due to the
   subscriber limit being reached. If we get lots of these in a short amount
   of time, it is likely to be an indication of a high number of users or
   possibly a DoS attacker trying to consume all the subscriber resources.

 * Event counter for the event of a specific node to send to not being in
   the known peers set. These events are expected while reconfiguring the
   network but otherwise it probably indicates a network misconfiguration.

 * Event counter for various enqueue & dequeue failure events:
    * Failure to enqueue to all peers in a single list of alternatives.
    * Failure to enqueue to all peers in all alternatives, i.e. total enqueue
      failure.
    * Failure of a conversation (separately for core,relay,edge)
   Each of these events should use separate counters for core, relay & edge.

 * Current number of peers where the outbound queue currently classifies them
   as failed. Again, broken down by peer type.

