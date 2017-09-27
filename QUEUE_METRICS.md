# EKG metrics for the OutboundQueue

The outbound queue introduces a number of EKG metrics that can be used used to
monitor the queue's health. There are a number of guages showing current status
and a number of counters that keep track of error conditions.

## Guages

### Ongoing conversations (e.g. `queue.node0.InFlight`)

Records the total number of conversations that are currently on-going with
peers (aka numbers of messages in-flight).

### Failures (e.g., `queue.node0.Failures`)

Records the total number of nodes to which a recent conversation failed; we will
not attempt to enqueue messages to such nodes. What exactly constitutes a
"recent" failure depends on the failure policy; currently it defaults to 10
slots (200 seconds). The recent failures statistics are cleared on SIGHUP.

### Scheduled (e.g., `queue.node0.Scheduled`)

Records the total number of conversations that have been scheduled (and
enqueued to specific peers), but not yet started.

### Known peers (e.g., `queue.node0.bucket.BucketStatic`)

The outbound queue keeps a number of buckets of peers it knows about.

* `BucketStatic`: statically known peers
  (from a `topology.yaml` file; core and relay nodes only).
  This should only change on `SIGHUP` and remain static otherwise.

* `BucketSubscriptionListener`: subscribers
  (peers that subscribed to this node by sending a `MsgSubscribe` message).
  A high number indicates a (relay) node under heavy load.

* `BucketBehindNatWorker`: relay nodes discovered through DNS query
  (used by behind-NAT edge nodes to declare relay nodes as known peers).
  This should be equal to the number of relay nodes returned by DNS
  (through a series of DNS queries to the various hostnames).

* `BucketKademliaWorker`: peers discovered through the Kademlia network
  (used in P2P mode).
  This number should remain more or less constant (configurable through the
  `topology.yaml` file), unless there are not enough Kademlia peers to discover.

For each of these buckets we counts the number of distinct known peers (i.e.,
ignoring routing information).

**Note on directionality**: The outbound queue deals with outgoing messages
only. Thus, the situation looks like this for behind-NAT nodes:

```
  /-------------------------\           /------------------------------\
  |                         |           |                              |
  |  BEHIND-NAT NODE        |<----------+--BucketSubscriptionListener  |
  |                         |           |                              |
  |  BucketBehindNatWorker--+---------->|  RELAY NODE                  |
  |                         |           |                              |
  \-------------------------/           \------------------------------/   
```

The behind-NAT node discovers the relay node through a DNS request and adds the
relay node to its `BucketBehindNatWorker`, so that it can send messages _to_ the
relay node. It then sends `MsgSubscribe` to the relay node, so that the relay
node can add the edge node to _its_ `BucketSubscriptionListener`, ensuring that
the behind-NAT node will receive messages _from_ the relay node.

In principle we could do something similar for P2P, but in this case we do not
use subscription. When a P2P node discovers peers through Kademlia, it adds them
to its `BucketKademliaWorker`, but instead of subscription it relies on Kademlia
to notify other peers that they have a new neighbour; hence, the picture looks
like

```
  /------------------------\           /------------------------\
  |                        |           |                        |
  |  NODE 1                |<----------+--BucketKademliaWorker  |
  |                        |           |                        |
  |  BucketKademliaWorker--+---------->|  NODE 2                |
  |                        |           |                        |
  \------------------------/           \------------------------/   
```

instead.

## Problems during enqueing

### Failed "enqueue-all" instructions (e.g., `queue.node0.FailedEnqueueAll`)

The queue is configured with detailed routing information, which minimizes the
risk of network fragmentation. Normally when a message gets enqueued, this
routing information (along with information about which nodes had recent
failures) dictates to which set of nodes the message gets enqueued to; we
call this an "enqueue-all instruction" (an instruction to enqueue it to all
nodes dictated by the routing configuration).

An enqueue-all instruction is considered to have failed when we could not find
_any_ node at all to enqueue the message to. In this case the message is lost.

(Enqueuing the message does not of course imply that the message will also be
successfully sent.)

### Failed "enqueue-one" instructions (e.g., `queue.node0.FailedEnqueueOne`)

Occassionally the queue is provided with a message that should be sent to one
of a specific set of nodes. Typically this happens when we learn that a certain
set of nodes has some information (for instance, a particular block) that
we would like to have; the queue will then enqueue the message to _one_ node
in this set; we call this an "enqueue-one instruction".

As for enqueue-all instructions, an enqueue-one instruction is considered to
have failed when we could not find _any_ node to enqueue the message to (that
is, any node in the specified set with no recent failures). In this case the
message is lost.

### Failure to choose an alternative (e.g., `queue.node0.FailedChooseAlt`)

This is related to `FailedEnqueueAll`, but is a less serious indicator: when we
enqueue a message, the routing information says "for each of these _n_ sets,
pick an node from that set to enqueue the message to". When we fail to pick
_any_ such node at all, we record the enqueue as having failed, but for each set
individually we increment the `FailedChooseAlt` when we fail to choose an
alternative from that set. In other words, we would only increment the
`FailedEnqueueAll` counter after having increment the `FailedChooseAlt` counter
by 1.

Failure to choose an alternative increases the chances of network partitioning,
but the message is not (necessarily) lost.

Side note: for enqueue-one instructions we don't separately warn about failure
to choose an alternative as in this case `n=1` and hence failure to choose
an alternative implies failure to enqueue.

## Problems during sending

At some point after enqueuing the queue will attempt to actually send the
message. As discussed above, when the message sits in the queue, it will be
reflected in the `Scheduled` gauge, and when the conversation starts, this is
reflected in the `InFlight` gauge.

When a message fails to send, the `FailedSend` counter is incremented. This does
not necessarily mean the message is lost, as it might still be sent to other
peers; if we fail to send the message to _all_ peers it was enqueued to, we
increment the `FailedAllSends` counter. This is the analogue of the
`FailedEnqueueAll`/`FailedChooseAlt` split during enqueueing. Unfortunately,
`FailedAllSends` will only ever be incremented when using the synchronous API
(where we enqueue a message and then wait for it to have been sent), as there is
no natural point to check whether _all_ sends failed otherwise. From a dev-ops
perspective, this means that `FailedAllSends` may be  an under-approximation,
and hence `FailedSend` is somewhat more important a measure than
`FailedChooseAlt`.

Finally, the queue offers a "cherished" send API. This is a wrapper around the
synchronous send API which attempts to re-enqueue a message if it failed to
send. Normally it will be enqueued to a different destination as the previous
one will now be recorded as having a recent failure, reflected in the
`Failures`) guage. Under very rare circumstances this could loop indefinitely;
when we detect such a loop the `FailedCherishLoop` counter. Any value above zero
for this counter indicates queue misconfiguration.

## Problems during subscription
## (e.g., `queue.node0.FailedBucketFull.BucketSubscriptionListener`)

The various buckets maintained by the queue can have a maximum size imposed
on them. When an attempt is based to grow a bucket past its maximum size,
the change is rejected and the corresponding counter is incremented.
