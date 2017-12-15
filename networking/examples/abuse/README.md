# Backpressure and peer abuse example

## Overview

network-transport-tcp, as it's implemented in master and all releases, uses
an unbounded queue to gather data from its peers. For each socket, a thread will
read data from it, determine the appropriate network-transport `Event`, put
that `Event` into the unbounded queue, and then continue reading. These `Event`s
will come out of the queue when `receive :: EndPoint -> IO Event` is used.
Consequently, there is no relationship between the rate at which the receiver
processes these events (via `receive`) and the rate at which they are generated
by incoming data from peers. In fact, there's no way at all to control the rate
of incoming data; the program will always take it in as fast as possible,
because an unbounded queue can always take more data. 
That's to say, this system lacks *backpressure*: congestion at a server does
not eventually induce congestion at its client.

A proposed and accepted [change to network-transport-tcp](https://github.com/haskell-distributed/network-transport-tcp/pull/46)
allows for the user to specify a different queueing strategy. Perhaps the
simplest alternative to the unbounded queueing present in master is to swap
the `Chan Event` for an `MVar Event`, conceptually replacing an unbounded
queue with a bounded, single-place queue. Both of these strategies, known as
`QDisc`s (queueing disciplines) are included in that pull request, and they're
simple enough to repeat here:

```Haskell
simpleUnboundedQDisc :: IO (QDisc t)
simpleUnboundedQDisc = do
  unboundedQueue <- newChan
  return $ QDisc {
      qdiscDequeue = readChan unboundedQueue
    , qdiscEnqueue = \_ _ t -> writeChan eventChan t
    }

simpleOnePlaceQDisc :: IO (QDisc t)
simpleOnePlaceQDisc = do
  onePlaceQueue <- newEmptyMVar
  return $ QDisc {
      qdiscDequeue = takeMVar onePlaceQueue
    , qdiscEnqueue = \_ _ t -> putMVar onePlaceQueue t
    }
```

`qdiscDequeue` will be used by `receive` to get the next `Event`, whereas
`qdiscEnqueue` will be used by the thread which processes a socket to put
its `Event` into the queue. Crucially: if `qdiscEnqueue` blocks, then
socket reads _from that peer_ stop until it unblocks. The unbounded `QDisc`
never blocks on enqueue, and so socket reading never stops, whereas the bounded
`QDisc` *will* block and hold up the socket in case the program is not
dequeueing the `Event`s at a sufficiently high rate.

By choosing a `QDisc` which intelligently blocks on `qdiscEnqueue`, perhaps in
response to shared mutable state informed by various application-specific
metrics, the rate of input from peers can be controlled so as to eliminate
excessive load and to maximize quality of service for all peers.

## Example

Perhaps the best way to appreciate what's described above is to watch the
phenomenon as it happens. The program `examples/abuse/Main.hs` implements a
client and a server, along with live monitoring of the server's health:
its heap size and productivity (time spent doing non-garbage-collector work),
among other things. The client sends 16mb payloads to the server as fast
as possible, and the server receives them, calculates their length, and
maintains a tally of the total number of bytes received. Here's how you can run
it:

```bash
stack build
stack exec ghc examples/abuse/Main -- -threaded

# Start the server using an unbounded QDisc. -T is required for monitoring.
# -N1 is chosen to make the server respond rather slowly compared to the
# client's send rate.
./examples/abuse/Main server 7777 unbounded +RTS -N1 -T

# Open the EKG console.
<your_graphical_web_browser> localhost:8000

# Start the client. Choose -N so that on multicore systems it's given more
# capabilities than the server.
./examples/abuse/Main client 7777 7778 +RTS -N
```

Watch the EKG web dashboard. The heap residency will grow without bound
until your OS kills the server. Now try that again, replacing `unbounded` with
`one_place` in the server command. This time, the residency will remain under 
control, as the client's TCP send buffers fill up and sending slows down.
These results were gathered on an Intel i5 with 4 cores at 2.50GHz, 8Gb RAM.

## How to use this right now

### Get the right network-transport-tcp branch

The `QDisc` feature has not yet been merged into network-transport-tcp, so
use this branch (`stack.yaml` format)

```yaml
- location:
    git: https://github.com/avieth/network-transport-tcp
    commit: 53f7e303d22c1c771cad213ec8c9552112eae549
  extra-dep: true
```

Note: this branch also includes a commit from George Agapov to allow
specification of a bind address not necessarily equal to the end point address.
This was needed for AWS deployment, where apparently `0.0.0.0:0` must be chosen
as the bind address.

### Supply a QDisc to `createTransport`

The default `QDisc` is an unbounded queue. To choose the one-place variant,
create the transport like this:

```Haskell
let parameters = TCP.defaultTCPParameters {
          TCP.tcpQDisc = TCP.simpleOnePlaceQDisc
        }
in  TCP.createTransport bindAddress address port parameters
```

## Future work

The `simpleOnePlaceQDisc` is effective but indeed *simple*. Moving forward,
we'll want a more sophisticated `QDisc` capable of prioritizing traffic
according to various metrics. One idea is to deprioritize traffic from peers
which have an inordinate amount of in-flight data, effectively limiting the
amount of in-flight data per-peer. Such a `QDisc` will be deployed in the
same way: give it as a parameter to `createTransport`, and then give that
`Transport` to `node`. If the `QDisc` is controlled by mutable state, then
that state will be available, along with node- and application-specific metrics.

```Haskell
(qdisc, qdiscControls) <- someSohpisticatedQDisc ...
let parameters = TCP.defaultTCPParameters {
          TCP.tcpQDisc = qdisc
        }
Right transport <- TCP.createTransport bind address port parameters
node transport prng BinaryP peerData $ \node -> do
    ...
    pure $ NodeAction listeners $ \saction -> do
        ...
        enforceQOS qdiscControls (nodeStatistics node) ...
```

### Metrics and statistics

Currently the following metrics are gathered by a node (see the
`Statistics` type in `src/Node/Internal.hs`):

  - The number of connected peers.
  - The distribution of handler running time (mean, std. dev., number of samples).
    Separate metrics for those which finished normally, and those which
    finished exceptionally.
  - The number of running handlers induced by a peer.
  - The mean and variance of the number of running handlers induced by a peer.
  - The number of running handlers induced locally (bidirectional connections).
  - The mean and variance of the number of running handlers induced locally.

With plans to also track

  - For each peer, the total number of bytes read from that peer but not yet
    consumed by a handler (in-flight data).
  - Others? Please offer suggestions.

These are all made available to an application through

`nodeStatistics :: Node m -> m (Statistics m)`

as shown in the example above. Application-specific metrics can of course
be gathered by the application itself, and combined with these
application-agnostic metrics in order to inform a `QDisc` by way of some
shared mutable state.

Many, but not all, of the metrics described above are exported to the EKG
monitoring system. Those which are not exported simply do not fit the model
of EKG (they are _gauges_ but they are not integral).
