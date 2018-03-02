# QoS / delta-Q estimation

Peer discovery gives us a pool of peers but no information about link quality.
To inform a choice of peers to actually communicate with, we'll need to gather
data for an estimation of the delta-Q at a given peer.

The esimate must be exposed (read-only) to the user of network-transport-tcp.
It's not determined what exactly that data should be. The distribution itself?
The s, g, and v components?

Comment: Duncan

> To check with Neil. I'd guess the latter is most useful.

```Haskell
-- Send some bytes and enclose it in some measure control codes, updating
-- some state when the measurement is done.
-- The diffusion layer will do this periodically as it sees fit.
sendAndMeasure :: Connection -> [ByteString] -> IO ()

-- Measurements are valid for the whole transport.
-- The EndPointAddress identifies the peer, but the EndPointId included in
-- there is irrelevant: a single socket at the remote transport serves all of
-- its endpoints, so we would expect getMeasurements to be the same for two
-- addresses which differ only in their EndPointId.
getMeasurements :: Transport -> EndPointAddress -> IO DeltaQStuff
```

## Implementation

Must be implemented close to the socket itself, i.e. in network-transport-tcp.
It will rely on precise time measurements on windows of data. New control
codes would be defined in order to delimit these windows, i.e. indicate a
leading and trailing edge. This allows for an estimation of the s component
of delta-Q (latency as a function of the amount of data sent).

Comment: Duncan

> We already preserve message boundaries on lightweight channels.

The `sendAndMeasure` function would send a control code immediately before and
immediately after the data itself (leading edge and trailing edge), recording
timestamps for each, and the peer would respond with another control code as
soon as it receives each edge, sending its observed timestamps along.

The sender would know:

  - When leading edge was sent
  - When trailing edge was sent
  - When leading edge was received remotely (different clock, possibly lying)
  - When trailing edge was received remotely (different clock, possibly lying)
  - When leading edge was received locally
  - When trailing edge was received locally

To get a precise timestamp for a receive, we can use the SIOCGSTAMP ioctl.
This is the timestamp of the last packet delivered to userspace. We would call
it after receiving a leading or trailing edge, and before doing the next
`recv`.

Comment: Duncan

> It may be tricky to work out how these match up with the message boundaries. We'll have to read the docs on what this ioclt means exactly for tcp connections.

Must discuss further with Neil and Peter, and understand the documents they've
provided.

## Cross-platform support

Must research the socket APIs of all supported platforms and ensure that
they can provide enough support for our implementation.
The SIOCGSTAMP ioctl available in linux, for instance, may not have an
equivalent in Windows.

As far as I can tell Winsock does not offer a way to get the timestamp of the
last received packet, as SIOCGSTAMP does. But Winsock *does* have a call to
get the TCP round-trip-time estimate. Are we interested in that?

## Relationship to input QDisc

network-transport-tcp endpoints may have nontrivial queueing policies. Should
latency induced by queueing factor into the delta-Q estimate? That's to say,
are we trying to estimate only TCP round-trip-time, or network-transport-tcp
round-trip-time? Neither option poses any extra challenge, it's only a matter
of deciding when to respond to the control message: before or after the
enqueue.

Comment: Duncan

> Good Q. Another point to discuss with Neil.

## Egress queueing

With a delta-Q estimate in hand, it may be useful to have egress queueing
in order to limit the send rate over a given socket, as rate limiting in this
way may improve performance.

## Gathering delta-Q data during discovery

We may want to make estimates at peer discovery time, over the traffic
produced by that subsystem. This may or may not be possible, as it runs over
UDP rather than TCP, which may not have the required cross-platform timer
support.
