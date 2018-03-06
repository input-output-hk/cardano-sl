# Peer discovery

Existing Kademlia solution can be altered and replaced without disrupting
cardano-sl as it stands today.

Andrzej will look into the original froozen/kademlia, and serokell/kademlia
fork, and come up with an implementation.

Comment: Duncan

> This is a design doc, not a planning one, so we don't need to say who is doing what. Imagine someone reading this in 6 months time, trying to get up to speed with the design of the network layer.

## Required feature: discovery without join.

A peer which is not publicly addressable (behind NAT or firewall) must be able to
discover peers using the typical method, without tainting the network:
its observed IP address should not be treated as the address of a reachable
peer (recorded in the index), as it is in the current implementation.
In fact we can settle for a weaker guarantee: an unreachable address will
never evict a reachable address from an index, and will never prevent a
reachable address from being included in the index.

~~Our original 3-way handshake design gets us nowhere.
The proposal was to try to send to the joiner's host at a different (known,
fixed) port, and include the joiner's address in the index only if the joiner
can show that it received the sent data on that port.
It's feasible and not difficult to arrange for the peer's send to go through,
even behind common NAT setups. The callback port is known, so the initiator
can simply send some data to the peer on a UDP socket bound on that port
before joining. Typical UDP hole punching NATs will admit the server's
response.~~

Comment: Duncan

> I think it's not necessary to say what our "original" design idea was here, or at least not necessary to say it was the original design. We can present two options and explain why we pick one, or just present our chosen one and justify the design.

~~Since it's so easy to lie, we may as well take the joiner's word for it, and
use a simpler join/discover protocol instead.
The initiator claims whether it's addressable or not, and the server belives
it. A malicious actor can't do any damage by lying. At worst, it will evict
a node from the peer's index, but only if that node is judged to be dead.
If it enters the index, it will never prevent a live node from entering it,
because it will be judged to be dead (won't respond to a ping).
With this solution a connector can choose the port on which it listens,
whereas in the 3-way handshake we'd need a known fixed port.~~

We'll use a 3-way handshake, in which the joining peer gives a port.
That peer will try to reach the joining peer over UDP at that port, sending a
nonce. If the initial peer receives the response, then it can assume that it
is reachable from the internet. It responds with that nonce so that its peer
also knows.

Even though this is easy to fool, we decided that it's useful because it offers
some sort of automatic discovery of public addressability. A node could become
a supernode without any configuration.

I (avieth) think this is silly. Anybody who doesn't know that their computer
is reachable from the internet is in big trouble. Surely such a user would be
capable of setting firewall rules, and therefore ought to have the technical
capacity required to pass an extra command line argument to cardano-node
indicating the public address.

Comment: Duncan

> You don't need to say this. Can simply state the assumption (the assumption you think is silly).

Nodes will not choose their own identifiers. Allowing this would open the
door to easy eclipse attacks. Instead, peers derive the identifiers of their
peers as a hash of their observed addresses.
Relevant paper detailing some attacks:
https://syssec.kaist.ac.kr/~yongdaek/doc/kad_attack_securecomm.pdf


Comment: Duncan

> Given the suggested options in the paper, we should explain why we go for hash(IP) rather than hash (PK).

> This is a mix of design and development plan, we should ideally separate these aspects.

## Details

There's a test suite, but it fails on `froozen/master`. Perhaps start by
looking into that?

Changing the identifier scheme will probably be the most invasive.

Must change the wire protocol so that a peer can indicate whether or not it's
publicly addressable. This could be added to the `Signal` type.
See `Network.Kademlia.Types`, and `parseSignal` from
`Network.Kademlia.Protocol.Parsing`. Instead of parsing their Kademlia
identifier (which we now generate from their address) we can parse this
"incognito" field.
`backgroundProcess` from `Network.Kademlia.Instance` must use this new
information to decide whether it should `insertNode`.

The data storage aspect of Kademlia could be removed, as we're not using it.
We should cut out the storage-related codes `FIND_VALUE`, `RETURN_VALUE`
and `STORE` as there's really no sense in responding to these
(these are constructors of `Command` in `Network.Kademlia.Types`).
That's to say, the server should be cut down to the minimal required
functionality.

In mainline (froozen/kademlia) the initial peer's identifier must be given
in order to join the network. We need to get rid of that requirement: the
host and port is enough. The serokell fork already has this implemented.
There's a notion of a reply queue with registration
(`src/Network/ReplyQueue.hs`). In mainline the `ReplyRegistration`
type uses the Kademlia identifier, but in the serokell fork it uses the
`Peer` value (host and port). It should be OK to use the `Peer`, as a host
and port uniquely identifies a Kademlia server.
There's a hazard here: if the peer reply is observed to have a different
host or port from the expected one, then the reply will appear to time
out. Using the Kademlia identifier avoids this, but it shouldn't be a
problem using the host and port either, as there's no reason to not know
the host and port that we expect: if we send to host:port and their response
comes back with a different host or port then they're either misbehaving or
going through some weird asymmetric NAT or proxy, which we don't have to
support.

There's a known issue with the serokell fork regarding response
serialization [TW-153]. We should make sure that goes away.
The use of `recvFrom` in `Network.Kademlia.Networking` chooses a limit of
1500 bytes, and always tries to parse the entire signal from this.

The serokell fork adds some features that we don't want:
  - Banned nodes. Not actually used, so let's drop it.
  - Socket bind port distinct from claimed address. Not necessary. Claimed
    address is always the one that the peer observes.
    All nodes can bind to whatever port they wish; Kademlia logic does not
    need to know what it is.
  - There may be others...

## Stretch feature: QoS estimation within the discovery protocol.

Kademlia includes no facility for estimating the QoS that a peer could
provide. It may be possible to gather a delta-Q estimate while doing
UDP exchanges in the discovery protocol.

It may not be possible to do cross-platform, as I'm not sure whether
the winsock API supports gathering precise timing information on UDP
sockets (though it seems to be possible for TCP sockets).

In order to do this we'll need to settle the design for QoS estimation.

This feature is not strictly necessary, as we plan to do this apart from
peer discovery, as a completely separate piece (next section). I recommend
giving it low priority.
