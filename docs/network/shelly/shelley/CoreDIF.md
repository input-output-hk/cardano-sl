# Core DIF

## Notes from meeting with Neil and Duncan November 8

A picture
https://docs.google.com/drawings/d/1IbclztwUlZs8HYbkLGVJ7vS9D_lVX_7Wu-S_M2SGsD0/edit

Comment: Duncan

> Lets try and get an svg (or png) export of this and include it in the repo here.


  - Bridge relay node
  - Relay node
  - Core node
    - Stakeholder
    - Nonstakeholder

This factors into 2 themes I suppose:

1. Optimal routing in order to deliver blocks and other data in time (so as to
   maintain chain quality).

2. Bearer address obfuscation / protection so as to mitigate attacks.

### Membership in core DIF

- Must be a threshold for joining the network. Can't have all stakeholders.
- Perhaps: you can only join if you are registered (on chain) as a delegate
  and you have stake.
  - I'm willing to be called (I opted into being a delegate).
  - I am called (I am in top (say) 80% of stakeholders, or the top 100
    stakeholders).
    The plan seems to be to limit the actual number of members rather than
    including top n% stakeholders, which could be arbitrarily-many.
- Expectation: core nodes don't hold coins, but have stake delegated to them.
  - Example: IOHK and partners would make a new address with no coins and
    delegate all their stake to these.
  - I guess the motivation is to protect the keys that control the coins; the
    key that controls the delegate isn't so precious, since delegation can be
    revoked.

Comment: Duncan

> This turns out to be fine. All coins are always held on a different keypair than balance. But we will have a offline/onlike stake key solution. Either way it doesn't affect the network or core DIF I think. We can assume there's a single key per participant known from the blockchain and either that key can be used to sign messages, or more likely a key plus a lightweight delegation cert that shows that the presented key is a valid proxy.

- To take delegation, you don't have to be on the public delegate list.
- Currently there is no freedom from being delegated to you; you can't
  deny delegation.
- There are points at which a holder *can* join and *must* join.
  Work this out with Lars...

Comment: Duncan

> Yeah, we don't need to worry about the freedom from being delegated to here. That's the responsibility of the delegation workstream.

### Routing; building spanning tree(s)

- Publish a POA address (signed) to some others (encrypted with their
  public keys). On the blockchain? Doesn't have to be the core node's IP
  address; could be perhaps a bridge relay which will relay back to the core
  node.

Comment: Duncan

> How many you publish to needs to not be an individual choice but a rule based on the known size & membership of the core DIF. For size 100 we were thinking 10, but we need a rule that works for any size. How about a linear function, bounded below at 5 and above at 10.

- Valency min and mix. Perhaps 3 and 10.
- Each node should randomly choose 3 to 10 others. Perhaps XOR metric of
  their public keys.

Comment: Duncan

> I think XOR is the plan, it means everyone knows what the connections ought to be and it's symmetric.

  - Partitions possible. Would it be a disaster?
    Detectable? Yes, because you know the whole graph (just not the bearer
    addresses).
  - Perhaps recompute the graph by using a different salt for the hash on
    public keys for XOR metric, until the graph is connected.
  - Nodes can also publish extra routing that they choose themselves?
    Addresse of relay nodes?
  - Need an amount of time between knowing the stake distr of the next epoch
    and before the network has to be established, in which we can do this
    network computation.
    TODO look into this. How long do we have?

Comment: Duncan

> The stake distribution is chosen 2k slots before the end of the epoch, but that info is only guaranteed stable after k blocks, which is guaranteed within 2k slots, ie only worst case guaranteed stable at the end of the epoch.

- Started talking about QoS/Delta-Q in the core DIF
  Apparently we're going to do exactly what we do in the public net:
  choose a bunch of peers based on some XOR, then gather some data about their
  delta-Q, then choose a subset according to percentiles of delta-Q/performance.
  Difference here is that the computation of delta-Q is wholistic: we're
  concerened with the entire path (which can be known here) rather than in the
  non-core-DIF where we're concerned only with the first hop.

- Some discussion about time constraints RE: ensuring that the slot leader's
  block goes out earlier enough that it reaches the next slot leader before
  it's too late.
  Always going for worst case actually lowers best case throughput (i.e. tps).

Comment: Duncan

> I think we concluded we should send out the block at the start of the slot, and not wait.

- Minimal spanning trees w.r.t. delta-Q.
- Fast converging spanning tree algorithms.
  - We don't have topology change problems?
  - These cause you to end up with loops which is bad for resource use.
  - Don't have problems because we have state and so can deal with loops.
- If we choose a new route, do we create a partition? Can we reason about
  that so as to prove that we don't create partitions?
- Neil will think about it.
  Looking for the right piece of literature about this.

- Make a best guess about a route: you know all of the pairwise delta-Q.
  You pick (some deterministically, some randomly) who to send it to in the
  hope that, according to the delta-Q's you know, it will optimally go
  throughout the network.

- Matrix of delta-Q's (100 x 100). How to fill it up? We know our own, we
  propagate our own throughout the network (must propagate in order to
  propagate, hm...)
  I'm about to publish. I look at the matrix and I can compute paths which
  with some certainty will get the data to everybody within some time. Of
  course, it depends upon what the forwarders do.
  Minimize maximum delay.
  Also, due to paranoia, send to a few others with some random choice.

Comment: Duncan

> We need to clarify what this algorithm is with Neil.

- How long before we time-out a peer i.e. set its delta-Q to _|_ ?

- Complication: some of the nodes are relays, some are stakeholders.
  One network entity per public key????? We need to identify the nodes.
  Would it not work for a core node to simply claim that its addresses is
  that of a relay that it controls?

### Unrelated notes

- Benchmarks for deserializing: include in CI?
- We want numbers to ensure that our logic layer time expenses fit within
  the time budgets required for diffusion RE slot time.
- It will be much easier to gather these measurements once we cut out networking
  from logic.

TIME is such an issue. Everything is limited by time and it causes so much
trouble.
Seeing the same issue when thinking about real-time music composition and
performance. It's hard enough for a single computer to make time guarantees,
let alone a network of them.

## Questions after meeting with Neil and Duncan November 8

1. Nodes in the core DIF never hold coins? Only stake? Why? Does this buy any
   security at all? I suppose the machine itself is a target, and you wouldn't
   want to keep the private key to the _coins_ on that machine. Keeping the
   key to the delegate is fine, because the holder of the coins can revoke if
   there's a compromise. Correct?

Comment: Duncan

> Yes, only stake keys on these servers, and we'll almost certainly have an online/offline stake key solution so that nodes really use an online key and present a certificate that shows that it's a valid proxy for the known stake key.

2. Ability to deny being delegated to? Is this a must have? I would hope so.

Comment: Duncan

> Yes, but not for this workstream I think.

3. What does sharing "on the blockchain" really mean? How does it work? I'm
   very skeptical. Why not flood the (non-core-dif) network with your bearer
   address encrypted with the target's public key?

Comment: Duncan

> > What does sharing "on the blockchain" really mean? How does it work?
> It's much like sending out an MPC message which gets incorporated into the blockchain. So it's just a special kind of blockchain entry.
> 
> >Why not flood the (non-core-dif) network with your bearer address encrypted with the target's public key?
> 
> It means we have evidence of non-compliance. We could do it by broadcast but then we have no objective evidence later that someone didn't do what they were supposed to. Perhaps for v1 that is ok since we have problems gathering evidence for any of the other bits either.

Comment: George

> What kind of evidence are you talking about?
> 
> Anyway, also vote for broadcasting. When one wanna join Core DIF, he broadcasts message he wants to get addresses of DIF relay nodes and retrieves these encrypted in some way. Then through DIF relay nodes he can authenticate to finally join DIF. See no need, room or rationale to do it on chain.

4. Is the core dif construction mechanism essentially the same as the bigger
   network? Start with some XOR kademlia-style thing, then use delta-Q to
   refine it. Only difference is the wholistic approach, in which the entire
   weighted graph is known?

Comment: Duncan

> I'm not aware of anything contradicting that. And if we can make them as similar as possible that is obviously good, we can reuse logic & code.

5. It was taken for granted that a node will know all of the pairwise delta-Q
   of the ~100 nodes in the core DIF. How? Surely we can't expect a consistent
   view of this. What to do in case of missing information? I'm skeptical that
   it will just work.

Comment: Duncan

> This is part of the propagation algorithm that is yet to be fully defined. And it doesn't need to be fully consistent.

6. Suppose a core dif member uses 2 relay bridges. Is it OK to give the
   address of one of these to some nodes, and the other to some other nodes,
   w.r.t. the delta-Q measurements? I suspect so. Some will calculate the
   delta-Q for one relay, some for the other, but it's all effectively the
   delta-Q to the node behind the bridge relay.
   I don't think 1 bearer address per public key is necessary. Indeed Neil
   was quite excited about it *not* being necessary; you can give different
   addresses to different peers. 

Comment: Duncan

> I think that's fine. The delta-Q is about the route from A to B.
