# Core DIF

Factors into 2 themes I suppose:

1. Optimal routing in order to deliver blocks and other data in time (so as to
   maintain chain quality).

2. Bearer address obfuscation / protection so as to mitigate attacks.

## Questions after meeting with Neil and Duncan November 8

1. Nodes in the core DIF never hold coins? Only stake? Why? Does this buy any
   security at all? I suppose the machine itself is a target, and you wouldn't
   want to keep the private key to the _coins_ on that machine. Keeping the
   key to the delegate is fine, because the holder of the coins can revoke if
   there's a compromise.
   Right?

2. Ability to deny being delegated to? Is this a must have? I would hope so.

3. What does sharing "on the blockchain" really mean? How does it work? I'm
   very skeptical. Why not flood the (non-core-dif) network with your bearer
   address encrypted with the target's public key?

4. Is the core dif construction mechanism essentially the same as the bigger
   network? Start with some XOR kademlia-style thing, then use delta-Q to
   refine it.

5. It was taken for granted that a node will know all of the pairwise delta-Q
   of the ~100 nodes in the core DIF. How? Surely we can't expect a consistent
   view of this. What to do in case of missing information?

6. Suppose a core dif member uses 2 relay bridges. Is it OK to give the
   address of one of these to some nodes, and the other to some other nodes,
   w.r.t. the delta-Q measurements? I suspect so. Some will calculate the
   delta-Q for one relay, some for the other, but it's all effectively the
   delta-Q to the node behind the bridge relay.

   I don't think 1 bearer address per public key is necessary. Indeed Neil
   was quite excited about it *not* being necessary; you can give different
   addresses to different peers. 


