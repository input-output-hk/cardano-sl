# Fast synchronization

In order to create transactions and know wallet's balance one needs to
know the state of the blockchain, e. g. UTXO set. The state itself is
not very big. Currently it takes about 50 Mb in mainnet and its size
increases slowly. So downloading whole state shouldn't be a big
problem, it can be done in a relatively small amount of time. Note
that it's not necessary to have all blocks, but we need some way to
know that the state is valid.

The only way to verify the state is to download all blocks which lead
to this state and verify them. However, downloading all blocks will
become very expensive after some time. For instance, a block with two
transactions currently takes about 1400 bytes. There are almost 130000
blocks in a month if all slots are filled. So if average block has
about 2 transactions, all blocks will take about 170 Mb in a month and
2 Gb in a year. Full verification is also an expensive operation which
may take even more time.

The goal of fast synchronization is to make it possible to download
all necessary state (snapshot) quickly and have reasonable confidence
in validity of this snapshot. Note that we are not talking about SPV
wallet, we still want to download all blocks and fully verify
them. The task can be split into two almost independent parts:
1. Specify the format of snapshot and how to obtain it.
2. Figure out how to check snapshot's validity.

The first part is described in TODO and the second one is described in
[Establishing trust in a snapshot](trust.md).
