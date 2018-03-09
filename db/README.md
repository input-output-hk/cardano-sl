# cardano-sl-db

Database operations for Cardano SL.

A Cardano node has a database that is used to store:

  * The blocks that make up the blockchain and a block index to make queries on
    the blocks easier and more efficient.
  * UTXOs (unspent transaction outputs).
  * LRC (leaders and richmen computation) data required for Proof-of-Stake.
  * Other miscellaneous data.

A cardano node stores its data in a [RocksDB] database. Since [RocksDB] is
written in C++, it is accessed from Haskell via the [rocksdb-haskell-ng] library.

In addition, this library provides a pure database interface built on top
of `Data.Map` that mirrors the RocksDB data base so that one can be tested
against the other.


[RocksDB]: http://rocksdb.org/
[rocksdb-haskell-ng]: https://github.com/input-output-hk/rocksdb-haskell-ng
