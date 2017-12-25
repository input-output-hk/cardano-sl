# Optimized block storage

## The problem

Currently we store blocks in a very inefficient way (space-wise). This is
tracked in [CSL-1025][]. The main concerns are:

[CSL-1025]: https://iohk.myjetbrains.com/youtrack/issue/CSL-1025

  * On most systems, files are stored in 4kB blocks, which means that any
    file smaller than 4kB will take at least 4kB on the disk. Since our
    blocks are very small right now, they take about **eight times** more
    space than they would if they were all stored in one file.

  * Our blocks contain a lot of repetitive data (e.g. hashes of empty
    payload) and some data that can easily be derived from the rest of the
    block (e.g. headers). By applying compression and removing the parts
    that can be derived, we can additionally decrease the required space and
    make the blocks about **four times** smaller.

So, we can go from 2.76 GB to about 80–150 MB, and since the later blocks
are bigger (and less compressible), it is estimated that after the above
optimizations the database would grow by 30 MB/epoch = 180 MB/month (as a
lower bound).

## The requirements

An obvious solution is **storing all blocks in RocksDB** (a key–value
database that we are using). Indeed, a long time ago we had all blocks
stored in RocksDB – but we switched from that to storing each block in a
different file. Storing blocks in RocksDB has several related disadvantages:

  * If you want to back the database up you would have to copy the blocks as
    well, while in reality the blocks can always be redownloaded and we
    don't care much about losing them.

  * A hard drive failure has a much bigger chance to corrupt the database,
    if the database contains lots of data (blocks).

So, here's a list of requirements for a good solution:

  * We'd like to store blocks separately from the main database.

  * We want fast lookup by header hash.

  * We'd like to store them in batches (e.g. one batch = one epoch), so that
    corruption of one batch wouldn't affect other batches.

  * We want to apply compression to the blocks, and since each block isn't
    very well compressible on its own, the solution should use either
    dictionary compression or compress all blocks as a whole.

  * It'd be good to be able to reuse somebody else's code, because generally
    there are lots of non-obvious problems with files (e.g. whenever you're
    modifying a file and want to protect against power failure, you should
    use `flush` in the right way – and it's very easy to forget about that).

## The solution

Out of all considered solutions (SQLite, storing blocks in an archive, CBOR
map, some database) it seems that the ideal solution is very simple: 

  1. Create a new RocksDB database for each epoch, with keys = hashes and
     values = blocks. Enable compression with dictionary.

  2. Store a map `hash -> epoch` in some other database (e.g. “blockindex”).

  3. When a new block comes, add it to the database and to the index map.

The pros are:
  * almost no new code to write
  * fast lookups
  * no new dependencies
  * compression out of the box
  * checksumming out of the box
  * whole database can be uploaded/downloaded pretty easily

## Why not just one database?

Admittedly, storing all blocks in one RocksDB database is even easier than
storing them in several databases, if only because we don't have to maintain
a `hash -> epoch` map. However, there are two good arguments for splitting
the database:

  1. **With a split database, we won't have to redownload the whole
     blockchain if hard drive corruption happens.**

     It might seem that we shouldn't actually care that much about blocks,
     because they can be redownloaded at any time. However, it doesn't mean
     we shouldn't care about them *at all* – downloading the blockchain is a
     slow operation (for Ethereum it can take up to a day even on a fast
     connection), and it's entirely possible that we will have users with
     slow connections and big, cheap, unreliable hard drives. Dividing the
     blockchain into epochs is a nice tradeoff.

     (It is also the tradeoff that Bitcoin makes, for instance.)

  2. **A split database provides more flexibility for users.**

     When the storage format isn't monolithic, it gives users some freedom
     to adjust it to their circumstances:

       * For instance, a user (e.g. an exchange) could write a simple script
         to store old epochs on a RAID array and the latest epoch – on an
         SSD.

       * A trusted party (i.e. trusted by a particular group of users and
         not necessarily by the whole community) could distribute a torrent
         containing the first N epochs, and then everyone would just
         download the missing blocks.

       * Somebody could want to keep an incremental backup of the blockchain
         on a portable hard drive (an incremental backup, of course, becomes
         much harder to perform with a monolithic database).
