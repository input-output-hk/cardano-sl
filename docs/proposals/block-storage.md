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

Here's a list of requirements for a good solution:

  * R1. Store blocks separately from the main database.

  * R2. Fast lookup by header hash.
  
  * R3. Fast sequential iteration

  * R4. A solution should be stable to corruptions

  * R5. Apply compression to the blocks, and since each block isn't
        very well compressible on its own, the solution should use either
        dictionary compression or compress all blocks as a whole.
    
  * R6. Solution should be easy to implement and maintain

## Possible solutions

### Storing all blocks in one RocksDB

  1. New RocksDB for blocks. Enable out of a box compression with dictionary.
  
  2. When a new block comes, add it along with undo to this database by header hash.

Such solution satisfies R1, R2, R5 and R6.
Also there is a way to achieve R3, it will be described in the next chapter.

### One RocksDB per epoch

  1. Create a new RocksDB database for each epoch, with keys = hashes and
     values = blocks. Enable out of a box compression with dictionary.

  2. Store a map `hash -> epoch` in some other database (e.g. “blockindex”).

  3. When a new block comes, add it along with undo to the database and to the index map.
  
This solution satisfies R1, R2, R4, R5.
Also there is the same way to achieve R3 like for **Storing all blocks in one RocksDB**.
  
### One plain file per epoch

  1. Create a new file for each epoch.
  
  2. Store epoch's blocks (along with undo) in plain file sequentially.
  
  3. Support a map `hash -> position in a file` in some other database (e.g. "blockindex")

Such solution satisfies R1, R2, R3 and R4, however, it's harder to do compression (R5) and it's hard to implement and maintain (R6).

### The best solution
It seems that the third solution can satisfy all R1-R6 requirements, however, 
it's harder than it may seem. Managing all this stuff (like positions, files and compression) can cause a lot of pain for us.

The third solution looks very similar to the first one and it has stable to corruption unlike the first one,
but it's very questionable whether we should be worried about stableness to corruptions, probably user himself should care about it.

So in result, it seems that we should go by the easiest way and implement the first approach.

## Sequential iteration hack

As it was mentioned in the previous section, 
there is a way to achieve fast sequential iteration for the approach based on storing blocks in RocksDB.
This ways is a little bit tricky, but I am sure it should be at least mentioned.

So, first of all, let's clarify the problem. Currently, all sequential iterations and loading are implemented in such way: 
we iterate over header hashes and get required data by this header hash. 
Getting data by header hash is random access operation and may take significant time 
(it should be measured how much it really takes, there is [CSL-2350](https://iohk.myjetbrains.com/youtrack/issue/CSL-2350) for it).

The hack is let's construct key of block (along with undo) as concationation of the following strings:
  1. Unique prefix for blocks' keys, for instance `f/`
  2. Char corresponding to `log₁₀(block's difficulty)`. 
     So the idea is to match to bigger difficulties chars with bigger codes and vice versa.
     Then blocks with smaller difficulty will follow earlier.
  3. Block's difficulty written as string, for instance difficulty `123` will turn into `"123"` string.
  4. Block's header hash
  
To get access to block by header hash we first of all need to get its header (which contains difficulty) from "blocindex", 
construct key for block using suggested approach and then get a block from database.

Sequential iteration over blockchain starting with specified header hash can be implemented using `iterSeek` 
and either `iterNext` for forward iteration or `iterPrev` for backward iteration.

This hack is useful when getting blocks by header hash isn't a frequent operation, 
this assumption seems to be applied to our case.

