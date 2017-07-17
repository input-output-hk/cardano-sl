-- | Union of blockchain types.

module Pos.Block.Core.Union.Types
       ( BiSsc
       , BlockHeader
       , Block
       , BiHeader

       , blockHeaderHash

       , module Pos.Core.Block
       , module Pos.Block.Core.Genesis.Types
       , module Pos.Block.Core.Main.Types
       ) where

import           Universum

import           Pos.Binary.Class             (Bi)
import           Pos.Core                     (HeaderHash)
import           Pos.Crypto                   (unsafeHash)
import           Pos.Ssc.Class.Types          (Ssc (..))

-- Re-exports
import           Pos.Block.Core.Genesis.Types
import           Pos.Block.Core.Main.Types
import           Pos.Core.Block

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

-- | Ssc w/ buildable blockchain
type BiSsc ssc =
    ( Ssc ssc
    , Bi (GenericBlockHeader (GenesisBlockchain ssc))
    , Bi (GenericBlockHeader (MainBlockchain ssc)))

-- | Either header of ordinary main block or genesis block.
type BlockHeader ssc = Either (GenesisBlockHeader ssc) (MainBlockHeader ssc)

-- | Block.
type Block ssc = Either (GenesisBlock ssc) (MainBlock ssc)

{- The story of unnecessary constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All of the instances below have a BiHeader constraint. BiHeader is defined as
“Bi BlockHeader”, which in its turn expands into:

    Bi (Either GenesisBlockHeader MainBlockHeader)

Thus, effectively we require “Bi MainBlockHeader” for the HasHeaderHash
instance of *Genesis*BlockHeader, and vice-versa. This is because hashing of
all headers (MainBlockHeader and GenesisBlockHeader) is done by converting
them to BlockHeader first, so that a header would have the same hash
regardless of whether it's inside a BlockHeader or not.
-}

type BiHeader ssc = Bi (BlockHeader ssc)

-- | This function is required because type inference fails in attempts to
-- hash only @Right@ or @Left@.
--
-- Perhaps, it shouldn't be here, but I decided not to create a module
-- for only this function.
blockHeaderHash :: BiHeader ssc => BlockHeader ssc -> HeaderHash
blockHeaderHash = unsafeHash
