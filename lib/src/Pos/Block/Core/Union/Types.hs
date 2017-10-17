-- | Union of blockchain types.

module Pos.Block.Core.Union.Types
       ( BiSsc
       , BlockHeader
       , Block

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
    , Bi (GenericBlockHeader (MainBlockchain ssc))
    )

-- | Either header of ordinary main block or genesis block.
type BlockHeader = Either GenesisBlockHeader MainBlockHeader

-- | Block.
type Block = Either GenesisBlock MainBlock

-- | This function is required because type inference fails in attempts to
-- hash only @Right@ or @Left@.
--
-- Perhaps, it shouldn't be here, but I decided not to create a module
-- for only this function.
blockHeaderHash :: Bi BlockHeader => BlockHeader -> HeaderHash
blockHeaderHash = unsafeHash
