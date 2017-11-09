-- | Union of blockchain types.

module Pos.Core.Block.Union.Types
       ( BlockHeader
       , Block

       , blockHeaderHash

       , module Pos.Core.Block.Genesis.Types
       , module Pos.Core.Block.Main.Types
       ) where

import           Universum

import           Pos.Binary.Class             (Bi)
import           Pos.Core.Types               (HeaderHash)
import           Pos.Crypto                   (unsafeHash)

-- Re-exports
import           Pos.Core.Block.Genesis.Types
import           Pos.Core.Block.Main.Types

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

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
