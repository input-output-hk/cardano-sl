-- | Union of blockchain types.

module Pos.Core.Block.Union.Types
       ( BlockHeader
       , Block
       , ComponentBlock (..)

       , blockHeaderHash

       , module Pos.Core.Block.Genesis.Types
       , module Pos.Core.Block.Main.Types
       ) where

import           Universum

import           Pos.Binary.Class (Bi)
import           Pos.Core.Common (HeaderHash)
import           Pos.Crypto (unsafeHash)
-- Re-exports
import           Pos.Core.Block.Genesis.Types
import           Pos.Core.Block.Main.Types
import           Pos.Core.Class (IsGenesisHeader, IsMainHeader (..))
import           Pos.Util.Some (Some)

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

-- | Either header of ordinary main block or genesis block.
type BlockHeader = Either GenesisBlockHeader MainBlockHeader

-- | Block.
type Block = Either GenesisBlock MainBlock

-- | Representation of 'Block' passed to a component.
data ComponentBlock payload =
    ComponentBlockGenesis (Some IsGenesisHeader)
    | ComponentBlockMain
       { bcmHeader  :: !(Some IsMainHeader)
       , bcmPayload :: !payload }

-- | This function is required because type inference fails in attempts to
-- hash only @Right@ or @Left@.
--
-- Perhaps, it shouldn't be here, but I decided not to create a module
-- for only this function.
blockHeaderHash :: Bi BlockHeader => BlockHeader -> HeaderHash
blockHeaderHash = unsafeHash
