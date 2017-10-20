-- | United miscellaneous functionality.

module Pos.Block.Core.Union.Misc
       ( getBlockHeader
       , blockHeader
       ) where

import           Universum

import           Control.Lens               (Getter, choosing, to)
import qualified Data.Text.Buildable        as Buildable

import           Pos.Binary.Class           (Bi)
import           Pos.Block.Core.Genesis     ()
import           Pos.Block.Core.Main        ()
import           Pos.Block.Core.Union.Types (Block, BlockHeader, blockHeaderHash)
import           Pos.Core                   (GenericBlock (..), HasDifficulty (..),
                                             HasHeaderHash (..), IsHeader)

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

instance Bi BlockHeader =>
         Buildable BlockHeader where
    build = either Buildable.build Buildable.build

----------------------------------------------------------------------------
-- HasHeaderHash
----------------------------------------------------------------------------

instance Bi BlockHeader =>
         HasHeaderHash BlockHeader where
    headerHash = blockHeaderHash

instance Bi BlockHeader =>
         HasHeaderHash Block where
    headerHash = blockHeaderHash . getBlockHeader

-- | Take 'BlockHeader' from either 'GenesisBlock' or 'MainBlock'.
getBlockHeader :: Block -> BlockHeader
getBlockHeader = bimap _gbHeader _gbHeader

blockHeader :: Getter Block BlockHeader
blockHeader = to getBlockHeader

----------------------------------------------------------------------------
-- HasDifficulty
----------------------------------------------------------------------------

instance HasDifficulty BlockHeader where
    difficultyL = choosing difficultyL difficultyL

instance HasDifficulty Block where
    difficultyL = choosing difficultyL difficultyL

----------------------------------------------------------------------------
-- IsHeader
----------------------------------------------------------------------------

instance Bi BlockHeader  => IsHeader BlockHeader
