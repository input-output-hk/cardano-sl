-- | United miscellaneous functionality.

module Pos.Block.Core.Union.Misc
       ( getBlockHeader
       , blockHeader
       ) where

import           Universum

import           Control.Lens               (Getter, choosing, to)
import qualified Data.Text.Buildable        as Buildable

import           Pos.Block.Core.Genesis     ()
import           Pos.Block.Core.Main        ()
import           Pos.Block.Core.Union.Types (BiHeader, BiSsc, Block, BlockHeader,
                                             blockHeaderHash)
import           Pos.Core                   (GenericBlock (..), HasDifficulty (..),
                                             HasHeaderHash (..), IsHeader)

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

instance BiSsc ssc => Buildable (BlockHeader ssc) where
    build = either Buildable.build Buildable.build

----------------------------------------------------------------------------
-- HasHeaderHash
----------------------------------------------------------------------------

instance BiHeader ssc =>
         HasHeaderHash (BlockHeader ssc) where
    headerHash = blockHeaderHash

instance BiHeader ssc =>
         HasHeaderHash (Block ssc) where
    headerHash = blockHeaderHash . getBlockHeader

-- | Take 'BlockHeader' from either 'GenesisBlock' or 'MainBlock'.
getBlockHeader :: Block ssc -> BlockHeader ssc
getBlockHeader = bimap _gbHeader _gbHeader

blockHeader :: Getter (Block ssc) (BlockHeader ssc)
blockHeader = to getBlockHeader

----------------------------------------------------------------------------
-- HasDifficulty
----------------------------------------------------------------------------

instance HasDifficulty (BlockHeader ssc) where
    difficultyL = choosing difficultyL difficultyL

instance HasDifficulty (Block ssc) where
    difficultyL = choosing difficultyL difficultyL

----------------------------------------------------------------------------
-- IsHeader
----------------------------------------------------------------------------

-- If this constraint seems wrong to you, read “The story of unnecessary
-- constraints” in 'Types.hs'.

instance BiHeader ssc => IsHeader (BlockHeader ssc)
