module Pos.Core.Common.ChainDifficulty
       ( ChainDifficulty (..)
       ) where

import           Universum

import           Pos.Core.Common.BlockCount

-- | Chain difficulty represents necessary effort to generate a
-- chain. In the simplest case it can be number of blocks in chain.
newtype ChainDifficulty = ChainDifficulty
    { getChainDifficulty :: BlockCount
    } deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Generic, Buildable, Typeable, NFData)
