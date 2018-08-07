module Pos.Core.Common.ChainDifficulty
       ( ChainDifficulty (..)
       , HasDifficulty (..)
       , isMoreDifficult
       ) where

import           Universum

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.SafeCopy (base, deriveSafeCopySimple)

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core.Common.BlockCount
import           Pos.Util.Some (Some, liftLensSome)

-- | Chain difficulty represents necessary effort to generate a
-- chain. In the simplest case it can be number of blocks in chain.
newtype ChainDifficulty = ChainDifficulty
    { getChainDifficulty :: BlockCount
    } deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Generic, Buildable, Typeable, NFData)

class HasDifficulty a where
    difficultyL :: Lens' a ChainDifficulty

instance HasDifficulty (Some HasDifficulty) where
    difficultyL = liftLensSome difficultyL

isMoreDifficult :: HasDifficulty a => a -> a -> Bool
a `isMoreDifficult` b = a ^. difficultyL > b ^. difficultyL

deriveJSON defaultOptions ''ChainDifficulty

deriveSimpleBi ''ChainDifficulty [
    Cons 'ChainDifficulty [
        Field [| getChainDifficulty :: BlockCount |]
    ]]

deriveSafeCopySimple 0 'base ''ChainDifficulty

