module Pos.Core.Slotting.EpochIndex
       ( EpochIndex (..)
       , HasEpochIndex (..)
       , isBootstrapEra
       ) where

import           Universum

import           Control.Lens (choosing)
import           Data.Ix (Ix)
import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, int, (%))
import           Pos.Binary.Class (Bi (..))
import           Pos.Util.Some (Some, liftLensSome)

-- | Index of epoch.
newtype EpochIndex = EpochIndex
    { getEpochIndex :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Ix, Integral, Real, Generic, Hashable, Bounded, Typeable, NFData)

instance Buildable EpochIndex where
    build = bprint ("#"%int)

class HasEpochIndex a where
    epochIndexL :: Lens' a EpochIndex

instance HasEpochIndex (Some HasEpochIndex) where
    epochIndexL = liftLensSome epochIndexL

instance (HasEpochIndex a, HasEpochIndex b) =>
         HasEpochIndex (Either a b) where
    epochIndexL = choosing epochIndexL epochIndexL

instance Bi EpochIndex where
    encode (EpochIndex epoch) = encode epoch
    decode = EpochIndex <$> decode

-- | Bootstrap era is ongoing until stakes are unlocked. The reward era starts
-- from the epoch specified as the epoch that unlocks stakes:
--
-- @
--                     [unlock stake epoch]
--                             /
-- Epoch: ...  E-3  E-2  E-1   E+0  E+1  E+2  E+3  ...
--        ------------------ | -----------------------
--             Bootstrap era   Reward era
-- @
--
isBootstrapEra
    :: EpochIndex -- ^ Unlock stake epoch
    -> EpochIndex -- ^ Epoch in question (for which we determine whether it
                  --                      belongs to the bootstrap era).
    -> Bool
isBootstrapEra unlockStakeEpoch epoch =
    epoch < unlockStakeEpoch
