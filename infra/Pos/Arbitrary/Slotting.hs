-- | Arbitrary instances for Pos.Slotting types (infra package)

module Pos.Arbitrary.Slotting () where

import           Test.QuickCheck                   (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Core                ()
import           Pos.Slotting.Types                (EpochSlottingData (..),
                                                    SlottingData (..))

instance Arbitrary EpochSlottingData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SlottingData where
    arbitrary = genericArbitrary
    shrink = genericShrink
