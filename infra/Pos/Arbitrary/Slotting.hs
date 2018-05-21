{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Pos.Slotting types (infra package)

module Pos.Arbitrary.Slotting () where

import           Universum

import           Test.QuickCheck (Arbitrary (..), arbitrary, oneof)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Core ()
import           Pos.Slotting.Types (EpochSlottingData (..), SlottingData, createInitSlottingData)

instance Arbitrary EpochSlottingData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SlottingData where
    -- Fixed instance since it's impossible to create and instance
    -- where one creates @SlottingData@ without at least two parameters.
    arbitrary = oneof [ createInitSlottingData <$> arbitrary <*> arbitrary ]
