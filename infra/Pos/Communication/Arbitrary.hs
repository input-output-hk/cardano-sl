-- | 'Arbitrary' instances for 'Pos.Communication.Types'

module Pos.Communication.Arbitrary () where

import           Test.QuickCheck                   (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Communication.Types.Relay     (DataMsg (..))

instance Arbitrary a => Arbitrary (DataMsg a) where
    arbitrary = genericArbitrary
    shrink = genericShrink
