-- | Arbitrary instances for Explorer types.

module Pos.Arbitrary.Explorer () where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Core (HasConfiguration)
import           Pos.Explorer.Core.Types (TxExtra (..))
import           Pos.Txp ()

instance HasConfiguration => Arbitrary TxExtra where
    arbitrary = genericArbitrary
    shrink = genericShrink
