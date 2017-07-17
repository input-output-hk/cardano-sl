-- | Arbitrary instances for Explorer types.

module Pos.Explorer.Arbitrary () where

import           Test.QuickCheck                   (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Explorer.Core.Types           (TxExtra (..))
--import           Pos.Core.Arbitrary                ()
import           Pos.Txp                           ()

instance Arbitrary TxExtra where
    arbitrary = genericArbitrary
    shrink = genericShrink
