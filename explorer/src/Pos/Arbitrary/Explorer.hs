{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Explorer types.

module Pos.Arbitrary.Explorer () where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Explorer.Core.Types (TxExtra (..))

import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Txp.Arbitrary ()

instance Arbitrary TxExtra where
    arbitrary = genericArbitrary
    shrink = genericShrink
