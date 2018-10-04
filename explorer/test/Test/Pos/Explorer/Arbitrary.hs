{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Explorer types.

module Test.Pos.Explorer.Arbitrary () where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Pos.Explorer.Core (TxExtra (..))

import           Test.Pos.Chain.Txp.Arbitrary ()
import           Test.Pos.Core.Arbitrary ()

instance Arbitrary TxExtra where
    arbitrary = genericArbitrary
    shrink = genericShrink
