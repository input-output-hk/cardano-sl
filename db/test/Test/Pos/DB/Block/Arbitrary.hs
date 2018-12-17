{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.DB.Block.Arbitrary () where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Pos.Chain.Block (SlogUndo, Undo (..))

import           Test.Pos.Chain.Delegation.Arbitrary ()
import           Test.Pos.Chain.Txp.Arbitrary ()
import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.DB.Update.Arbitrary ()

instance Arbitrary SlogUndo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Undo where
    arbitrary = genericArbitrary
    shrink = genericShrink
