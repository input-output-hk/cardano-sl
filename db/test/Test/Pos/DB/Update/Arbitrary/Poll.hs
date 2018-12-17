{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Update System Poll types.

module Test.Pos.DB.Update.Arbitrary.Poll () where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Pos.Chain.Update (BlockVersionState (..),
                     ConfirmedProposalState (..), DecidedProposalState (..),
                     DpsExtra (..), PollModifier (..), PrevValue,
                     ProposalState (..), USUndo, UndecidedProposalState (..),
                     UpsExtra (..))
import           Pos.DB.Update (PollState (..), psActivePropsIdx)

import           Test.Pos.Chain.Update.Arbitrary ()
import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Core.Arbitrary.Slotting ()
import           Test.Pos.Util.Modifier ()

instance Arbitrary UpsExtra where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary UndecidedProposalState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary DpsExtra where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary DecidedProposalState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ConfirmedProposalState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ProposalState  where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BlockVersionState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PollModifier where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PollState where
    arbitrary = do
        ps <- genericArbitrary
        return (ps & psActivePropsIdx %~ HM.filter (not . null))
    shrink = genericShrink

instance Arbitrary USUndo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (PrevValue a) where
    arbitrary = genericArbitrary
    shrink = genericShrink
