{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Update System Poll types.

module Test.Pos.Update.Arbitrary.Poll () where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Pos.Binary.Update ()
import           Pos.Update.Poll.Modifier (PollModifier (..))
import           Pos.Update.Poll.PollState (PollState (..), psActivePropsIdx)
import           Pos.Update.Poll.Types (BlockVersionState (..),
                     ConfirmedProposalState (..), DecidedProposalState (..),
                     DpsExtra (..), PrevValue, ProposalState (..), USUndo,
                     UndecidedProposalState (..), UpsExtra (..))

import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Infra.Arbitrary.Slotting ()
import           Test.Pos.Update.Arbitrary.Core ()
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
