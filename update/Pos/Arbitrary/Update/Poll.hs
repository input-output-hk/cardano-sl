{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Update System Poll types.

module Pos.Arbitrary.Update.Poll () where

import           Test.QuickCheck                   (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Core                ()
import           Pos.Arbitrary.Slotting            ()
import           Pos.Arbitrary.Update.Core         ()
import           Pos.Binary.Core                   ()
import           Pos.Binary.Update                 ()
import           Pos.Update.Poll.PollState         (PollState (..))
import           Pos.Update.Poll.Types             (BlockVersionState (..),
                                                    ConfirmedProposalState (..),
                                                    DecidedProposalState (..),
                                                    DpsExtra (..), PollModifier (..),
                                                    PrevValue, ProposalState (..),
                                                    UndecidedProposalState (..),
                                                    UpsExtra (..), USUndo)

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
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary USUndo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (PrevValue a) where
    arbitrary = genericArbitrary
    shrink = genericShrink
