{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Update System Poll types.

module Pos.Update.Arbitrary.Poll () where

import           Data.DeriveTH                     (derive, makeArbitrary)
import           Test.QuickCheck                   (Arbitrary (..), choose)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
import           Universum

import           Pos.Binary.Core                   ()
import           Pos.Binary.Update                 ()
import           Pos.Slotting.Arbitrary            ()
import           Pos.Core.Arbitrary                ()
import           Pos.Update.Arbitrary.Core         ()
import           Pos.Update.Poll.PollState         (PollState (..))
import           Pos.Update.Poll.Types             (BlockVersionState (..),
                                                    ConfirmedProposalState (..),
                                                    DecidedProposalState (..),
                                                    DpsExtra (..), PollModifier (..),
                                                    PrevValue, ProposalState (..),
                                                    UndecidedProposalState (..),
                                                    UpsExtra (..), USUndo)

derive makeArbitrary ''UpsExtra
derive makeArbitrary ''UndecidedProposalState

derive makeArbitrary ''DpsExtra
derive makeArbitrary ''DecidedProposalState

derive makeArbitrary ''ConfirmedProposalState
derive makeArbitrary ''ProposalState

derive makeArbitrary ''BlockVersionState

derive makeArbitrary ''PollModifier

derive makeArbitrary ''PollState

instance Arbitrary a => Arbitrary (PrevValue a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary USUndo where
    arbitrary = genericArbitrary
    shrink = genericShrink
