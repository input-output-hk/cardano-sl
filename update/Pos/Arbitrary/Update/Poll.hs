{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Update System Poll types.

module Pos.Arbitrary.Update.Poll () where

import           Universum

import qualified Data.HashMap.Strict               as HM
import           Test.QuickCheck                   (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Core                ()
import           Pos.Arbitrary.Slotting            ()
import           Pos.Arbitrary.Update.Core         ()
import           Pos.Binary.Core                   ()
import           Pos.Binary.Update                 ()
import           Pos.Core.Context                  (HasCoreConstants)
import           Pos.Update.Poll.PollState         (PollState (..), psActivePropsIdx)
import           Pos.Update.Poll.Types             (BlockVersionState (..),
                                                    ConfirmedProposalState (..),
                                                    DecidedProposalState (..),
                                                    DpsExtra (..), PollModifier (..),
                                                    PrevValue, ProposalState (..), USUndo,
                                                    UndecidedProposalState (..),
                                                    UpsExtra (..))

instance Arbitrary UpsExtra where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasCoreConstants => Arbitrary UndecidedProposalState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary DpsExtra where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasCoreConstants => Arbitrary DecidedProposalState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ConfirmedProposalState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasCoreConstants => Arbitrary ProposalState  where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BlockVersionState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasCoreConstants => Arbitrary PollModifier where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasCoreConstants => Arbitrary PollState where
    arbitrary = do
        ps <- genericArbitrary
        return (ps & psActivePropsIdx %~ HM.filter (not . null))
    shrink = genericShrink

instance HasCoreConstants => Arbitrary USUndo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (PrevValue a) where
    arbitrary = genericArbitrary
    shrink = genericShrink
