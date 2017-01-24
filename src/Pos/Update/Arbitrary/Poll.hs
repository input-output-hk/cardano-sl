{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Update System Poll types.

module Pos.Update.Arbitrary.Poll where

import           Data.DeriveTH             (derive, makeArbitrary)
import           Test.QuickCheck           (Arbitrary (..))
import           Universum

import           Pos.Binary.Update         ()
import           Pos.Types.Arbitrary       ()
import           Pos.Update.Arbitrary.Core ()
import           Pos.Update.Poll.Types     (ConfirmedProposalState (..),
                                            DecidedProposalState (..), DpsExtra (..),
                                            UndecidedProposalState (..), UpsExtra (..))

derive makeArbitrary ''UpsExtra
derive makeArbitrary ''UndecidedProposalState

derive makeArbitrary ''DpsExtra
derive makeArbitrary ''DecidedProposalState

derive makeArbitrary ''ConfirmedProposalState
