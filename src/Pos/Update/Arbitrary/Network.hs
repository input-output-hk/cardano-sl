{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Update System networking types.

module Pos.Update.Arbitrary.Network
       (
       ) where

import           Data.DeriveTH            (derive, makeArbitrary)
import           Test.QuickCheck          (Arbitrary (..))
import           Universum

import           Pos.Update.Network.Types (ProposalMsgTag (..), VoteMsgTag (..))

derive makeArbitrary ''ProposalMsgTag
derive makeArbitrary ''VoteMsgTag
