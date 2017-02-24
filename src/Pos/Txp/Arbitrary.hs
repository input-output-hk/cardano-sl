{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for 'Txp' networking types.

module Pos.Txp.Arbitrary () where

import           Data.DeriveTH                 (derive, makeArbitrary)
import           Test.QuickCheck               (Arbitrary (..))
import           Universum

import           Pos.Binary.Update             ()
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Types.Arbitrary           ()
import           Pos.Update.Arbitrary.Core     ()

import           Pos.Txp.Network.Types         (TxMsgContents (..), TxMsgTag (..))


derive makeArbitrary ''TxMsgTag
derive makeArbitrary ''TxMsgContents

instance Arbitrary (DataMsg TxMsgContents) where
    arbitrary = DataMsg <$> arbitrary
