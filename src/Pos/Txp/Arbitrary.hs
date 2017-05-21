{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for 'Txp' types.

module Pos.Txp.Arbitrary () where

import           Universum

import           Data.DeriveTH                 (derive, makeArbitrary)
import           Test.QuickCheck               (Arbitrary (..))

import           Pos.Binary.Update             ()
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Txp.Core                  (TxAux (..))
import           Pos.Txp.Network.Types         (TxMsgContents (..))
import           Pos.Types.Arbitrary           ()

derive makeArbitrary ''TxAux
derive makeArbitrary ''TxMsgContents

instance Arbitrary (DataMsg TxMsgContents) where
    arbitrary = DataMsg <$> arbitrary
