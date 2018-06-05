{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' instances for 'Pos.Txp.Network' types defined in 'src'

module Test.Pos.Txp.Arbitrary.Network () where

import           Universum

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Core (HasProtocolMagic)
import           Pos.Infra.Communication.Types.Relay (DataMsg (..))
import           Pos.Txp.Network.Types (TxMsgContents (..))

import           Test.Pos.Txp.Arbitrary ()

instance HasProtocolMagic => Arbitrary TxMsgContents where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasProtocolMagic => Arbitrary (DataMsg TxMsgContents) where
    arbitrary = DataMsg <$> arbitrary
    shrink = genericShrink
