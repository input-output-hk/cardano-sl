{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' instances for 'Pos.Txp.Network' types defined in 'src'

module Pos.Arbitrary.Txp.Network () where

import           Universum

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Txp ()
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Core (HasProtocolMagic)
import           Pos.Txp.Network.Types (TxMsgContents (..))

instance HasProtocolMagic => Arbitrary TxMsgContents where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasProtocolMagic => Arbitrary (DataMsg TxMsgContents) where
    arbitrary = DataMsg <$> arbitrary
    shrink = genericShrink
