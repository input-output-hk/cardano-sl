{-# LANGUAGE TemplateHaskell #-}

-- | 'Arbitrary' instances for 'Pos.Txp.Network' types defined in 'src'

module Pos.Arbitrary.Txp.Network () where

import           Universum

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Txp ()
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Txp.Network.Types (TxMsgContents (..))

instance HasConfiguration => Arbitrary TxMsgContents where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasConfiguration => Arbitrary (DataMsg TxMsgContents) where
    arbitrary = DataMsg <$> arbitrary
    shrink = genericShrink
