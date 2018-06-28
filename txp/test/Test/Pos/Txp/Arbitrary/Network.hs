{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' instances for 'Pos.Txp.Network' types defined in 'src'

module Test.Pos.Txp.Arbitrary.Network () where

import           Universum

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Pos.Infra.Communication.Types.Relay (DataMsg (..))
import           Pos.Txp.Network.Types (TxMsgContents (..))

import           Test.Pos.Txp.Arbitrary ()

instance Arbitrary TxMsgContents where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (DataMsg TxMsgContents) where
    arbitrary = DataMsg <$> arbitrary
    shrink = genericShrink
