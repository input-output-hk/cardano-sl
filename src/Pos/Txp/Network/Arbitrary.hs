{-# LANGUAGE TemplateHaskell #-}

-- | 'Arbitrary' instances for 'Pos.Txp.Network' types defined in 'src'

module Pos.Txp.Network.Arbitrary () where

import           Universum

import           Test.QuickCheck                   (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Binary.Update                 ()
import           Pos.Communication.Types.Relay     (DataMsg (..))
import           Pos.Txp.Arbitrary                 ()
import           Pos.Txp.Network.Types             (TxMsgContents (..))

instance Arbitrary TxMsgContents where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (DataMsg TxMsgContents) where
    arbitrary = DataMsg <$> arbitrary
    shrink = genericShrink
