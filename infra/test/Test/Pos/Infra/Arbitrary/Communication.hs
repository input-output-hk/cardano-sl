{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' instances for 'Pos.Infra.Communication' types defined in 'src'

module Test.Pos.Infra.Arbitrary.Communication () where

import           Universum

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericShrink)

import           Pos.Core.Txp (TxMsgContents (..))
import           Pos.Infra.Communication.Types.Relay (DataMsg (..))

import           Test.Pos.Infra.Arbitrary.Txp ()

instance Arbitrary (DataMsg TxMsgContents) where
    arbitrary = DataMsg <$> arbitrary
    shrink = genericShrink
