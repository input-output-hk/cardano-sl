{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' instances for 'Pos.Infra.Communication' types defined in 'src'

module Test.Pos.Infra.Arbitrary.Txp () where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Pos.Chain.Txp (TxMsgContents (..))

import           Test.Pos.Chain.Txp.Arbitrary ()

instance Arbitrary TxMsgContents where
    arbitrary = genericArbitrary
    shrink = genericShrink
