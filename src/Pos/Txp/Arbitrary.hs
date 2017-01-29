{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for 'Txp' networking types.

module Pos.Txp.Arbitrary () where

import           Data.DeriveTH               (derive, makeArbitrary)
import           Test.QuickCheck             (Arbitrary (..))
import           Universum

import           Pos.Binary.Update           ()
import           Pos.Txp.Types.Communication (TxMsgContents (..), TxMsgTag (..))
import           Pos.Types.Arbitrary         ()
import           Pos.Update.Arbitrary.Core   ()
import           Pos.Util.Relay              (DataMsg (..))


derive makeArbitrary ''TxMsgTag
derive makeArbitrary ''TxMsgContents

instance Arbitrary (DataMsg TxMsgContents) where
    arbitrary = DataMsg <$> arbitrary
