{-# LANGUAGE LambdaCase #-}

-- | Binary specification for txp-related (Pos.Txp) messages.

module Test.Pos.Txp.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec               (Spec, describe)
import           Universum

import           Pos.Binary               ()
import           Pos.Communication.Relay  as R
import qualified Pos.Txp                  as T
import           Test.Pos.Arbitrary.Infra ()

import           Test.Pos.Util            (msgLenLimitedTest, networkBinaryTest)

spec :: Spec
spec =
  describe "Txp (transaction processing) system" $ do
    describe "Bi instances" $ do
      describe "Network" $ do
        networkBinaryTest @(R.InvMsg T.TxId T.TxMsgTag)
        networkBinaryTest @(R.ReqMsg T.TxId T.TxMsgTag)
        networkBinaryTest @(R.DataMsg T.TxMsgContents)
    describe "Message length limit" $ do
      msgLenLimitedTest @(R.InvMsg T.TxId T.TxMsgTag)
      msgLenLimitedTest @(R.ReqMsg T.TxId T.TxMsgTag)
      -- No check for (DataMsg T.TxMsgContents) since overal message size
      -- is forcely limited
