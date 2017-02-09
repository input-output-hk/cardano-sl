-- | Binary specification for txp-related (Pos.Txp) messages.

module Test.Pos.Txp.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec              (Spec, describe)
import           Universum

import           Pos.Binary              ()
import qualified Pos.Communication.Relay as R
import qualified Pos.DB.GState           as GState
import qualified Pos.Txp                 as T
import           Pos.Types               (TxId)
import           Pos.Util                (Limited)

import           Test.Pos.Util           (networkBinaryTest, msgLenLimitedTest)

spec :: Spec
spec =
  describe "Txp (transaction processing) system" $ do
    describe "Bi instances" $ do
      describe "Network" $ do
        networkBinaryTest @(R.InvMsg TxId T.TxMsgTag)
        networkBinaryTest @(R.ReqMsg TxId T.TxMsgTag)
        networkBinaryTest @(R.DataMsg T.TxMsgContents)
    describe "Message length limit" $ do
      msgLenLimitedTest
        @(Limited (R.InvMsg TxId T.TxMsgTag)) GState.getMaxInvSize
      msgLenLimitedTest
        @(Limited (R.ReqMsg TxId T.TxMsgTag)) GState.getMaxReqSize
