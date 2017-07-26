-- | Binary specification for txp-related (Pos.Txp) messages.

module Test.Pos.Txp.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Data.Tagged             (Tagged)
import           Test.Hspec              (Spec, describe)

import           Pos.Arbitrary.Infra     ()
import           Pos.Arbitrary.Txp       (SmallTxPayload)
import           Pos.Binary              ()
import           Pos.Communication.Relay as R
import qualified Pos.Txp                 as T

import           Test.Pos.Util           (binaryTest, msgLenLimitedTest,
                                          networkBinaryTest)

spec :: Spec
spec =
  describe "Txp (transaction processing) system" $ do
    describe "Bi instances" $ do
      describe "Core" $ do
          binaryTest @T.TxIn
          binaryTest @T.TxOut
          binaryTest @T.TxOutAux
          binaryTest @T.Tx
          binaryTest @T.TxInWitness
          binaryTest @T.TxDistribution
          binaryTest @T.TxSigData
          binaryTest @T.TxAux
          binaryTest @T.TxProof
          binaryTest @SmallTxPayload
      describe "Network" $ do
        networkBinaryTest @(R.InvMsg (Tagged T.TxMsgContents T.TxId))
        networkBinaryTest @(R.ReqMsg (Tagged T.TxMsgContents T.TxId))
        networkBinaryTest @(R.MempoolMsg T.TxMsgContents)
        networkBinaryTest @(R.DataMsg T.TxMsgContents)
    describe "Message length limit" $ do
      msgLenLimitedTest @(R.InvMsg (Tagged T.TxMsgContents T.TxId))
      msgLenLimitedTest @(R.ReqMsg (Tagged T.TxMsgContents T.TxId))
      msgLenLimitedTest @(R.MempoolMsg T.TxMsgContents)
      -- No check for (DataMsg T.TxMsgContents) since overal message size
      -- is forcely limited
