-- | Binary specification for txp-related (Pos.Txp) messages.

module Test.Pos.Txp.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Data.Tagged (Tagged)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)

import           Pos.Arbitrary.Infra ()
import           Pos.Binary ()
import           Pos.Communication.Relay as R
import qualified Pos.Core.Txp as T
import qualified Pos.Txp as T
import           Pos.Util (SmallGenerator)

import           Test.Pos.Cbor.CborSpec (extensionProperty)
import           Test.Pos.Helpers (binaryTest, msgLenLimitedTest)
import           Test.Pos.Util (withDefConfiguration, withDefInfraConfiguration)

spec :: Spec
spec = withDefInfraConfiguration $ withDefConfiguration $
  describe "Txp (transaction processing) system" $ do
    describe "Bi instances" $ do
        describe "Core" $ do
            binaryTest @T.TxIn
            binaryTest @T.TxOut
            binaryTest @T.TxOutAux
            binaryTest @T.Tx
            binaryTest @T.TxInWitness
            binaryTest @T.TxSigData
            binaryTest @T.TxAux
            binaryTest @T.TxProof
            binaryTest @(SmallGenerator T.TxPayload)
        describe "Network" $ do
            binaryTest @(R.InvMsg (Tagged T.TxMsgContents T.TxId))
            binaryTest @(R.ReqMsg (Tagged T.TxMsgContents T.TxId))
            binaryTest @(R.MempoolMsg T.TxMsgContents)
            binaryTest @(R.DataMsg T.TxMsgContents)
    describe "Bi extension" $ do
        prop "TxInWitness" (extensionProperty @T.TxInWitness)
    describe "Message length limit" $ do
        msgLenLimitedTest @(R.InvMsg (Tagged T.TxMsgContents T.TxId))
        msgLenLimitedTest @(R.ReqMsg (Tagged T.TxMsgContents T.TxId))
        msgLenLimitedTest @(R.MempoolMsg T.TxMsgContents)
        -- No check for (DataMsg T.TxMsgContents) since overal message size
        -- is forcely limited
