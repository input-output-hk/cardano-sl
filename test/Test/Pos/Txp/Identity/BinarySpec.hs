-- | Binary specification for txp-related (Pos.Txp) messages.

module Test.Pos.Txp.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec              (Spec, describe)
import           Universum

import           Pos.Binary              ()
import           Pos.Communication.Relay as R
import qualified Pos.Txp                 as T
import           Pos.Types               (TxId)

import           Test.Pos.Util           (networkBinaryTest)

spec :: Spec
spec =
  describe "Txp (transaction processing) system" $ do
    describe "Bi instances" $ do
      describe "Network" $ do
        networkBinaryTest @(R.InvMsg TxId T.TxMsgTag)
        networkBinaryTest @(R.ReqMsg TxId T.TxMsgTag)
        networkBinaryTest @(R.DataMsg T.TxMsgContents)
