module TxMetaStorageSpecs (txMetaStorageSpecs) where

import           Universum

import           Cardano.Wallet.Kernel.DB.TxMeta
import           Control.Exception.Safe (bracket)

import           Test.Hspec (Expectation, SpecWith, runIO)
import           Test.QuickCheck (arbitrary, generate)
import           Util.Buildable.Hspec

withTemporaryDb :: (MetaDBHandle -> Expectation) -> SpecWith b
withTemporaryDb action = runIO (bracket acquire release action)
    where
       acquire :: MonadIO m => m MetaDBHandle
       acquire = liftIO $ do
           openMetaDB "txmeta_test.db"

       release :: MonadIO m => MetaDBHandle -> m ()
       release = liftIO . closeMetaDB

txMetaStorageSpecs :: Spec
txMetaStorageSpecs = do
    describe "storing a txmeta can be retrieved back" $
        withTemporaryDb $ \hdl -> do
          testMeta <- liftIO $ generate arbitrary
          putTxMeta hdl testMeta
          mbTx <- getTxMeta hdl (testMeta ^. txMetaId)
          mbTx `shouldBe` Just testMeta
