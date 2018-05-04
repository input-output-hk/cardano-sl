{-# LANGUAGE RankNTypes #-}
module TxMetaStorageSpecs (txMetaStorageSpecs) where

import           Universum

import           Cardano.Wallet.Kernel.DB.TxMeta
import           Control.Exception.Safe (bracket, catchJust)

import           System.Directory (getTemporaryDirectory, removeFile, withCurrentDirectory)
import           System.IO.Error (isDoesNotExistError)
import           Test.QuickCheck (arbitrary, generate)
import           Util.Buildable.Hspec

import qualified Cardano.Wallet.Kernel.DB.Sqlite as Storage

-- | Handy combinator which yields a fresh database to work with on each spec.
withTemporaryDb :: forall m a. (MonadIO m, MonadMask m) => (MetaDBHandle -> m a) -> m a
withTemporaryDb action = bracket acquire release action
    where
       acquire :: m MetaDBHandle
       acquire = liftIO $ do
           tmpDir <- getTemporaryDirectory
           withCurrentDirectory tmpDir $ do
               -- Remove any existing db, if any.
               catchJust (\e -> if isDoesNotExistError e then Just () else Nothing)
                         (removeFile "txmeta_test.db")
                         (\_ -> return ())
               db <- openMetaDB "txmeta_test.db"
               Storage.unsafeMigrate db
               return db

       release :: MetaDBHandle -> m ()
       release = liftIO . closeMetaDB

-- | Specs which tests the persistent storage and API provided by 'TxMeta'.
txMetaStorageSpecs :: Spec
txMetaStorageSpecs = do
    describe "TxMeta storage" $ do
        it "can store a TxMeta and retrieve it back" $ do
            withTemporaryDb $ \hdl -> do
                testMeta <- liftIO $ generate arbitrary
                putTxMeta hdl testMeta
                mbTx <- getTxMeta hdl (testMeta ^. txMetaId)
                mbTx `shouldBe` Just testMeta
