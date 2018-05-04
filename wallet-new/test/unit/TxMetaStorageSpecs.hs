{-# LANGUAGE RankNTypes #-}
module TxMetaStorageSpecs (txMetaStorageSpecs) where

import           Universum

import           Cardano.Wallet.Kernel.DB.TxMeta
import           Control.Exception.Safe (bracket, catchJust)

import           System.Directory (getTemporaryDirectory, removeFile, withCurrentDirectory)
import           System.IO.Error (isDoesNotExistError)
import           Test.HUnit ((@?))
import           Test.QuickCheck (arbitrary, generate)
import           Util.Buildable.Hspec

import qualified Cardano.Wallet.Kernel.DB.Sqlite as Storage
import qualified Pos.Core as Core

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


isDuplicateTxError :: Core.TxId -> Either TxMetaStorageError a -> Bool
isDuplicateTxError txid (Left (InvariantViolated (DuplicatedTransaction txid'))) =
  txid == txid'
isDuplicateTxError _ _ = False

-- | Specs which tests the persistent storage and API provided by 'TxMeta'.
txMetaStorageSpecs :: Spec
txMetaStorageSpecs = do
    describe "TxMeta storage" $ do

        it "can store a TxMeta and retrieve it back" $ do
            withTemporaryDb $ \hdl -> do
                testMeta <- liftIO $ generate arbitrary
                void $ putTxMeta hdl testMeta
                mbTx <- getTxMeta hdl (testMeta ^. txMetaId)
                mbTx `shouldBe` Just testMeta

        it "inserting the same tx twice yields a DuplicatedTransaction error" $ do
            withTemporaryDb $ \hdl -> do
                testMeta <- liftIO $ generate arbitrary

                insert1 <- putTxMeta hdl testMeta
                isLeft insert1 `shouldBe` False

                insert2 <- putTxMeta hdl testMeta
                isLeft insert2 @? "Expecting insert2 to yield Left, but it didn't"
                isDuplicateTxError (testMeta ^. txMetaId) insert2 @? "Not a DuplicatedTransaction error"

