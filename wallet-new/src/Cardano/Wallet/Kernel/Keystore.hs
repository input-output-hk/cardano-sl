{-- | An opaque handle to a keystore, used to read and write 'EncryptedSecretKey'
      from/to disk.

    NOTE: This module aims to provide a stable interface with a concrete
    implementation concealed by the user of this module. The internal operations
    are currently quite inefficient, as they have to work around the legacy
    'UserSecret' storage.
--}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Cardano.Wallet.Kernel.Keystore (
      Keystore -- opaque
      -- * Constructing a keystore
    , bracketKeystore
    , bracketLegacyKeystore
    -- * Destroying a keystore (something you rarely should do)
    , releaseAndDestroyKeystore
    -- * Releasing a keystore and its associated resources
    , releaseKeystore
    -- * Inserting values
    , insert
    -- * Deleting values
    , delete
    -- * Queries on a keystore
    , lookup
    -- * Conversions
    , toList
    -- * Tests handy functions
    , newTemporaryKeystore
    -- * Internal and test-only exports.
    , newKeystore
    ) where

import           Universum hiding (toList)

import           Control.Concurrent (modifyMVar_, withMVar)
import           Control.Monad.Trans.Identity (IdentityT (..), runIdentityT)
import qualified Data.List
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.IO (hClose, openTempFile)

import           Pos.Crypto (EncryptedSecretKey, hash)
import           Pos.Util.UserSecret (UserSecret, getUSPath, takeUserSecret,
                     usKeys, writeUserSecretRelease)
import           System.Wlog (CanLog (..), HasLoggerName (..), LoggerName (..),
                     logMessage)

import           Cardano.Wallet.Kernel.DB.HdWallet (eskToHdRootId)
import           Cardano.Wallet.Kernel.Types (WalletId (..))

-- Internal storage necessary to smooth out the legacy 'UserSecret' API.
data InternalStorage =
      StorageInitialised !UserSecret
    | StorageReleased

data Keystore = Keystore (MVar InternalStorage)

data KeystoreOperation =
      KeystoreLookup
    | KeystoreDelete
    | KeystoreInsert
    | KeystoreToList
    | KeystoreRelease
    deriving Show

data KeystoreError =
    KeystoreErrorAlreadyReleased KeystoreOperation
    -- ^ The keystore was already released by the time the requested
    -- operation was attempted.
    deriving Show

instance Exception KeystoreError

-- | Internal monad used to smooth out the 'WithLogger' dependency imposed
-- by 'Pos.Util.UserSecret', to not commit to any way of logging things just yet.
newtype KeystoreM a = KeystoreM { fromKeystore :: IdentityT IO a }
                    deriving (Functor, Applicative, Monad, MonadIO)

instance HasLoggerName KeystoreM where
    askLoggerName = return (LoggerName "Keystore")
    modifyLoggerName _ action = action

instance CanLog KeystoreM where
    dispatchMessage _ln sev txt = logMessage sev txt

{-------------------------------------------------------------------------------
  Creating a keystore
-------------------------------------------------------------------------------}

-- | Creates a 'Keystore' using a 'bracket' pattern, where the
-- initalisation and teardown of the resource are wrapped in 'bracket'.
bracketKeystore :: FilePath -> (Keystore -> IO a) -> IO a
bracketKeystore fp withKeystore =
    bracket (newKeystore fp) releaseKeystore withKeystore

-- | Creates a new keystore.
newKeystore :: FilePath -> IO Keystore
newKeystore fp = runIdentityT $ fromKeystore $ do
    us <- takeUserSecret fp
    Keystore <$> newMVar (StorageInitialised us)

-- | Creates a legacy 'Keystore' by reading the 'UserSecret' from a 'NodeContext'.
-- Hopefully this function will go in the near future.
newLegacyKeystore :: UserSecret -> IO Keystore
newLegacyKeystore us = Keystore <$> newMVar (StorageInitialised us)

-- | Creates a legacy 'Keystore' using a 'bracket' pattern, where the
-- initalisation and teardown of the resource are wrapped in 'bracket'.
bracketLegacyKeystore :: UserSecret -> (Keystore -> IO a) -> IO a
bracketLegacyKeystore us withKeystore =
    bracket (newLegacyKeystore us)
            (\_ -> return ()) -- Leave teardown to the legacy wallet
            withKeystore

-- | Creates a 'Keystore' out of a randomly generated temporary file (i.e.
-- inside your $TMPDIR of choice). Suitable for testing.
-- We don't offer a 'bracket' style here as the teardown is irrelevant, as
-- the file is disposed automatically from being created into the
-- OS' temporary directory.
newTemporaryKeystore :: IO Keystore
newTemporaryKeystore = liftIO $ runIdentityT $ fromKeystore $ do
    tempDir         <- liftIO getTemporaryDirectory
    (tempFile, hdl) <- liftIO $ openTempFile tempDir "keystore.key"
    liftIO $ hClose hdl
    us <- takeUserSecret tempFile
    Keystore <$> newMVar (StorageInitialised us)


-- | Release the resources associated with this 'Keystore'.
-- This function is idempotent and can be called multiple times.
releaseKeystore :: Keystore -> IO ()
releaseKeystore (Keystore ks) = modifyMVar_ ks (fmap fst . release)


-- | Releases the underlying 'InternalStorage' and returns the updated
-- 'InternalStorage' and the file on disk this storage lives in.
-- 'FilePath'.
release :: InternalStorage -> IO (InternalStorage, FilePath)
release internalStorage =
    case internalStorage of
         StorageReleased -> throwM (KeystoreErrorAlreadyReleased KeystoreRelease)
         StorageInitialised us -> do
             let fp = getUSPath us
             writeUserSecretRelease us
             return (StorageReleased, fp)


{-------------------------------------------------------------------------------
  Destroying a keystore
-------------------------------------------------------------------------------}

-- | Destroys a 'Keystore'.
-- Completely obliterate the keystore from disk, with all its secrets.
-- This operation cannot be reverted.
-- This is a very destructive option that most of the time you
-- probably don't want.
-- This has still its use in some teardowns or tests.
-- Note that this operation will always succeed. Use with care.
releaseAndDestroyKeystore :: Keystore -> IO ()
releaseAndDestroyKeystore (Keystore ks) =
    modifyMVar_ ks $ \internalStorage -> do
        (newStorage, fp) <- release internalStorage
        removeFile fp
        return newStorage

{-------------------------------------------------------------------------------
  Inserting things inside a keystore
-------------------------------------------------------------------------------}

-- | Insert a new 'EncryptedSecretKey' indexed by the input 'WalletId'.
insert :: WalletId
       -> EncryptedSecretKey
       -> Keystore
       -> IO ()
insert _walletId esk (Keystore ks) =
    modifyMVar_ ks $ \case
        StorageInitialised us -> do
           return . StorageInitialised $
               if view usKeys us `contains` esk
                        then us
                        else us & over usKeys (esk :)
        StorageReleased ->
            throwM (KeystoreErrorAlreadyReleased KeystoreInsert)
    where
      -- Comparator taken from the old code which needs to hash
      -- all the 'EncryptedSecretKey' in order to compare them.
      contains :: [EncryptedSecretKey] -> EncryptedSecretKey -> Bool
      contains ls k = hash k `elem` map hash ls

{-------------------------------------------------------------------------------
  Looking up things inside a keystore
-------------------------------------------------------------------------------}

-- | Lookup an 'EncryptedSecretKey' associated to the input 'HdRootId'.
lookup :: WalletId
       -> Keystore
       -> IO (Maybe EncryptedSecretKey)
lookup wId (Keystore ks) =
    withMVar ks $ \case
        StorageInitialised us -> return $ lookupKey us wId
        StorageReleased ->
            throwM (KeystoreErrorAlreadyReleased KeystoreLookup)


lookupKey :: UserSecret -> WalletId -> Maybe EncryptedSecretKey
lookupKey us (WalletIdHdRnd walletId) =
    Data.List.find (\k -> eskToHdRootId k == walletId) (us ^. usKeys)

{-------------------------------------------------------------------------------
  Deleting things from the keystore
-------------------------------------------------------------------------------}
delete :: WalletId
       -> Keystore
       -> IO ()
delete walletId (Keystore ks) = do
    modifyMVar_ ks $ \case
        StorageReleased ->
            throwM (KeystoreErrorAlreadyReleased KeystoreDelete)
        StorageInitialised us -> do
           let mbEsk = lookupKey us walletId
           let erase = Data.List.deleteBy (\a b -> hash a == hash b)
           let us' = maybe us (\esk -> us & over usKeys (erase esk)) mbEsk
           return (StorageInitialised us')

{-------------------------------------------------------------------------------
  Converting a Keystore into container types
-------------------------------------------------------------------------------}

-- | Returns all the 'EncryptedSecretKey' known to this 'Keystore'.
toList :: Keystore -> IO [(WalletId, EncryptedSecretKey)]
toList (Keystore ks) =
    withMVar ks $ \case
        StorageReleased ->
            throwM (KeystoreErrorAlreadyReleased KeystoreToList)
        StorageInitialised us -> do
           let kss = us ^. usKeys
           return $ map (\k -> (WalletIdHdRnd (eskToHdRootId k), k)) kss
