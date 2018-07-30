{-- | An opaque handle to a keystore, used to read and write 'EncryptedSecretKey'
      from/to disk.

    NOTE: This module aims to provide a stable interface with a concrete
    implementation concealed by the user of this module. The internal operations
    are currently quite inefficient, as they have to work around the legacy
    'UserSecret' storage.

--}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Kernel.Keystore (
      Keystore -- opaque
    , DeletePolicy(..)
      -- * Constructing a keystore
    , bracketKeystore
    , bracketLegacyKeystore
    -- * Inserting values
    , insert
    , replace
    -- * Deleting values
    , delete
    -- * Queries on a keystore
    , lookup
    -- * Conversions
    , toList
    -- * Tests handy functions
    , bracketTestKeystore
    ) where

import           Universum hiding (toList)

import           Control.Concurrent (modifyMVar_, withMVar)
import           Control.Monad.Trans.Identity (IdentityT (..), runIdentityT)
import qualified Data.List
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.IO (hClose, openTempFile)

import           Pos.Crypto (EncryptedSecretKey, hash)
import           Pos.Util.UserSecret (UserSecret, getUSPath, isEmptyUserSecret,
                     takeUserSecret, usKeys, writeUserSecretRelease)
import           System.Wlog (CanLog (..), HasLoggerName (..), LoggerName (..),
                     logMessage)

import           Cardano.Wallet.Kernel.DB.HdWallet (eskToHdRootId)
import           Cardano.Wallet.Kernel.Types (WalletId (..))

-- Internal storage necessary to smooth out the legacy 'UserSecret' API.
data InternalStorage = InternalStorage !UserSecret

-- A 'Keystore'.
data Keystore = Keystore (MVar InternalStorage)

-- | Internal monad used to smooth out the 'WithLogger' dependency imposed
-- by 'Pos.Util.UserSecret', to not commit to any way of logging things just yet.
newtype KeystoreM a = KeystoreM { fromKeystore :: IdentityT IO a }
                    deriving (Functor, Applicative, Monad, MonadIO)

instance HasLoggerName KeystoreM where
    askLoggerName = return (LoggerName "Keystore")
    modifyLoggerName _ action = action

instance CanLog KeystoreM where
    dispatchMessage _ln sev txt = logMessage sev txt

-- | A 'DeletePolicy' is a preference the user can express on how to release
-- the 'Keystore' during its teardown.
data DeletePolicy =
      RemoveKeystoreIfEmpty
      -- ^ Completely obliterate the 'Keystore' if is empty, including the
      -- file on disk.
    | KeepKeystoreIfEmpty
      -- ^ Release the 'Keystore' without touching its file on disk, even
      -- if the latter is empty.

{-------------------------------------------------------------------------------
  Creating a keystore
-------------------------------------------------------------------------------}

-- FIXME [CBR-316] Due to the current, legacy 'InternalStorage' being
-- used, the 'Keystore' does not persist the in-memory content on disk after
-- every destructive operations, which means that in case of a node crash or
-- another catastrophic behaviour (e.g. power loss, etc) the in-memory content
-- not yet written in memory will be forever lost.

-- | Creates a 'Keystore' using a 'bracket' pattern, where the
-- initalisation and teardown of the resource are wrapped in 'bracket'.
bracketKeystore :: DeletePolicy
                -- ^ What to do if the keystore is empty
                -> FilePath
                -- ^ The path to the file which will be used for the 'Keystore'
                -> (Keystore -> IO a)
                -- ^ An action on the 'Keystore'.
                -> IO a
bracketKeystore deletePolicy fp withKeystore =
    bracket (newKeystore fp) (releaseKeystore deletePolicy) withKeystore

-- | Creates a new keystore.
newKeystore :: FilePath -> IO Keystore
newKeystore fp = runIdentityT $ fromKeystore $ do
    us <- takeUserSecret fp
    Keystore <$> newMVar (InternalStorage us)

-- | Creates a legacy 'Keystore' by reading the 'UserSecret' from a 'NodeContext'.
-- Hopefully this function will go in the near future.
newLegacyKeystore :: UserSecret -> IO Keystore
newLegacyKeystore us = Keystore <$> newMVar (InternalStorage us)

-- | Creates a legacy 'Keystore' using a 'bracket' pattern, where the
-- initalisation and teardown of the resource are wrapped in 'bracket'.
-- For a legacy 'Keystore' users do not get to specify a 'DeletePolicy', as
-- the release of the keystore is left for the node and the legacy code
-- themselves.
bracketLegacyKeystore :: UserSecret -> (Keystore -> IO a) -> IO a
bracketLegacyKeystore us withKeystore =
    bracket (newLegacyKeystore us)
            (\_ -> return ()) -- Leave teardown to the legacy wallet
            withKeystore

bracketTestKeystore :: (Keystore -> IO a) -> IO a
bracketTestKeystore withKeystore =
    bracket newTestKeystore
            (releaseKeystore RemoveKeystoreIfEmpty)
            withKeystore

-- | Creates a 'Keystore' out of a randomly generated temporary file (i.e.
-- inside your $TMPDIR of choice).
-- We don't offer a 'bracket' style here as the teardown is irrelevant, as
-- the file is disposed automatically from being created into the
-- OS' temporary directory.
-- NOTE: This 'Keystore', as its name implies, shouldn't be using in
-- production, but only for testing, as it can even possibly contain data
-- races due to the fact its underlying file is stored in the OS' temporary
-- directory.
newTestKeystore :: IO Keystore
newTestKeystore = liftIO $ runIdentityT $ fromKeystore $ do
    tempDir         <- liftIO getTemporaryDirectory
    (tempFile, hdl) <- liftIO $ openTempFile tempDir "keystore.key"
    liftIO $ hClose hdl
    us <- takeUserSecret tempFile
    Keystore <$> newMVar (InternalStorage us)

-- | Release the resources associated with this 'Keystore'.
releaseKeystore :: DeletePolicy -> Keystore -> IO ()
releaseKeystore dp (Keystore ks) =
    -- We are not modifying the 'MVar' content, because this function is
    -- not exported and called exactly once from the bracket de-allocation.
    withMVar ks $ \internalStorage@(InternalStorage us) -> do
        fp <- release internalStorage
        case dp of
             KeepKeystoreIfEmpty   -> return ()
             RemoveKeystoreIfEmpty ->
                 when (isEmptyUserSecret us) $ removeFile fp

-- | Releases the underlying 'InternalStorage' and returns the updated
-- 'InternalStorage' and the file on disk this storage lives in.
-- 'FilePath'.
release :: InternalStorage -> IO FilePath
release (InternalStorage us) = do
    let fp = getUSPath us
    writeUserSecretRelease us
    return fp

{-------------------------------------------------------------------------------
  Inserting things inside a keystore
-------------------------------------------------------------------------------}

-- | Insert a new 'EncryptedSecretKey' indexed by the input 'WalletId'.
insert :: WalletId
       -> EncryptedSecretKey
       -> Keystore
       -> IO ()
insert _walletId esk (Keystore ks) =
    modifyMVar_ ks $ \(InternalStorage us) -> do
        return . InternalStorage . insertKey esk $ us

-- | Insert a new 'EncryptedSecretKey' directly inside the 'UserSecret'.
insertKey :: EncryptedSecretKey -> UserSecret -> UserSecret
insertKey esk us =
    if view usKeys us `contains` esk
        then us
        else us & over usKeys (esk :)
    where
      -- Comparator taken from the old code which needs to hash
      -- all the 'EncryptedSecretKey' in order to compare them.
      contains :: [EncryptedSecretKey] -> EncryptedSecretKey -> Bool
      contains ls k = hash k `elem` map hash ls

-- | Replace an old 'EncryptedSecretKey' with a new one.
replace :: WalletId
        -> EncryptedSecretKey
        -> Keystore
        -> IO ()
replace walletId esk (Keystore ks) =
    modifyMVar_ ks $ \(InternalStorage us) -> do
        return . InternalStorage . insertKey esk . deleteKey walletId $ us

{-------------------------------------------------------------------------------
  Looking up things inside a keystore
-------------------------------------------------------------------------------}

-- | Lookup an 'EncryptedSecretKey' associated to the input 'HdRootId'.
lookup :: WalletId
       -> Keystore
       -> IO (Maybe EncryptedSecretKey)
lookup wId (Keystore ks) =
    withMVar ks $ \(InternalStorage us) -> return $ lookupKey us wId

-- | Lookup a key directly inside the 'UserSecret'.
lookupKey :: UserSecret -> WalletId -> Maybe EncryptedSecretKey
lookupKey us (WalletIdHdRnd walletId) =
    Data.List.find (\k -> eskToHdRootId k == walletId) (us ^. usKeys)

{-------------------------------------------------------------------------------
  Deleting things from the keystore
-------------------------------------------------------------------------------}

-- | Deletes an element from the 'Keystore'. This is an idempotent operation
-- as in case a key was not present, no error would be thrown.
delete :: WalletId -> Keystore -> IO ()
delete walletId (Keystore ks) = do
    modifyMVar_ ks $ \(InternalStorage us) -> do
        return (InternalStorage $ deleteKey walletId us)

-- | Delete a key directly inside the 'UserSecret'.
deleteKey :: WalletId -> UserSecret -> UserSecret
deleteKey walletId us =
    let mbEsk = lookupKey us walletId
        erase = Data.List.deleteBy (\a b -> hash a == hash b)
    in maybe us (\esk -> us & over usKeys (erase esk)) mbEsk

{-------------------------------------------------------------------------------
  Converting a Keystore into container types
-------------------------------------------------------------------------------}

-- | Returns all the 'EncryptedSecretKey' known to this 'Keystore'.
toList :: Keystore -> IO [(WalletId, EncryptedSecretKey)]
toList (Keystore ks) =
    withMVar ks $ \(InternalStorage us) -> do
        let kss = us ^. usKeys
        return $ map (\k -> (WalletIdHdRnd (eskToHdRootId k), k)) kss
