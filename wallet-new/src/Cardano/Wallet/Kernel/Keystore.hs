{-- | An opaque handle to a keystore, used to read and write 'PublicKey'
      and 'EncryptedSecretKey' from/to disk.

    NOTE: This module aims to provide a stable interface with a concrete
    implementation concealed by the user of this module. The internal operations
    are currently quite inefficient, as they have to work around the legacy
    'UserPublic' and 'UserSecret' storages.

--}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Kernel.Keystore (
      Keystore -- opaque
    , DeletePolicy(..)
    , ReplaceResult(..)
    , WalletUserKey(..)
      -- * Constructing a keystore
    , bracketKeystore
    , bracketLegacyKeystore
    , bracketTestKeystore
      -- * For import of regular wallet
    , readWalletSecret
      -- * Inserting wallet keys in a keystore
    , insert
      -- * Replacing wallet keys in a keystore, atomically
    , compareAndReplace
      -- * Deleting wallet keys from a keystore
    , delete
      -- * Lookup wallet keys in a keystore, atomically
    , lookup
    ) where

import           Universum

import           Control.Lens (ASetter, Getting)
import           Data.Default (def)
import qualified Data.List
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.FilePath.Posix ((<.>))
import           System.IO (hClose, openTempFile)

import           Pos.Binary.Class (Bi)
import           Pos.Core.NetworkMagic (NetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, PublicKey, hash)
import           Pos.Util.UserPublic (UserPublic, getUPPath, isEmptyUserPublic,
                     takeUserPublic, upKeys, upPath, writeUserPublicRelease)
import qualified Pos.Util.UserPublic as UP
import           Pos.Util.UserSecret (UserSecret, getUSPath, isEmptyUserSecret,
                     readUserSecret, takeUserSecret, usKeys, usWallet,
                     writeUserSecretRelease, _wusRootKey)
import qualified Pos.Util.UserSecret as US
import           Pos.Util.Wlog (addLoggerName)

import           Cardano.Wallet.Kernel.DB.HdWallet (HdRootId, eskToHdRootId,
                     pkToHdRootId)
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import qualified Cardano.Wallet.Kernel.Util.Strict as Strict

-- Internal storage necessary to smooth out the legacy 'UserPublic' and 'UserSecret' APIs.
-- 'UserPublic' contains a list of extended public keys for external wallets, 'UserSecret'
-- contains a list of secret keys for regular wallets.
data InternalStorage = InternalStorage !UserPublic !UserSecret

-- | We are not really interested in fully-forcing internal storages.
-- We are happy here with the operations on the keystore being applied not lazily.
instance NFData InternalStorage where
    rnf x = x `seq` ()

-- A 'Keystore'.
data Keystore = Keystore (Strict.MVar InternalStorage)

-- | A 'DeletePolicy' is a preference the user can express on how to release
-- the 'Keystore' during its teardown.
data DeletePolicy =
      RemoveKeystoreIfEmpty
      -- ^ Completely obliterate the 'Keystore' if is empty, including the
      -- file on disk.
    | KeepKeystoreIfEmpty
      -- ^ Release the 'Keystore' without touching its file on disk, even
      -- if the latter is empty.

-- | Every Cardano wallet has a key which identifies it.
data WalletUserKey =
      RegularWalletKey !EncryptedSecretKey
      -- ^ Regular wallet has a root secret key (generated from backup mnemonic).
    | ExternalWalletKey !PublicKey
      -- ^ External wallet has only extended public key, because its root secret key
      -- is stored externally, for example, in the memory of Ledger device.

{-------------------------------------------------------------------------------
  Creating a keystore
-------------------------------------------------------------------------------}

-- | Creates a 'Keystore' using a 'bracket' pattern, where the
-- initalisation and teardown of the resource are wrapped in 'bracket'.
bracketKeystore :: DeletePolicy
                -- ^ What to do if the keystore is empty
                -> FilePath
                -- ^ The path to the file with public keys, it will be used for the 'Keystore'
                -> FilePath
                -- ^ The path to the file with secret keys, it will be used for the 'Keystore'
                -> (Keystore -> IO a)
                -- ^ An action on the 'Keystore'.
                -> IO a
bracketKeystore deletePolicy fileWithPublic fileWithSecret withKeystore =
    bracket (newKeystore fileWithPublic fileWithSecret)
            (releaseKeystore deletePolicy)
            withKeystore

-- | Creates a new keystore.
newKeystore :: FilePath -> FilePath -> IO Keystore
newKeystore fileWithPublic fileWithSecret = do
    up <- takeUserPublic fileWithPublic
    us <- takeUserSecret fileWithSecret
    Keystore <$> Strict.newMVar (InternalStorage up us)

-- | Reads the legacy root key stored in the specified keystore. This is
-- useful only for importing a wallet using the legacy '.key' format.
readWalletSecret :: FilePath
                 -- ^ The path to the file which will be used for the 'Keystore'
                 -> IO (Maybe EncryptedSecretKey)
readWalletSecret fileWithSecret =
    addLoggerName "KeyStore" $
    importKeystore >>= lookupLegacyRootKey
  where
    lookupLegacyRootKey :: Keystore -> IO (Maybe EncryptedSecretKey)
    lookupLegacyRootKey (Keystore ks) =
        Strict.withMVar ks $ \(InternalStorage _ us) ->
            case us ^. usWallet of
                 Nothing -> return Nothing
                 Just w  -> return (Just $ _wusRootKey w)

    importKeystore :: IO Keystore
    importKeystore = do
        us <- readUserSecret fileWithSecret
        -- | 'fp' contains a path to the file for secret keys.
        -- The function 'readWalletSecret' will be used only for
        -- wallet's import, but importing an /external/ wallet doesn't
        -- make sence, so we don't have a path to the file for public keys.
        -- But it is possible that user will create an external
        -- wallet(s) later, so we cannot provide just default (empty)
        -- path to file for public keys. So let's take the value of 'fp'
        -- and create the similar file.
        let fileWithPublic = fileWithSecret <.> "pub"
            up = (def :: UserPublic) & upPath .~ fileWithPublic
        Keystore <$> Strict.newMVar (InternalStorage up us)

-- | Creates a legacy 'Keystore' by reading the 'UserSecret' from a 'NodeContext'.
-- Hopefully this function will go in the near future.
newLegacyKeystore :: UserPublic -> UserSecret -> IO Keystore
newLegacyKeystore up us = Keystore <$> Strict.newMVar (InternalStorage up us)

-- | Creates a legacy 'Keystore' using a 'bracket' pattern, where the
-- initalisation and teardown of the resource are wrapped in 'bracket'.
-- For a legacy 'Keystore' users do not get to specify a 'DeletePolicy', as
-- the release of the keystore is left for the node and the legacy code
-- themselves.
bracketLegacyKeystore :: UserPublic -> UserSecret -> (Keystore -> IO a) -> IO a
bracketLegacyKeystore up us withKeystore =
    bracket (newLegacyKeystore up us)
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
newTestKeystore = do
    tempDir <- liftIO getTemporaryDirectory
    (tempFileWithPublic, upHdl) <- liftIO $ openTempFile tempDir (fileName <.> "pub")
    (tempFileWithSecret, usHdl) <- liftIO $ openTempFile tempDir (fileName <.> "key")
    liftIO $ hClose upHdl
    liftIO $ hClose usHdl
    up <- takeUserPublic tempFileWithPublic
    us <- takeUserSecret tempFileWithSecret
    Keystore <$> Strict.newMVar (InternalStorage up us)
  where
    fileName = "keystore"

-- | Release the resources associated with this 'Keystore'.
releaseKeystore :: DeletePolicy -> Keystore -> IO ()
releaseKeystore policy (Keystore ks) =
    -- We are not modifying the 'MVar' content, because this function is
    -- not exported and called exactly once from the bracket de-allocation.
    Strict.withMVar ks $ \internalStorage@(InternalStorage up us) -> do
        (fileWithPublic, fileWithSecret) <- release internalStorage
        case policy of
             KeepKeystoreIfEmpty -> return ()
             RemoveKeystoreIfEmpty -> do
                 when (isEmptyUserPublic up) $ removeFile fileWithPublic
                 when (isEmptyUserSecret us) $ removeFile fileWithSecret

-- | Releases the underlying 'InternalStorage' and retuthe updated
-- 'InternalStorage' and the files on disk internal storages live in.
release :: InternalStorage -> IO (FilePath, FilePath)
release (InternalStorage up us) = do
    writeUserPublicRelease up
    writeUserSecretRelease us
    return (fileWithPublic, fileWithSecret)
  where
    fileWithPublic = getUPPath up
    fileWithSecret = getUSPath us

{-------------------------------------------------------------------------------
  Modifying the Keystore
  We wrap each operation which modifies the underlying `InternalStorage` into
  a combinator which also writes the updated `UserPublic` and `UserSecret` to
  corresponding files.
-------------------------------------------------------------------------------}

-- | Modifies the 'Keystore' by applying the transformation 'f' on the
-- underlying 'UserPublic'.
modifyKeystorePublic_ :: Keystore -> (UserPublic -> UserPublic) -> IO ()
modifyKeystorePublic_ ks modifier = modifyKeystorePublic ks f'
  where f' us = (modifier us, ())

-- | Modifies the 'Keystore' by applying the transformation 'f' on the
-- underlying 'UserSecret'.
modifyKeystoreSecret_ :: Keystore -> (UserSecret -> UserSecret) -> IO ()
modifyKeystoreSecret_ ks modifier = modifyKeystoreSecret ks f'
  where f' us = (modifier us, ())

-- | Like 'modifyKeystore_', but it returns a result at the end.
modifyKeystorePublic :: Keystore -> (UserPublic -> (UserPublic, a)) -> IO a
modifyKeystorePublic (Keystore ks) modifier =
    Strict.modifyMVar ks $ \(InternalStorage up us) -> do
        let (modifiedUP, a) = modifier up
        -- This is a safe operation to be because we acquired the exclusive
        -- lock on this file when we initialised the keystore, and as we are
        -- using 'bracket', we are the sole owner of this lock.
        UP.writeRaw modifiedUP
        return (InternalStorage modifiedUP us, a)

-- | Like 'modifyKeystore_', but it returns a result at the end.
modifyKeystoreSecret :: Keystore -> (UserSecret -> (UserSecret, a)) -> IO a
modifyKeystoreSecret (Keystore ks) modifier =
    Strict.modifyMVar ks $ \(InternalStorage up us) -> do
        let (modifiedUS, a) = modifier us
        US.writeRaw modifiedUS
        return (InternalStorage up modifiedUS, a)

{-------------------------------------------------------------------------------
  Inserting things inside a keystore
-------------------------------------------------------------------------------}

-- | Insert a new 'EncryptedSecretKey' indexed by the input 'WalletId'.
insert :: WalletUserKey -> Keystore -> IO ()
insert (RegularWalletKey rootSK) ks  = modifyKeystoreSecret_ ks (insertSecretKey rootSK)
insert (ExternalWalletKey rootPK) ks = modifyKeystorePublic_ ks (insertPublicKey rootPK)

-- | Insert a new 'PublicKey' directly inside the 'UserPublic'.
insertPublicKey :: PublicKey -> UserPublic -> UserPublic
insertPublicKey pk up = insertKey pk upKeys upKeys up

-- | Insert a new 'EncryptedSecretKey' directly inside the 'UserSecret'.
insertSecretKey :: EncryptedSecretKey -> UserSecret -> UserSecret
insertSecretKey esk us = insertKey esk usKeys usKeys us

insertKey :: Bi k
          => k
          -> Getting [k] s [k]
          -> ASetter s s [k] [k]
          -> s
          -> s
insertKey key getter setter storage =
    if view getter storage `containsKey` key
        then storage
        else storage & over setter (key :)
  where
    containsKey :: Bi k => [k] -> k -> Bool
    containsKey storedKeys aKey = hash aKey `elem` map hash storedKeys

-- | Result of secret key replacing.
data ReplaceResult =
      Replaced
    | OldKeyLookupFailed
    | PredicateFailed
    -- ^ The supplied predicate failed.
    deriving (Show, Eq)

-- | Replace an old 'EncryptedSecretKey' with a new one,
-- verifying a pre-condition on the previously stored key.
compareAndReplace :: NetworkMagic
                  -> WalletId
                  -> (EncryptedSecretKey -> Bool)
                  -> EncryptedSecretKey
                  -> Keystore
                  -> IO ReplaceResult
compareAndReplace nm walletId predicateOnOldKey newKey ks =
    modifyKeystoreSecret ks $ \us ->
        let mbOldKey = lookupSecretKey nm us walletId
        in case predicateOnOldKey <$> mbOldKey of
            Nothing ->
                (us, OldKeyLookupFailed)
            Just False ->
                (us, PredicateFailed)
            Just True ->
                (insertSecretKey newKey . tryToDeleteSecretKey nm walletId $ us, Replaced)

{-------------------------------------------------------------------------------
  Looking up things inside a keystore
-------------------------------------------------------------------------------}

-- | Lookup an 'EncryptedSecretKey' associated to the input 'HdRootId'.
lookup :: NetworkMagic
       -> WalletId
       -> Keystore
       -> IO (Maybe WalletUserKey)
lookup nm walletId (Keystore ks) =
    Strict.withMVar ks $ \(InternalStorage up us) -> return $
        -- We don't know if this wallet is a regular one or external one.
        -- So try to find its secret key, then - its public key.
        case lookupSecretKey nm us walletId of
            Just esk -> Just $ RegularWalletKey esk
            Nothing  -> case lookupPublicKey nm up walletId of
                Just pk -> Just $ ExternalWalletKey pk
                Nothing -> Nothing

-- | Lookup a key directly inside the 'UserPublic'.
lookupPublicKey :: NetworkMagic -> UserPublic -> WalletId -> Maybe PublicKey
lookupPublicKey nm up walletId = lookupKey nm up walletId upKeys pkToHdRootId

-- | Lookup a key directly inside the 'UserSecret'.
lookupSecretKey :: NetworkMagic -> UserSecret -> WalletId -> Maybe EncryptedSecretKey
lookupSecretKey nm us walletId = lookupKey nm us walletId usKeys eskToHdRootId

lookupKey :: NetworkMagic
          -> s
          -> WalletId
          -> Getting [k] s [k]
          -> (NetworkMagic -> k -> HdRootId)
          -> Maybe k
lookupKey nm storage (WalletIdHdRnd walletId) getter convert =
    Data.List.find (\key -> convert nm key == walletId) (storage ^. getter)

{-------------------------------------------------------------------------------
  Deleting things from the keystore
-------------------------------------------------------------------------------}

-- | Deletes a key from the 'Keystore'. This is an idempotent operation
-- as in case a key was not present, no error would be thrown.
delete :: NetworkMagic -> WalletId -> Keystore -> IO ()
delete nm walletId ks = do
    -- Since the wallet definitely has secret or public key,
    -- try to remove secret key, then - public one. Obviously
    -- only one of these functions will actually delete the key.
    modifyKeystoreSecret_ ks (tryToDeleteSecretKey nm walletId)
    modifyKeystorePublic_ ks (tryToDeletePublicKey nm walletId)

-- | Try to delete a key directly inside the 'UserSecret'.
tryToDeleteSecretKey :: NetworkMagic -> WalletId -> UserSecret -> UserSecret
tryToDeleteSecretKey nm walletId us = tryToDeleteKey nm walletId us lookupSecretKey usKeys

-- | Try to delete a key directly inside the 'UserPublic'.
tryToDeletePublicKey :: NetworkMagic -> WalletId -> UserPublic -> UserPublic
tryToDeletePublicKey nm walletId up = tryToDeleteKey nm walletId up lookupPublicKey upKeys

tryToDeleteKey :: Bi k
               => NetworkMagic
               -> WalletId
               -> s
               -> (NetworkMagic -> s -> WalletId -> Maybe k)
               -> ASetter s s [k] [k]
               -> s
tryToDeleteKey nm walletId storage finder setter =
    let mbKey = finder nm storage walletId
        erase = Data.List.deleteBy (\k1 k2 -> hash k1 == hash k2)
    in maybe storage (\key -> storage & over setter (erase key)) mbKey
