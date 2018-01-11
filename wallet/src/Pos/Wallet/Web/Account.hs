-- | Helpers for Wallet Set, Wallet and Account.

module Pos.Wallet.Web.Account
       ( myRootAddresses
       , getSKById
       , getSKByAddress
       , getSKByAddressPure
       , genSaveRootKey
       , genUniqueAccountId
       , genUniqueAddress
       , deriveAddressSK
       , deriveAddress
       , AccountMode
       , GenSeed (..)
       , AddrGenSeed

       , MonadKeySearch (..)
       ) where

import           Control.Monad.Except (MonadError (throwError), runExceptT)
import           Formatting (build, sformat, (%))
import           System.Random (randomRIO)
import           System.Wlog (WithLogger)
import           Universum

import           Pos.Client.KeyStorage (AllUserSecrets (..), MonadKeys, MonadKeysRead, addSecretKey,
                                        getSecretKeys, getSecretKeysPlain)
import           Pos.Core (Address (..), IsBootstrapEraAddr (..), deriveLvl2KeyPair)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, ShouldCheckPassphrase (..),
                             firstHardened)
import           Pos.Util (eitherToThrow)
import           Pos.Util.BackupPhrase (BackupPhrase, safeKeysFromPhrase)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CId, CWAddressMeta (..), Wal,
                                             addrMetaToAccount, addressToCId, encToCId)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (AddressLookupMode (Ever), MonadWalletDBRead,
                                       doesWAddressExist, getAccountMeta)

type AccountMode ctx m =
    ( MonadThrow m
    , WithLogger m
    , MonadKeysRead m
    , MonadWalletDBRead ctx m
    )

myRootAddresses :: MonadKeysRead m => m [CId Wal]
myRootAddresses = encToCId <<$>> getSecretKeysPlain

getSKById
    :: AccountMode ctx m
    => CId Wal
    -> m EncryptedSecretKey
getSKById wid = do
    secrets <- getSecretKeys
    runExceptT (getSKByIdPure secrets wid) >>= eitherToThrow

getSKByIdPure
    :: MonadError WalletError m
    => AllUserSecrets
    -> CId Wal
    -> m EncryptedSecretKey
getSKByIdPure (AllUserSecrets secrets) wid =
    maybe (throwError notFound) pure (find (\k -> encToCId k == wid) secrets)
  where
    notFound =
        RequestError $ sformat ("No wallet with address "%build%" found") wid

getSKByAddress
    :: AccountMode ctx m
    => ShouldCheckPassphrase
    -> PassPhrase
    -> CWAddressMeta
    -> m EncryptedSecretKey
getSKByAddress scp passphrase addrMeta = do
    secrets <- getSecretKeys
    runExceptT (getSKByAddressPure secrets scp passphrase addrMeta) >>= eitherToThrow

getSKByAddressPure
    :: MonadError WalletError m
    => AllUserSecrets
    -> ShouldCheckPassphrase
    -> PassPhrase
    -> CWAddressMeta
    -> m EncryptedSecretKey
getSKByAddressPure secrets scp passphrase addrMeta@CWAddressMeta {..} = do
    (addr, addressKey) <-
            deriveAddressSKPure secrets scp passphrase (addrMetaToAccount addrMeta) cwamAddressIndex
    let accCAddr = addressToCId addr
    if accCAddr /= cwamId
             -- if you see this error, maybe you generated public key address with
             -- no hd wallet attribute (if so, address would be ~half shorter than
             -- others)
        then throwError . InternalError $ "Account is contradictory!"
        else pure addressKey

genSaveRootKey
    :: (AccountMode ctx m, MonadKeys m)
    => PassPhrase
    -> BackupPhrase
    -> m EncryptedSecretKey
genSaveRootKey passphrase ph = do
    sk <- either keyFromPhraseFailed (pure . fst) $
        safeKeysFromPhrase passphrase ph
    addSecretKey sk
    return sk
  where
    keyFromPhraseFailed msg =
        throwM . RequestError $ "Key creation from phrase failed: " <> msg

data GenSeed a
    = DeterminedSeed a
    | RandomSeed

type AddrGenSeed = GenSeed Word32   -- with derivation index

generateUnique
    :: (MonadIO m, MonadThrow m)
    => Text -> AddrGenSeed -> (Word32 -> m b) -> (Word32 -> b -> m Bool) -> m b
generateUnique desc RandomSeed generator isDuplicate = loop (100 :: Int)
  where
    loop 0 = throwM . RequestError $
             sformat (build%": generation of unique item seems too difficult, \
                      \you are approaching the limit") desc
    loop i = do
        rand  <- liftIO $ randomRIO (firstHardened, maxBound)
        value <- generator rand
        isDup <- isDuplicate rand value
        if isDup then
            loop (i - 1)
        else
            return value
generateUnique desc (DeterminedSeed seed) generator notFit = do
    value <- generator (fromIntegral seed)
    whenM (notFit seed value) $
        throwM . InternalError $
        sformat (build%": this index is already taken")
        desc
    return value

genUniqueAccountId
    :: AccountMode ctx m
    => AddrGenSeed
    -> CId Wal
    -> m AccountId
genUniqueAccountId genSeed wsCAddr =
    generateUnique
        "account generation"
        genSeed
        (return . AccountId wsCAddr)
        notFit
  where
    notFit _idx addr = isJust <$> getAccountMeta addr

genUniqueAddress
    :: AccountMode ctx m
    => AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CWAddressMeta
genUniqueAddress genSeed passphrase wCAddr@AccountId{..} =
    generateUnique "address generation" genSeed mkAddress notFit
  where
    mkAddress :: AccountMode ctx m => Word32 -> m CWAddressMeta
    mkAddress cwamAddressIndex =
        deriveAddress passphrase wCAddr cwamAddressIndex
    notFit _idx addr = doesWAddressExist Ever addr

deriveAddressSK
    :: AccountMode ctx m
    => ShouldCheckPassphrase
    -> PassPhrase
    -> AccountId
    -> Word32
    -> m (Address, EncryptedSecretKey)
deriveAddressSK scp passphrase accId addressIndex = do
    secrets <- getSecretKeys
    runExceptT (deriveAddressSKPure secrets scp passphrase accId addressIndex) >>= eitherToThrow

deriveAddressSKPure
    :: MonadError WalletError m
    => AllUserSecrets
    -> ShouldCheckPassphrase
    -> PassPhrase
    -> AccountId
    -> Word32
    -> m (Address, EncryptedSecretKey)
deriveAddressSKPure secrets scp passphrase AccountId {..} addressIndex = do
    key <- getSKByIdPure secrets aiWId
    maybe (throwError badPass) pure $
        deriveLvl2KeyPair
            (IsBootstrapEraAddr True) -- TODO: make it context-dependent!
            scp
            passphrase
            key
            aiIndex
            addressIndex
  where
    badPass = RequestError "Passphrase doesn't match"

deriveAddress
    :: AccountMode ctx m
    => PassPhrase
    -> AccountId
    -> Word32
    -> m CWAddressMeta
deriveAddress passphrase accId@AccountId{..} cwamAddressIndex = do
    (addr, _) <- deriveAddressSK (ShouldCheckPassphrase True) passphrase accId cwamAddressIndex
    let cwamWId         = aiWId
        cwamAccountIndex = aiIndex
        cwamId          = addressToCId addr
    return CWAddressMeta{..}

-- | Allows to find a key related to given @id@ item.
class MonadKeySearch id m where
    findKey :: id -> m EncryptedSecretKey

instance AccountMode ctx m => MonadKeySearch (CId Wal) m where
    findKey = getSKById

instance AccountMode ctx m => MonadKeySearch AccountId m where
    findKey = findKey . aiWId

instance AccountMode ctx m => MonadKeySearch CWAddressMeta m where
    findKey = findKey . cwamWId
