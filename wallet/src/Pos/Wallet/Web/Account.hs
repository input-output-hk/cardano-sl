-- | Helpers for Wallet Set, Wallet and Account.

module Pos.Wallet.Web.Account
       ( myRootAddresses
       , getSKById
       , getSKByAddress
       , getSKByAddressPure
       , genSaveRootKey
       , nextAccountId
       , nextAddress
       , deriveAddressSK
       , deriveAddress
       , AccountMode

       , MonadKeySearch (..)
       ) where

import           Control.Monad.Except (MonadError (throwError), runExceptT)
import           Formatting (build, sformat, (%))
import           System.Wlog (WithLogger)
import           Universum

import           Pos.Client.KeyStorage (AllUserSecrets (..), MonadKeys, MonadKeysRead, addSecretKey,
                                        getSecretKeys, getSecretKeysPlain)
import           Pos.Core (Address (..), IsBootstrapEraAddr (..), deriveLvl2KeyPair)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, ShouldCheckPassphrase (..))
import           Pos.Util (eitherToThrow)
import           Pos.Util.BackupPhrase (BackupPhrase, safeKeysFromPhrase)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CId, CWalletMeta(..), CWAddressMeta (..), Wal,
                                             CAccountMeta(..),
                                             addrMetaToAccount, addressToCId, encToCId)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (MonadWalletDB, getAccountMeta, setAccountMeta,
                                       getWalletMetaIncludeUnready, setWalletMeta)

type AccountMode ctx m =
    ( MonadThrow m
    , WithLogger m
    , MonadKeysRead m
    , MonadWalletDB ctx m
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

nextAccountId
    :: AccountMode ctx m
    => CId Wal                   -- FIXME Confusing name
    -> m (Either Text AccountId)
nextAccountId walletId = do
  walletMetaMay <- getWalletMetaIncludeUnready True walletId
  case walletMetaMay of
      Just walletMeta -> do
          updateWalletMeta walletMeta (cwAccountIndex walletMeta + 1)
      Nothing -> do
          return $ Left "No wallet was found"
  where
    updateWalletMeta walletMeta nextAccountIndex
        | nextAccountIndex > maxBound = do
            return $ Left "Account index max bound exceeded"
        | otherwise = do
            let nextWalletMeta = walletMeta { cwAccountIndex = nextAccountIndex }
            setWalletMeta walletId nextWalletMeta
            return $ Right AccountId
                { aiWId   = walletId
                , aiIndex = cwAccountIndex walletMeta
                }

nextAddress
    :: AccountMode ctx m
    => AccountId
    -> PassPhrase
    -> m (Either Text CWAddressMeta)
nextAddress accountId passphrase = do
    accountMetaMay <- getAccountMeta accountId
    case accountMetaMay of
        Just accountMeta -> do
            updateAccountMeta accountMeta (caAddressIndex accountMeta + 1)
        Nothing -> do
            return $ Left "No account was found"
  where
    updateAccountMeta accountMeta nextAddressIndex
        | nextAddressIndex > maxBound = do
            return $ Left "Address index max bound exceeded"
        | otherwise = do
            let nextAccountMeta = accountMeta { caAddressIndex = nextAddressIndex }
            setAccountMeta accountId nextAccountMeta
            addressMeta <- deriveAddress passphrase accountId (caAddressIndex accountMeta)
            return $ Right addressMeta

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
