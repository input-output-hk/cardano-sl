-- | Helpers for Wallet Set, Wallet and Account.

module Pos.Wallet.Web.Account
       ( myRootAddresses
       , getAddrIdx
       , getSKByAddr
       , getSKByAccAddr
       , genSaveRootAddress
       , genUniqueWalletAddress
       , genUniqueAccountAddress
       , deriveAccountSK
       , deriveAccountAddress
       , AccountMode
       ) where

import           Data.Bits                  (setBit)
import           Data.List                  (elemIndex, (!!))
import           Formatting                 (build, sformat, (%))
import           Pos.Util                   (maybeThrow)
import           System.Random              (Random, randomIO)
import           Universum

import           Pos.Core                   (Address (..), createHDAddressH,
                                             makePubKeyAddress)
import           Pos.Crypto                 (EncryptedSecretKey, PassPhrase,
                                             deriveHDPassphrase, deriveHDSecretKey,
                                             encToPublic)
import           Pos.Util.BackupPhrase      (BackupPhrase, safeKeysFromPhrase)
import           Pos.Wallet.KeyStorage      (MonadKeys (..))
import           Pos.Wallet.Web.ClientTypes (CAccountAddress (..), CAddress,
                                             CWalletAddress (..), WS, addressToCAddress,
                                             walletAddrByAccount)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.State       (AccountLookupMode (..), WebWalletModeDB,
                                             doesAccountExist, getWalletMeta)

type AccountMode m = (MonadKeys m, WebWalletModeDB m, MonadThrow m)

myRootAddresses :: MonadKeys m => m [CAddress WS]
myRootAddresses =
    addressToCAddress . makePubKeyAddress . encToPublic <<$>> getSecretKeys

getAddrIdx :: AccountMode m => CAddress WS -> m Int
getAddrIdx addr = elemIndex addr <$> myRootAddresses >>= maybeThrow notFound
  where notFound =
          Internal $ sformat ("Address "%build%" is not found in wallet") addr

getSKByAddr
    :: AccountMode m
    => CAddress WS
    -> m EncryptedSecretKey
getSKByAddr cAddr = do
    idx <- getAddrIdx cAddr
    sks <- getSecretKeys
    let sk = sks !! idx
    return sk

getSKByAccAddr
    :: AccountMode m
    => PassPhrase
    -> CAccountAddress
    -> m EncryptedSecretKey
getSKByAccAddr passphrase accAddr@CAccountAddress {..} = do
    (addr, accKey) <-
        deriveAccountSK passphrase (walletAddrByAccount accAddr) caaAccountIndex
    let accCAddr = addressToCAddress addr
    if accCAddr /= caaAddress
        then throwM . Internal $ "Account is contradictory!"
        else return accKey

genSaveRootAddress
    :: AccountMode m
    => PassPhrase
    -> BackupPhrase
    -> m (CAddress WS)
genSaveRootAddress passphrase ph =
    addressToCAddress . makePubKeyAddress . encToPublic <$> genSaveSK
  where
    genSaveSK = do
        sk <- either keyFromPhraseFailed (pure . fst)
            $ safeKeysFromPhrase passphrase ph
        addSecretKey sk
        return sk
    keyFromPhraseFailed msg = throwM . Internal $ "Key creation from phrase failed: " <> msg

generateUnique :: (MonadIO m, Random a) => (a -> m b) -> (b -> m Bool) -> m b
generateUnique generator isDuplicate = loop
  where
    loop = do
        rand  <- liftIO randomIO
        value <- generator rand
        bad   <- isDuplicate value
        if bad
            then loop
            else return value

nonHardenedOnly :: Word32 -> Word32
nonHardenedOnly index = setBit index 31

genUniqueWalletAddress
    :: AccountMode m
    => CAddress WS
    -> m CWalletAddress
genUniqueWalletAddress wsCAddr =
    generateUnique (return . CWalletAddress wsCAddr . nonHardenedOnly)
                   (fmap isJust . getWalletMeta)

genUniqueAccountAddress
    :: AccountMode m
    => PassPhrase
    -> CWalletAddress
    -> m CAccountAddress
genUniqueAccountAddress passphrase wCAddr@CWalletAddress{..} =
    generateUnique (mkAccount . nonHardenedOnly)
                   (doesAccountExist Ever)
  where
    mkAccount caaAccountIndex =
        deriveAccountAddress passphrase wCAddr caaAccountIndex

deriveAccountSK
    :: AccountMode m
    => PassPhrase
    -> CWalletAddress
    -> Word32
    -> m (Address, EncryptedSecretKey)
deriveAccountSK passphrase CWalletAddress{..} accIndex = do
    wsKey     <- getSKByAddr cwaWSAddress
    let wKey   = deriveHDSecretKey passphrase wsKey cwaIndex
    let hdPass = deriveHDPassphrase $ encToPublic wsKey
    return $ createHDAddressH passphrase hdPass wKey [cwaIndex] accIndex

deriveAccountAddress
    :: AccountMode m
    => PassPhrase
    -> CWalletAddress
    -> Word32
    -> m CAccountAddress
deriveAccountAddress passphrase wAddr@CWalletAddress{..} caaAccountIndex = do
    (accAddr, _) <- deriveAccountSK passphrase wAddr caaAccountIndex
    let caaWSAddress   = cwaWSAddress
        caaWalletIndex = cwaIndex
        caaAddress     = addressToCAddress accAddr
    return CAccountAddress{..}


