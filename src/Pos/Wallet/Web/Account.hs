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
       , GenSeed (..)
       , AddrGenSeed
       ) where

import           Data.List                  (elemIndex)
import           Formatting                 (build, sformat, (%))
import           System.Random              (Random, randomIO)
import           Universum

import           Pos.Core                   (Address (..))
import           Pos.Crypto                 (EncryptedSecretKey, PassPhrase,
                                             isNonHardened)
import           Pos.Util                   (maybeThrow)
import           Pos.Util.BackupPhrase      (BackupPhrase, safeKeysFromPhrase)
import           Pos.Wallet.KeyStorage      (MonadKeys, addSecretKey, getSecretKeys)
import           Pos.Wallet.Web.ClientTypes (CAccountAddress (..), CAddress, WS,
                                             WalletAddress (..), addressToCAddress,
                                             encToCAddress, walletAddrByAccount)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.State       (AccountLookupMode (..), WebWalletModeDB,
                                             doesAccountExist, getWalletMeta)
import           Pos.Wallet.Web.Util        (deriveLvl2KeyPair)

type AccountMode m = (MonadKeys m, WebWalletModeDB m, MonadThrow m)

myRootAddresses :: MonadKeys m => m [CAddress WS]
myRootAddresses = encToCAddress <<$>> getSecretKeys

getAddrIdx :: AccountMode m => CAddress WS -> m Int
getAddrIdx addr = elemIndex addr <$> myRootAddresses >>= maybeThrow notFound
  where notFound =
          RequestError $ sformat ("No wallet set with address "%build%" found") addr

getSKByAddr
    :: AccountMode m
    => CAddress WS
    -> m EncryptedSecretKey
getSKByAddr addr = do
    msk <- find (\k -> encToCAddress k == addr) <$> getSecretKeys
    maybeThrow notFound msk
  where notFound =
          RequestError $ sformat ("No wallet set with address "%build%" found") addr

getSKByAccAddr
    :: AccountMode m
    => PassPhrase
    -> CAccountAddress
    -> m EncryptedSecretKey
getSKByAccAddr passphrase accAddr@CAccountAddress {..} = do
    (addr, accKey) <-
        deriveAccountSK passphrase (walletAddrByAccount accAddr) caaAccountIndex
    let accCAddr = addressToCAddress addr
    if accCAddr /= caaId
             -- if you see this error, maybe you generated public key address with
             -- no hd wallet attribute (if so, address would be ~half shorter than
             -- others)
        then throwM . InternalError $ "Account is contradictory!"
        else return accKey

genSaveRootAddress
    :: AccountMode m
    => PassPhrase
    -> BackupPhrase
    -> m (CAddress WS)
genSaveRootAddress passphrase ph = encToCAddress <$> genSaveSK
  where
    genSaveSK = do
        sk <- either keyFromPhraseFailed (pure . fst)
            $ safeKeysFromPhrase passphrase ph
        addSecretKey sk
        return sk
    keyFromPhraseFailed msg =
        throwM . RequestError $ "Key creation from phrase failed: " <> msg

data GenSeed a
    = DeterminedSeed a
    | RandomSeed

type AddrGenSeed = GenSeed Word32   -- with derivation index

generateUnique
    :: (MonadIO m, MonadThrow m, Integral a, Random a)
    => Text -> GenSeed a -> (a -> m b) -> (a -> b -> m Bool) -> m b
generateUnique desc RandomSeed generator isDuplicate = loop (100 :: Int)
  where
    loop 0 = throwM . RequestError $
             sformat (build%": generation of unique item seems too difficult, \
                      \you are approaching the limit") desc
    loop i = do
        rand  <- liftIO randomIO
        value <- generator rand
        bad   <- isDuplicate rand value
        if bad
            then loop (i - 1)
            else return value
generateUnique desc (DeterminedSeed seed) generator notFit = do
    value <- generator (fromIntegral seed)
    whenM (notFit seed value) $
        throwM . InternalError $
        sformat (build%": this value is already taken")
        desc
    return value

genUniqueWalletAddress
    :: AccountMode m
    => AddrGenSeed
    -> CAddress WS
    -> m WalletAddress
genUniqueWalletAddress genSeed wsCAddr =
    generateUnique "wallet generation"
                   genSeed
                   (return . WalletAddress wsCAddr)
                   notFit
  where
    notFit idx addr = andM
        [ pure $ isNonHardened idx
        , isJust <$> getWalletMeta addr
        ]

genUniqueAccountAddress
    :: AccountMode m
    => AddrGenSeed
    -> PassPhrase
    -> WalletAddress
    -> m CAccountAddress
genUniqueAccountAddress genSeed passphrase wCAddr@WalletAddress{..} =
    generateUnique "account generation" genSeed mkAccount notFit
  where
    mkAccount caaAccountIndex =
        deriveAccountAddress passphrase wCAddr caaAccountIndex
    notFit idx addr = andM
        [ pure $ isNonHardened idx
        , doesAccountExist Ever addr
        ]

deriveAccountSK
    :: AccountMode m
    => PassPhrase
    -> WalletAddress
    -> Word32
    -> m (Address, EncryptedSecretKey)
deriveAccountSK passphrase WalletAddress{..} accIndex = do
    -- this function is used in conditions when several secret keys with same
    -- public key are stored, thus checking for passphrase here as well
    let niceSK k = encToCAddress k == waWSId
    key <- maybeThrow noKey . find niceSK =<< getSecretKeys
    maybeThrow badPass $
        deriveLvl2KeyPair passphrase key waIndex accIndex
  where
    noKey   = RequestError "No secret key with such address found"
    badPass = RequestError "Passphrase doesn't match"

deriveAccountAddress
    :: AccountMode m
    => PassPhrase
    -> WalletAddress
    -> Word32
    -> m CAccountAddress
deriveAccountAddress passphrase wAddr@WalletAddress{..} caaAccountIndex = do
    (accAddr, _) <- deriveAccountSK passphrase wAddr caaAccountIndex
    let caaWSId   = waWSId
        caaWalletIndex = waIndex
        caaId     = addressToCAddress accAddr
    return CAccountAddress{..}
