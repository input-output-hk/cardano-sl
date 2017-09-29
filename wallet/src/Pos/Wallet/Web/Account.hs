-- | Helpers for Wallet Set, Wallet and Account.

module Pos.Wallet.Web.Account
       ( myRootAddresses
       , getAddrIdx
       , getSKById
       , getSKByAccAddr
       , genSaveRootKey
       , genUniqueAccountId
       , genUniqueAccountAddress
       , deriveAccountSK
       , deriveAccountAddress
       , AccountMode
       , GenSeed (..)
       , AddrGenSeed

       , MonadKeySearch (..)
       ) where

import           Data.List                  (elemIndex)
import           Formatting                 (build, sformat, (%))
import           System.Random              (randomIO)
import           System.Wlog                (WithLogger)
import           Universum

import           Pos.Core                   (Address (..), IsBootstrapEraAddr (..),
                                             deriveLvl2KeyPair)
import           Pos.Crypto                 (EncryptedSecretKey, PassPhrase, isHardened)
import           Pos.Util                   (maybeThrow)
import           Pos.Util.BackupPhrase      (BackupPhrase, safeKeysFromPhrase)
import           Pos.Util.Servant           (encodeCType)
import           Pos.Wallet.KeyStorage      (MonadKeys, addSecretKey, getSecretKeys)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CId, CWAddressMeta (..), Wal,
                                             addrMetaToAccount, encToCId)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.State       (AddressLookupMode (Ever), WebWalletModeDB,
                                             doesWAddressExist, getAccountMeta)

type AccountMode ctx m =
    ( MonadCatch m
    , WithLogger m
    , MonadKeys ctx m
    , WebWalletModeDB ctx m
    )

myRootAddresses :: MonadKeys ctx m => m [CId Wal]
myRootAddresses = encToCId <<$>> getSecretKeys

getAddrIdx :: AccountMode ctx m => CId Wal -> m Int
getAddrIdx addr = elemIndex addr <$> myRootAddresses >>= maybeThrow notFound
  where notFound =
          RequestError $ sformat ("No wallet with address "%build%" found") addr

getSKById
    :: AccountMode ctx m
    => CId Wal
    -> m EncryptedSecretKey
getSKById wid = do
    msk <- find (\k -> encToCId k == wid) <$> getSecretKeys
    maybeThrow notFound msk
  where notFound =
          RequestError $ sformat ("No wallet with address "%build%" found") wid

getSKByAccAddr
    :: AccountMode ctx m
    => IsBootstrapEraAddr
    -> PassPhrase
    -> CWAddressMeta
    -> m EncryptedSecretKey
getSKByAccAddr ibe passphrase addrMeta@CWAddressMeta {..} = do
    (addr, accKey) <-
        deriveAccountSK ibe passphrase (addrMetaToAccount addrMeta) cwamAccountIndex
    let accCAddr = encodeCType addr
    if accCAddr /= cwamId
             -- if you see this error, maybe you generated public key address with
             -- no hd wallet attribute (if so, address would be ~half shorter than
             -- others)
        then throwM . InternalError $ "Account is contradictory!"
        else return accKey

genSaveRootKey
    :: AccountMode ctx m
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
        rand  <- liftIO randomIO
        value <- generator rand
        bad   <- orM
            [ isDuplicate rand value
            , pure $ isHardened rand  -- using hardened keys only for now
            ]
        if bad
            then loop (i - 1)
            else return value
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

genUniqueAccountAddress
    :: AccountMode ctx m
    => IsBootstrapEraAddr
    -> AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CWAddressMeta
genUniqueAccountAddress ibe genSeed passphrase wCAddr@AccountId{..} =
    generateUnique "address generation" genSeed mkAccount notFit
  where
    mkAccount cwamAccountIndex =
        deriveAccountAddress ibe passphrase wCAddr cwamAccountIndex
    notFit _idx addr = doesWAddressExist Ever addr

deriveAccountSK
    :: (AccountMode ctx m)
    => IsBootstrapEraAddr
    -> PassPhrase
    -> AccountId
    -> Word32
    -> m (Address, EncryptedSecretKey)
deriveAccountSK ibe passphrase AccountId {..} accIndex = do
    key <- getSKById aiWId
    maybeThrow badPass $
        deriveLvl2KeyPair
            ibe
            passphrase
            key
            aiIndex
            accIndex
  where
    badPass = RequestError "Passphrase doesn't match"

deriveAccountAddress
    :: AccountMode ctx m
    => IsBootstrapEraAddr
    -> PassPhrase
    -> AccountId
    -> Word32
    -> m CWAddressMeta
deriveAccountAddress ibe passphrase accId@AccountId{..} cwamAccountIndex = do
    (addr, _) <- deriveAccountSK ibe passphrase accId cwamAccountIndex
    let cwamWId         = aiWId
        cwamWalletIndex = aiIndex
        cwamId          = encodeCType addr
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
