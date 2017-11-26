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
       , GenerationMode (..)
       , AddressGenerationMode

       , MonadKeySearch (..)
       ) where

import           Control.Monad.Except (MonadError (throwError), runExceptT)
import           Formatting (build, sformat, (%))
import           System.Random (randomIO)
import           System.Wlog (WithLogger)
import           Universum

import           Pos.Client.KeyStorage (AllUserSecrets (..), MonadKeys, MonadKeysRead, addSecretKey,
                                        getSecretKeys, getSecretKeysPlain)
import           Pos.Core (Address (..), IsBootstrapEraAddr (..), deriveLvl3KeyPair)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, ShouldCheckPassphrase (..), isHardened)
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
    (addr, addressKey) <- deriveAddressSKPure secrets scp passphrase
                            (addrMetaToAccount addrMeta) cwamChainIndex cwamAddressIndex
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

data GenerationMode a b
    = DeterministicMode a b
    | RandomMode

-- The seeds used for generating the account and chain indices.
type AddressGenerationMode = GenerationMode Word32 Word32

generateUnique
    :: (MonadIO m, MonadThrow m)
    => Text
    -> AddressGenerationMode      -- ^ The type of address generation to use.
    -> (Word32 -> Word32 -> m b)
    -> (b -> m Bool)              -- ^ Whether this account + chain index is a duplicate of existing.
    -> m b
generateUnique desc RandomMode generator valueExists = loop (100 :: Int)
  where
    loop 0 = throwM . RequestError $
             sformat (build%": generation of unique item seems too difficult, \
                      \you are approaching the limit") desc
    loop i = do
        l1Rand <- liftIO randomIO
        l2Rand <- liftIO randomIO
        value <- generator l1Rand l2Rand
        bad <- orM
            [ valueExists value
            , pure $ isHardened l1Rand      -- using hardened keys only for now
            , pure $ isHardened l2Rand
            ]
        if bad then
            loop (i - 1)
        else
            return value
generateUnique desc (DeterministicMode s1 s2) generator notFit = do
    value <- generator (fromIntegral s1) (fromIntegral s2)
    whenM (notFit value) $
        throwM . InternalError $
        sformat (build%": this index is already taken")
        desc
    return value

genUniqueAccountId
    :: AccountMode ctx m
    => AddressGenerationMode
    -> CId Wal
    -> m AccountId
genUniqueAccountId genMode wsCAddr =
    generateUnique "accountId generation" genMode generator isValid
  where
    generator :: AccountMode ctx m => Word32 -> Word32 -> m AccountId
    generator i j = return $ AccountId wsCAddr i j

    isValid addr = isJust <$> getAccountMeta addr

genUniqueAddress
    :: AccountMode ctx m
    => AddressGenerationMode
    -> PassPhrase
    -> AccountId
    -> m CWAddressMeta
genUniqueAddress genMode passphrase wCAddr@AccountId{..} =
    generateUnique "address generation" genMode mkAddress isValid
  where
    mkAddress :: AccountMode ctx m => Word32 -> Word32 -> m CWAddressMeta
    mkAddress cwamChainIndex cwamAddressIndex =
        deriveAddress passphrase wCAddr cwamChainIndex cwamAddressIndex

    isValid addr = doesWAddressExist Ever addr

deriveAddressSK
    :: AccountMode ctx m
    => ShouldCheckPassphrase
    -> PassPhrase
    -> AccountId
    -> Word32
    -> Word32
    -> m (Address, EncryptedSecretKey)
deriveAddressSK scp passphrase accId chainIndex addressIndex = do
    secrets <- getSecretKeys
    runExceptT (deriveAddressSKPure secrets scp passphrase accId chainIndex addressIndex) >>= eitherToThrow

deriveAddressSKPure
    :: MonadError WalletError m
    => AllUserSecrets
    -> ShouldCheckPassphrase
    -> PassPhrase
    -> AccountId
    -> Word32
    -> Word32
    -> m (Address, EncryptedSecretKey)
deriveAddressSKPure secrets scp passphrase AccountId {..} chainIndex addressIndex = do
    key <- getSKByIdPure secrets aiWId
    maybe (throwError badPass) pure $
        deriveLvl3KeyPair
            (IsBootstrapEraAddr True) -- TODO: make it context-dependent!
            scp
            passphrase
            key
            aiIndex
            chainIndex
            addressIndex
  where
    badPass = RequestError "Passphrase doesn't match"

deriveAddress
    :: AccountMode ctx m
    => PassPhrase
    -> AccountId
    -> Word32
    -> Word32
    -> m CWAddressMeta
deriveAddress passphrase accId@AccountId{..} cwamChainIndex cwamAddressIndex = do
    (addr, _) <- deriveAddressSK (ShouldCheckPassphrase True) passphrase accId
                   cwamChainIndex cwamAddressIndex
    let cwamWId          = aiWId
        cwamAccountIndex = aiIndex
        cwamId           = addressToCId addr
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
