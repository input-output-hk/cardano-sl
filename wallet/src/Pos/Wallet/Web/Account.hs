-- | Helpers for Wallet Set, Wallet and Account.
{-# LANGUAGE DataKinds #-}

module Pos.Wallet.Web.Account
       ( myRootAddresses
       , getSKById
       , getSKByAddress
       , getSKByAddressPure
       , getKeyById
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

import           Universum

import           Control.Monad.Except (MonadError (throwError), runExceptT)
import           Formatting (build, sformat, (%))
import           System.Random (randomRIO)

import           Pos.Client.KeyStorage (AllUserSecrets (..), MonadKeys,
                     MonadKeysRead, addSecretKey, getSecretKeys,
                     getSecretKeysPlain)
import           Pos.Core (Address (..), IsBootstrapEraAddr (..),
                     deriveLvl2KeyPair)
import           Pos.Core.NetworkMagic (NetworkMagic (..))
import           Pos.Crypto (EncryptedSecretKey, PassPhrase,
                     ShouldCheckPassphrase (..), firstHardened,
                     safeDeterministicKeyGen)
import           Pos.Util (eitherToThrow)
import           Pos.Util.Mnemonic (Mnemonic, mnemonicToSeed)
import           Pos.Util.Wlog (WithLogger)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CId, Wal, encToCId)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (AddressLookupMode (Ever),
                     HasWAddressMeta (..), WAddressMeta (..), WalletSnapshot,
                     doesWAddressExist, getAccountMeta, wamAccount)

type AccountMode ctx m =
    ( MonadThrow m
    , WithLogger m
    , MonadKeysRead m
    , MonadIO m
    )

myRootAddresses :: MonadKeysRead m => NetworkMagic -> m [CId Wal]
myRootAddresses nm = encToCId nm <<$>> getSecretKeysPlain

getSKById
    :: AccountMode ctx m
    => NetworkMagic
    -> CId Wal
    -> m (Maybe EncryptedSecretKey)
getSKById nm walletId = do
    secretKeys <- getSecretKeys
    return $ getSKByIdPure nm secretKeys walletId

getSKByIdPure
    :: NetworkMagic
    -> AllUserSecrets
    -> CId Wal
    -> Maybe EncryptedSecretKey
getSKByIdPure nm (AllUserSecrets secretKeys) wid =
    find (\k -> encToCId nm k == wid) secretKeys

getKeyById
    :: AccountMode ctx m
    => NetworkMagic
    -> CId Wal
    -> m EncryptedSecretKey
getKeyById nm walletId = do
    secretKeys <- getSecretKeys
    case getSKByIdPure nm secretKeys walletId of
        Just sk -> return sk
        Nothing -> throwM . InternalError $
            sformat ("'Impossible' happened: there's no secret key for wallet "%build) walletId

getSKByAddress
    :: AccountMode ctx m
    => NetworkMagic
    -> ShouldCheckPassphrase
    -> PassPhrase
    -> WAddressMeta
    -> m EncryptedSecretKey
getSKByAddress nm scp passphrase addrMeta = do
    secrets <- getSecretKeys
    runExceptT (getSKByAddressPure nm secrets scp passphrase addrMeta) >>= eitherToThrow

getSKByAddressPure
    :: MonadError WalletError m
    => NetworkMagic
    -> AllUserSecrets
    -> ShouldCheckPassphrase
    -> PassPhrase
    -> WAddressMeta
    -> m EncryptedSecretKey
getSKByAddressPure nm secrets scp passphrase addrMeta = do
    (addr, addressKey) <-
            deriveAddressSKPure nm secrets scp passphrase (addrMeta ^. wamAccount) (addrMeta ^. wamAddressIndex)
    if addr /= addrMeta ^. wamAddress
             -- if you see this error, maybe you generated public key address with
             -- no hd wallet attribute (if so, address would be ~half shorter than
             -- others)
        then throwError . InternalError $ "Account is contradictory!"
        else pure addressKey

genSaveRootKey
    :: (AccountMode ctx m, MonadKeys m)
    => PassPhrase
    -> Mnemonic 12
    -> m EncryptedSecretKey
genSaveRootKey passphrase mnemonic =
    let
        (_, key) =
            safeDeterministicKeyGen (mnemonicToSeed mnemonic) passphrase
    in
        addSecretKey key >> return key

data GenSeed a
    = DeterminedSeed a
    | RandomSeed

type AddrGenSeed = GenSeed Word32   -- with derivation index

generateUnique
    :: (MonadIO m, MonadThrow m)
    => Text -> AddrGenSeed -> (Word32 -> m b) -> (Word32 -> b -> Bool) -> m b
generateUnique desc RandomSeed generator isDuplicate = loop (100 :: Int)
  where
    loop 0 = throwM . RequestError $
             sformat (build%": generation of unique item seems too difficult, \
                      \you are approaching the limit") desc
    loop i = do
        rand  <- liftIO $ randomRIO (firstHardened, maxBound)
        value <- generator rand
        if isDuplicate rand value then
            loop (i - 1)
        else
            return value
generateUnique desc (DeterminedSeed seed) generator notFit = do
    value <- generator (fromIntegral seed)
    when (notFit seed value) $
        throwM . InternalError $
        sformat (build%": this index is already taken")
        desc
    return value

genUniqueAccountId
    :: (MonadIO m, MonadThrow m)
    => WalletSnapshot
    -> AddrGenSeed
    -> CId Wal
    -> m AccountId
genUniqueAccountId ws genSeed wsCAddr =
    generateUnique
        "account generation"
        genSeed
        (return . AccountId wsCAddr)
        notFit
  where
    notFit _idx addr = isJust $ getAccountMeta ws addr

genUniqueAddress
    :: AccountMode ctx m
    => NetworkMagic
    -> WalletSnapshot
    -> AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m WAddressMeta
genUniqueAddress nm ws genSeed passphrase wCAddr@AccountId{..} =
    generateUnique "address generation" genSeed mkAddress notFit
  where
    mkAddress :: AccountMode ctx m => Word32 -> m WAddressMeta
    mkAddress cwamAddressIndex =
        deriveAddress nm passphrase wCAddr cwamAddressIndex
    notFit _idx addr = doesWAddressExist ws Ever addr

deriveAddressSK
    :: AccountMode ctx m
    => NetworkMagic
    -> ShouldCheckPassphrase
    -> PassPhrase
    -> AccountId
    -> Word32
    -> m (Address, EncryptedSecretKey)
deriveAddressSK nm scp passphrase accId addressIndex = do
    secrets <- getSecretKeys
    runExceptT (deriveAddressSKPure nm secrets scp passphrase accId addressIndex) >>= eitherToThrow

deriveAddressSKPure
    :: MonadError WalletError m
    => NetworkMagic
    -> AllUserSecrets
    -> ShouldCheckPassphrase
    -> PassPhrase
    -> AccountId
    -> Word32
    -> m (Address, EncryptedSecretKey)
deriveAddressSKPure nm secrets scp passphrase AccountId {..} addressIndex = do
    key <- case getSKByIdPure nm secrets aiWId of
        Just key -> pure key
        Nothing  -> throwError noSuchSecretKey

    maybe (throwError badPass) pure $
        deriveLvl2KeyPair
            nm
            (IsBootstrapEraAddr True) -- TODO: make it context-dependent!
            scp
            passphrase
            key
            aiIndex
            addressIndex
  where
    badPass = RequestError "Passphrase doesn't match"
    noSuchSecretKey = RequestError "No such secret key found"

deriveAddress
    :: AccountMode ctx m
    => NetworkMagic
    -> PassPhrase
    -> AccountId
    -> Word32
    -> m WAddressMeta
deriveAddress nm passphrase accId@AccountId{..} cwamAddressIndex = do
    (addr, _) <- deriveAddressSK nm (ShouldCheckPassphrase True) passphrase accId cwamAddressIndex
    return $ WAddressMeta aiWId aiIndex cwamAddressIndex addr

-- | Allows to find a key related to given @id@ item.
class MonadKeySearch id m where
    findKey :: NetworkMagic -> id -> m EncryptedSecretKey

instance AccountMode ctx m => MonadKeySearch (CId Wal) m where
    findKey = getKeyById

instance AccountMode ctx m => MonadKeySearch AccountId m where
    findKey nm = (findKey nm) . aiWId

instance AccountMode ctx m => MonadKeySearch WAddressMeta m where
    findKey nm = (findKey nm) . _wamWalletId
