{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Kernel.Addresses (
      createAddress
    , newHdAddress
    , importAddresses
    -- * Errors
    , CreateAddressError(..)
    , ImportAddressError(..)
    ) where

import qualified Prelude
import           Universum

import           Control.Lens (to)
import           Control.Monad.Except (throwError)
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting as F
import qualified Formatting.Buildable
import           System.Random.MWC (GenIO, createSystemRandom, uniformR)

import           Data.Acid (update)

import           Pos.Core (Address, IsBootstrapEraAddr (..), deriveLvl2KeyPair)
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase,
                     ShouldCheckPassphrase (..))

import           Cardano.Wallet.Kernel.DB.AcidState (CreateHdAddress (..))
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId,
                     HdAccountIx (..), HdAddress, HdAddressId (..),
                     HdAddressIx (..), HdRootId (..), IsOurs (..),
                     hdAccountIdIx, hdAccountIdParent, hdAddressIdIx)
import           Cardano.Wallet.Kernel.DB.HdWallet.Create
                     (CreateHdAddressError (..), initHdAddress)
import           Cardano.Wallet.Kernel.DB.HdWallet.Derivation
                     (HardeningMode (..), deriveIndex)
import           Cardano.Wallet.Kernel.Internal (PassiveWallet, walletKeystore,
                     walletProtocolMagic, wallets)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Types (AccountId (..), WalletId (..))
import           Cardano.Wallet.WalletLayer.Kernel.Conv (toCardanoAddress)

import           Test.QuickCheck (Arbitrary (..), oneof)

data CreateAddressError =
      CreateAddressUnknownHdAccount HdAccountId
      -- ^ When trying to create the 'Address', the parent 'Account' was not
      -- there.
    | CreateAddressKeystoreNotFound AccountId
      -- ^ When trying to create the 'Address', the 'Keystore' didn't have
      -- any secret associated with this 'Account'.
      -- there.
    | CreateAddressHdRndGenerationFailed HdAccountId
      -- ^ The crypto-related part of address generation failed. This is
      -- likely to happen if the 'PassPhrase' does not match the one used
      -- to encrypt the 'EncryptedSecretKey'.
    | CreateAddressHdRndAddressSpaceSaturated HdAccountId
      -- ^ The available number of HD addresses in use in such that trying
      -- to find another random index would be too expensive
    deriving Eq

instance Arbitrary CreateAddressError where
    arbitrary = oneof
        [ CreateAddressUnknownHdAccount <$> arbitrary
        , CreateAddressKeystoreNotFound . AccountIdHdRnd <$> arbitrary
        , CreateAddressHdRndGenerationFailed <$> arbitrary
        , CreateAddressHdRndAddressSpaceSaturated <$> arbitrary
        ]

instance Buildable CreateAddressError where
    build (CreateAddressUnknownHdAccount uAccount) =
        bprint ("CreateAddressUnknownHdAccount " % F.build) uAccount
    build (CreateAddressKeystoreNotFound accId) =
        bprint ("CreateAddressKeystoreNotFound " % F.build) accId
    build (CreateAddressHdRndGenerationFailed hdAcc) =
        bprint ("CreateAddressHdRndGenerationFailed " % F.build) hdAcc
    build (CreateAddressHdRndAddressSpaceSaturated hdAcc) =
        bprint ("CreateAddressHdRndAddressSpaceSaturated " % F.build) hdAcc

instance Show CreateAddressError where
    show = formatToString build

-- | Creates a new 'Address' for the input account.
createAddress :: PassPhrase
              -- ^ The 'Passphrase' (a.k.a the \"Spending Password\").
              -> AccountId
              -- ^ An abstract notion of an 'Account' identifier
              -> PassiveWallet
              -> IO (Either CreateAddressError Address)
createAddress spendingPassword accId pw = do
    let nm       = makeNetworkMagic (pw ^. walletProtocolMagic)
        keystore = pw ^. walletKeystore
    case accId of
         -- \"Standard\" HD random derivation. The strategy is as follows:
         --
         -- 1. Generate the Address' @index@ and @HdAddress@ structure outside
         --    of an atomic acid-state transaction. This could lead to data
         --    races in the sense that an index is picked and such index
         --    is already claimed, but if this happens we simply try again.
         -- 2. Perform the actual creation of the 'HdAddress' as an atomic
         --    transaction in acid-state.
         --
         -- The reason why we do this is because:
         -- 1. We cannot do IO (thus index derivation) in an acid-state
         --    transaction
         -- 2. In order to create an 'HdAddress' we need a proper 'Address',
         -- but this cannot be derived with having access to the
         -- 'EncryptedSecretKey' and the 'PassPhrase', and we do not want
         -- these exposed in the acid-state transaction log.
         (AccountIdHdRnd hdAccId) -> do
             mbEsk <- Keystore.lookup nm
                                      (WalletIdHdRnd (hdAccId ^. hdAccountIdParent))
                                      keystore
             case mbEsk of
                  Nothing  -> return (Left $ CreateAddressKeystoreNotFound accId)
                  Just esk -> createHdRndAddress spendingPassword esk hdAccId pw

-- | Creates a new 'Address' using the random HD derivation under the hood.
-- Being this an operation bound not only by the number of available derivation
-- indexes \"left\" in the account, some form of short-circuiting is necessary.
-- Currently, the algorithm is as follows:
--
-- 1. Try to generate an 'Address' by picking a random index;
-- 2. If the operation succeeds, return the 'Address';
-- 3. If the DB operation fails due to a collision, try again, up to a max of
--    1024 attempts.
-- 4. If after 1024 attempts there is still no result, flag this upstream.
createHdRndAddress :: PassPhrase
                   -> EncryptedSecretKey
                   -> HdAccountId
                   -> PassiveWallet
                   -> IO (Either CreateAddressError Address)
createHdRndAddress spendingPassword esk accId pw = do
    gen <- createSystemRandom
    go gen 0
    where
        go :: GenIO -> Word32 -> IO (Either CreateAddressError Address)
        go gen collisions =
            case collisions >= maxAllowedCollisions of
                 True  -> return $ Left (CreateAddressHdRndAddressSpaceSaturated accId)
                 False -> tryGenerateAddress gen collisions

        tryGenerateAddress :: GenIO
                           -> Word32
                           -- ^ The current number of collisions
                           -> IO (Either CreateAddressError Address)
        tryGenerateAddress gen collisions = do
            newIndex <- deriveIndex (flip uniformR gen) HdAddressIx HardDerivation
            let hdAddressId = HdAddressId accId newIndex
                nm = makeNetworkMagic $ pw ^. walletProtocolMagic
                mbAddr = newHdAddress nm esk spendingPassword accId hdAddressId
            case mbAddr of
                 Nothing -> return (Left $ CreateAddressHdRndGenerationFailed accId)
                 Just hdAddress -> do
                    let db = pw ^. wallets
                    res <- update db (CreateHdAddress hdAddress)
                    case res of
                         (Left (CreateHdAddressExists _)) ->
                             go gen (succ collisions)
                         (Left (CreateHdAddressUnknown _)) ->
                             return (Left $ CreateAddressUnknownHdAccount accId)
                         Right () -> return (Right $ toCardanoAddress hdAddress)

        -- The maximum number of allowed collisions.
        maxAllowedCollisions :: Word32
        maxAllowedCollisions = 1024


-- | Generates a new 'HdAddress' by performing the HD crypto derivation
-- underneath. Returns 'Nothing' if the cryptographic derivation fails.
newHdAddress :: NetworkMagic
             -> EncryptedSecretKey
             -> PassPhrase
             -> HdAccountId
             -> HdAddressId
             -> Maybe HdAddress
newHdAddress nm esk spendingPassword accId hdAddressId =
    let mbAddr = deriveLvl2KeyPair nm
                                   (IsBootstrapEraAddr True)
                                   (ShouldCheckPassphrase True)
                                   spendingPassword
                                   esk
                                   (accId ^. hdAccountIdIx . to getHdAccountIx)
                                   (hdAddressId ^. hdAddressIdIx . to getHdAddressIx)
    in case mbAddr of
         Nothing              -> Nothing
         Just (newAddress, _) -> Just $ initHdAddress hdAddressId newAddress


data ImportAddressError
    = ImportAddressKeystoreNotFound HdRootId
    -- ^ When trying to import the 'Address', the parent root was not there.
    deriving Eq

instance Arbitrary ImportAddressError where
    arbitrary = oneof
        [ ImportAddressKeystoreNotFound <$> arbitrary
        ]

instance Buildable ImportAddressError where
    build = \case
        ImportAddressKeystoreNotFound rootId ->
            bprint ("ImportAddressError" % F.build) rootId

instance Show ImportAddressError where
    show = formatToString build


-- | Import already existing addresses into the DB. A typical use-case for that
-- is backend migration, where users (e.g. exchanges) want to import unused
-- addresses they've generated in the past (and likely communicated to their
-- users). Because Addresses in the old scheme are generated randomly, there's
-- no guarantee that addresses would be generated in the same order on a new
-- node (they better not actually!).
importAddresses
    :: HdRootId
    -> [Address]
    -> PassiveWallet
    -> IO (Either ImportAddressError [Either Address ()])
importAddresses rootId addrs pw = runExceptT $ do
    esk <- lookupSecretKey rootId
    lift $ forM addrs (flip importOneAddress [(rootId, esk)])
  where
    lookupSecretKey
        :: HdRootId
        -> ExceptT ImportAddressError IO EncryptedSecretKey
    lookupSecretKey k = do
        let nm = makeNetworkMagic (pw ^. walletProtocolMagic)
        let keystore = pw ^. walletKeystore
        lift (Keystore.lookup nm (WalletIdHdRnd k) keystore) >>= \case
            Nothing  -> throwError (ImportAddressKeystoreNotFound rootId)
            Just esk -> return esk

    importOneAddress
        :: Address
        -> [(HdRootId, EncryptedSecretKey)]
        -> IO (Either Address ())
    importOneAddress addr = evalStateT $ do
        let updateLifted = fmap Just .  lift . update (pw ^. wallets)
        res <- state (isOurs addr) >>= \case
            Nothing     -> return Nothing
            Just hdAddr -> updateLifted $ CreateHdAddress hdAddr
        return $ case res of
            Just (Right _) -> Right ()
            _              -> Left addr
