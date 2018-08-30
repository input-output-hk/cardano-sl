{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Kernel.Addresses (
      createAddress
    , newHdAddress
    -- * Errors
    , CreateAddressError(..)
    ) where

import qualified Prelude
import           Universum

import           Control.Lens (to)
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting as F
import qualified Formatting.Buildable
import           System.Random.MWC (GenIO, createSystemRandom, uniformR)

import           Data.Acid (update)
import qualified Data.Aeson as Aeson

import           Pos.Core (Address, IsBootstrapEraAddr (..), deriveLvl2KeyPair)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase,
                     ShouldCheckPassphrase (..))

import           Cardano.Wallet.Kernel.DB.AcidState (CreateHdAddress (..))
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId,
                     HdAccountIx (..), HdAddress, HdAddressId (..),
                     HdAddressIx (..), hdAccountIdIx, hdAccountIdParent,
                     hdAddressIdIx)
import           Cardano.Wallet.Kernel.DB.HdWallet.Create
                     (CreateHdAddressError (..), initHdAddress)
import           Cardano.Wallet.Kernel.DB.HdWallet.Derivation
                     (HardeningMode (..), deriveIndex)
import           Cardano.Wallet.Kernel.Internal (PassiveWallet, walletKeystore,
                     wallets)
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
    deriving (Generic, Eq)

instance Aeson.ToJSON CreateAddressError
instance Aeson.FromJSON CreateAddressError

instance Arbitrary CreateAddressError where
    arbitrary = oneof [ CreateAddressUnknownHdAccount <$> arbitrary
                      , CreateAddressKeystoreNotFound <$> arbitrary
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
    let keystore = pw ^. walletKeystore
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
             mbEsk <- Keystore.lookup (WalletIdHdRnd (hdAccId ^. hdAccountIdParent))
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
                mbAddr = newHdAddress esk spendingPassword accId hdAddressId
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
newHdAddress :: EncryptedSecretKey
             -> PassPhrase
             -> HdAccountId
             -> HdAddressId
             -> Maybe HdAddress
newHdAddress esk spendingPassword accId hdAddressId =
    let mbAddr = deriveLvl2KeyPair (IsBootstrapEraAddr True)
                                   (ShouldCheckPassphrase True)
                                   spendingPassword
                                   esk
                                   (accId ^. hdAccountIdIx . to getHdAccountIx)
                                   (hdAddressId ^. hdAddressIdIx . to getHdAddressIx)
    in case mbAddr of
         Nothing              -> Nothing
         Just (newAddress, _) -> Just $ initHdAddress hdAddressId newAddress
