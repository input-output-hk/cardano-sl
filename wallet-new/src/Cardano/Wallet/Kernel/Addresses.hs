module Cardano.Wallet.Kernel.Addresses (
    createAddress
    -- * Errors
    , CreateAddressError(..)
    ) where

import           Universum

import           Control.Lens (to)
import           Formatting (bprint, (%))
import qualified Formatting as F
import qualified Formatting.Buildable
import           System.Random.MWC (GenIO, createSystemRandom, uniformR)

import           Data.Acid (update)

import           Pos.Core (Address, IsBootstrapEraAddr (..), deriveLvl2KeyPair)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase,
                     ShouldCheckPassphrase (..))

import           Cardano.Wallet.Kernel (PassiveWallet, walletKeystore, wallets)
import           Cardano.Wallet.Kernel.DB.AcidState (CreateHdAddress (..))
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId,
                     HdAccountIx (..), HdAddressId (..), HdAddressIx (..),
                     UnknownHdAccount (..), hdAccountIdIx, hdAccountIdParent,
                     hdAddressIdIx)
import           Cardano.Wallet.Kernel.DB.HdWallet.Create
                     (CreateHdAddressError (..), initHdAddress)
import           Cardano.Wallet.Kernel.DB.HdWallet.Derivation
                     (HardeningMode (..), deriveIndex)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Types (AccountId (..), WalletId (..))

import           Test.QuickCheck (Arbitrary (..), oneof)

data CreateAddressError =
      CreateAddressUnknownHdAccount UnknownHdAccount
      -- ^ When trying to create the 'Address', the parent 'Account' was not
      -- there.
    | CreateAddressKeystoreNotFound AccountId
      -- ^ When trying to create the 'Address', the 'Keystore' didn't have
      -- any secret associated with this 'Account'.
      -- there.
    | CreateAddressHdCreationFailed CreateHdAddressError
      -- ^ The creation of the new HD 'Address' failed with a database error.
    | CreateAddressHdRndGenerationFailed HdAccountId
      -- ^ The crypto-related part of address generation failed.
    | CreateAddressHdRndAddressSpaceSaturated HdAccountId
      -- ^ The available number of HD addresses in use in such that trying
      -- to find another random index would be too expensive
    deriving Eq

-- TODO(adn)
instance Arbitrary CreateAddressError where
    arbitrary = oneof []

instance Buildable CreateAddressError where
    build (CreateAddressUnknownHdAccount uAccount) =
        bprint ("CreateAddressUnknownHdAccount " % F.build) uAccount
    build (CreateAddressKeystoreNotFound accId) =
        bprint ("CreateAddressKeystoreNotFound " % F.build) accId
    build (CreateAddressHdCreationFailed hdErr) =
        bprint ("CreateAddressHdCreationFailed " % F.build) hdErr
    build (CreateAddressHdRndGenerationFailed hdAcc) =
        bprint ("CreateAddressHdRndGenerationFailed " % F.build) hdAcc
    build (CreateAddressHdRndAddressSpaceSaturated hdAcc) =
        bprint ("CreateAddressHdRndAddressSpaceSaturated " % F.build) hdAcc

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
         -- The reason why we do this is because in order to create an
         -- 'HdAddress' we need a proper 'Address', but this cannot be derived
         -- with having access to the 'EncryptedSecretKey' and the 'PassPhrase',
         -- and we do not want these exposed in the acid-state transaction log.
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
--    2147483647 attempts (which is half of the address space).
-- 4. If after 2147483647 attempts there is still no result, flag this upstream.
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
                mbAddr = deriveLvl2KeyPair (IsBootstrapEraAddr True)
                                           (ShouldCheckPassphrase True)
                                           spendingPassword
                                           esk
                                           (accId ^. hdAccountIdIx . to getHdAccountIx)
                                           (hdAddressId ^. hdAddressIdIx . to getHdAddressIx)
            case mbAddr of
                 Nothing -> return (Left $ CreateAddressHdRndGenerationFailed accId)
                 Just (newAddress, _) -> do
                    let hdAddress  = initHdAddress hdAddressId (InDb newAddress)
                    let db = pw ^. wallets
                    res <- update db (CreateHdAddress hdAddress)
                    case res of
                         (Left (CreateHdAddressExists _)) ->
                             go gen (succ collisions)
                         (Left err) ->
                             return (Left $ CreateAddressHdCreationFailed err)
                         Right () -> return (Right newAddress)

        -- The maximum number of allowed collisions. This number was empirically
        -- chosen as half of the total available address space. A possible line
        -- of reasoning is that if we hit that many collision it meas the
        -- probability to find a new, unused address is ~ <= 50%, and therefore
        -- it's better to switch to a new account.
        maxAllowedCollisions :: Word32
        maxAllowedCollisions = 2147483647

