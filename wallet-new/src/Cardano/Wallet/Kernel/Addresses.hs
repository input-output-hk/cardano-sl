module Cardano.Wallet.Kernel.Addresses (
    createAddress
    -- * Errors
    , CreateAddressError(..)
    ) where

import           Universum

import           Control.Lens (to)
import           Data.Text.Buildable (Buildable (..))
import           Formatting (bprint, (%))
import qualified Formatting as F
import           System.Random (randomRIO)

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
      -- ^ The overall generation process failed.
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


createHdRndAddress :: PassPhrase
                   -> EncryptedSecretKey
                   -> HdAccountId
                   -> PassiveWallet
                   -> IO (Either CreateAddressError Address)
createHdRndAddress spendingPassword esk accId pw = do
    hdAddressId <- HdAddressId <$> pure accId
                               <*> (deriveIndex randomRIO HdAddressIx HardDerivation)
    let mbAddr = deriveLvl2KeyPair (IsBootstrapEraAddr True)
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
                     createHdRndAddress spendingPassword esk accId pw
                 (Left err) ->
                     return (Left $ CreateAddressHdCreationFailed err)
                 Right () -> return (Right newAddress)
