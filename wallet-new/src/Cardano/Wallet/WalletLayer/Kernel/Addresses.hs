module Cardano.Wallet.WalletLayer.Kernel.Addresses (
    createAddress
) where

import           Universum

import           Data.Coerce (coerce)

import           Pos.Core (Address)

import           Cardano.Wallet.API.V1.Types (V1 (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.Types (AccountId (..))
import           Cardano.Wallet.WalletLayer.Kernel.Conv
import           Cardano.Wallet.WalletLayer.Types (CreateAddressError (..))

createAddress :: MonadIO m
              => Kernel.PassiveWallet
              -> V1.NewAddress
              -> m (Either CreateAddressError Address)
createAddress wallet
              (V1.NewAddress
                mbSpendingPassword
                accIx
                wId) = runExceptT $ do
    accId <- withExceptT CreateAddressAddressDecodingFailed $
               fromAccountId wId accIx
    withExceptT CreateAddressError $ ExceptT $ liftIO $
      Kernel.createAddress passPhrase (AccountIdHdRnd accId) wallet
  where
    passPhrase = maybe mempty coerce mbSpendingPassword
