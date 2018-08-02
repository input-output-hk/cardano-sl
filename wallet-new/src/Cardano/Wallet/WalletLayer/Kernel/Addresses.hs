module Cardano.Wallet.WalletLayer.Kernel.Addresses (
    createAddress
) where

import           Universum

import           Data.Coerce (coerce)
import           Data.Time.Units (Second)


import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Addresses as Kernel

import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.Types (AccountId (..))
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (limitExecutionTimeTo)
import           Cardano.Wallet.WalletLayer.Types (CreateAddressError (..))

import           Pos.Core (Address, decodeTextAddress)

import qualified Cardano.Wallet.API.V1.Types as V1

import           Cardano.Wallet.API.V1.Types (V1 (..))

createAddress :: MonadIO m
              => Kernel.PassiveWallet
              -> V1.NewAddress
              -> m (Either CreateAddressError Address)
createAddress wallet (V1.NewAddress mbSpendingPassword accIdx (V1.WalletId wId)) = do
    liftIO $ limitExecutionTimeTo (30 :: Second) CreateAddressTimeLimitReached $ do
        case decodeTextAddress wId of
             Left _ ->
                 return $ Left (CreateAddressAddressDecodingFailed wId)
             Right rootAddr -> do
                let hdRootId = HD.HdRootId . InDb $ rootAddr
                let hdAccountId = HD.HdAccountId hdRootId (HD.HdAccountIx accIdx)
                let passPhrase = maybe mempty coerce mbSpendingPassword
                res <- liftIO $ Kernel.createAddress passPhrase
                                                     (AccountIdHdRnd hdAccountId)
                                                     wallet
                case res of
                     Right newAddr -> return (Right newAddr)
                     Left  err     -> return (Left $ CreateAddressError err)
