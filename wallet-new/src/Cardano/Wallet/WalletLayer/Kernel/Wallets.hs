module Cardano.Wallet.WalletLayer.Kernel.Wallets (
    createWallet
) where

import           Universum

import           Control.Lens (to)
import           Data.Coerce (coerce)
import           Data.Time.Units (Second)
import           Formatting (build, sformat)

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Wallets as Kernel

import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (limitExecutionTimeTo)
import           Cardano.Wallet.WalletLayer.Types (CreateWalletError (..))

import           Pos.Core (mkCoin)

import qualified Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Kernel.Util (getCurrentTimestamp)
import           Pos.Crypto.Signing

import           Cardano.Wallet.API.V1.Types (V1 (..))

createWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.NewWallet
             -> m (Either CreateWalletError V1.Wallet)
createWallet wallet (V1.NewWallet (V1.BackupPhrase mnemonic) mbSpendingPassword v1AssuranceLevel v1WalletName operation) = do
    liftIO $ limitExecutionTimeTo (30 :: Second) CreateWalletTimeLimitReached $ do
        case operation of
             V1.RestoreWallet -> error "Not implemented, see [CBR-243]."
             V1.CreateWallet  -> do
                 let spendingPassword = maybe emptyPassphrase coerce mbSpendingPassword
                 let hdAssuranceLevel = case v1AssuranceLevel of
                       V1.NormalAssurance -> HD.AssuranceLevelNormal
                       V1.StrictAssurance -> HD.AssuranceLevelStrict

                 res <- liftIO $ Kernel.createHdWallet wallet
                                                       mnemonic
                                                       spendingPassword
                                                       hdAssuranceLevel
                                                       (HD.WalletName v1WalletName)
                 case res of
                      Left kernelError ->
                          return (Left $ CreateWalletError kernelError)
                      Right hdRoot -> do
                          let (hasSpendingPassword, mbLastUpdate) =
                                  case hdRoot ^. HD.hdRootHasPassword of
                                       HD.NoSpendingPassword -> (False, Nothing)
                                       HD.HasSpendingPassword lastUpdate -> (True, Just (lastUpdate ^. fromDb))
                          now <- liftIO getCurrentTimestamp
                          let lastUpdate = fromMaybe now mbLastUpdate
                          let createdAt  = hdRoot ^. HD.hdRootCreatedAt . fromDb
                          let walletId = hdRoot ^. HD.hdRootId . to (sformat build . _fromDb . HD.getHdRootId)
                          return $ Right V1.Wallet {
                              walId                         = (V1.WalletId walletId)
                            , walName                       = v1WalletName
                            , walBalance                    = V1 (mkCoin 0)
                            , walHasSpendingPassword        = hasSpendingPassword
                            , walSpendingPasswordLastUpdate = V1 lastUpdate
                            , walCreatedAt                  = V1 createdAt
                            , walAssuranceLevel             = v1AssuranceLevel
                            , walSyncState                  = V1.Synced
                          }
