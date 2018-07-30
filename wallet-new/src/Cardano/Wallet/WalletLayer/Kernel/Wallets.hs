module Cardano.Wallet.WalletLayer.Kernel.Wallets (
      createWallet
    , updateWalletPassword
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
import           Cardano.Wallet.WalletLayer.Types (CreateWalletError (..),
                     UpdateWalletPasswordError (..))

import           Pos.Core (Coin, Timestamp, decodeTextAddress, mkCoin,
                     unsafeAddCoin)

import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
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
                                       HD.HasSpendingPassword lu -> (True, Just (lu ^. fromDb))
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

-- | Converts an 'HdRoot' into a V1 'Wallet.
toV1Wallet :: Timestamp -> Kernel.DB -> HD.HdRoot -> V1.Wallet
toV1Wallet now db hdRoot =
    let (hasSpendingPassword, mbLastUpdate) =
            case hdRoot ^. HD.hdRootHasPassword of
                 HD.NoSpendingPassword     -> (False, Nothing)
                 HD.HasSpendingPassword lu -> (True, Just (lu ^. fromDb))
        rootId = hdRoot ^. HD.hdRootId
        lastUpdate = fromMaybe now mbLastUpdate
        createdAt  = hdRoot ^. HD.hdRootCreatedAt . fromDb
        walletId   = sformat build . _fromDb . HD.getHdRootId $ rootId
        v1AssuranceLevel = case hdRoot ^. HD.hdRootAssurance of
                               HD.AssuranceLevelNormal -> V1.NormalAssurance
                               HD.AssuranceLevelStrict -> V1.StrictAssurance
    in V1.Wallet {
        walId                         = (V1.WalletId walletId)
      , walName                       = hdRoot ^. HD.hdRootName
                                                . to HD.getWalletName
      , walBalance                    = V1 (walletTotalBalance db rootId)
      , walHasSpendingPassword        = hasSpendingPassword
      , walSpendingPasswordLastUpdate = V1 lastUpdate
      , walCreatedAt                  = V1 createdAt
      , walAssuranceLevel             = v1AssuranceLevel
      -- FIXME(adn) Do this as part of CBR-243.
      , walSyncState                  = V1.Synced
    }

-- | Computes the total balance for this wallet, given its 'HdRootId'.
walletTotalBalance :: Kernel.DB -> HD.HdRootId -> Coin
walletTotalBalance db hdRootId =
    IxSet.foldl' (\total account ->
                      total `unsafeAddCoin`
                      Kernel.accountTotalBalance db (account ^. HD.hdAccountId)
                 )
                 (mkCoin 0)
                 (Kernel.walletAccounts db hdRootId)

-- | Updates the 'SpendingPassword' for this wallet.
updateWalletPassword :: MonadIO m
                     => Kernel.PassiveWallet
                     -> V1.WalletId
                     -> V1.PasswordUpdate
                     -> m (Either UpdateWalletPasswordError V1.Wallet)
updateWalletPassword wallet (V1.WalletId wId) (V1.PasswordUpdate (V1 oldPwd) (V1 newPwd)) =
    case decodeTextAddress wId of
        Left _ -> return $ Left (UpdateWalletPasswordWalletIdDecodingFailed wId)
        Right rootAddr -> do
           let hdRootId = HD.HdRootId . InDb $ rootAddr
           res <- liftIO $ Kernel.updatePassword wallet hdRootId oldPwd newPwd
           case res of
                Left e  -> return $ Left (UpdateWalletPasswordError e)
                Right (db, updatedWallet) -> do
                    now <- liftIO getCurrentTimestamp
                    return $ Right $ toV1Wallet now db updatedWallet
