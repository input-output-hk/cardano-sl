module Cardano.Wallet.WalletLayer.Kernel.Wallets (
      createWallet
    , updateWallet
    , updateWalletPassword
    , deleteWallet
    , getWallet
    , getWallets
    , getWalletUtxos
    ) where

import           Universum

import           Data.Coerce (coerce)

import           Pos.Chain.Txp (Utxo)
import           Pos.Core (mkCoin)
import           Pos.Core.Slotting (Timestamp)
import           Pos.Crypto.Signing

import           Cardano.Wallet.API.V1.Types (V1 (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Accounts as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState (dbHdWallets)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.Kernel.Util.Core (getCurrentTimestamp)
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import           Cardano.Wallet.WalletLayer (CreateWalletError (..),
                     DeleteWalletError (..), GetUtxosError (..),
                     GetWalletError (..), UpdateWalletError (..),
                     UpdateWalletPasswordError (..))
import           Cardano.Wallet.WalletLayer.Kernel.Conv

createWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.NewWallet
             -> m (Either CreateWalletError V1.Wallet)
createWallet wallet
             (V1.NewWallet
               (V1.BackupPhrase mnemonic)
               mbSpendingPassword
               v1AssuranceLevel
               v1WalletName
               operation) = liftIO $ do
    case operation of
      V1.RestoreWallet -> error "Not implemented, see [CBR-243]."
      V1.CreateWallet  -> create
  where
    create :: IO (Either CreateWalletError V1.Wallet)
    create = runExceptT $ do
      now  <- liftIO getCurrentTimestamp
      root <- withExceptT CreateWalletError $ ExceptT $
                Kernel.createHdWallet wallet
                                      mnemonic
                                      spendingPassword
                                      hdAssuranceLevel
                                      (HD.WalletName v1WalletName)
      let rootId = root ^. HD.hdRootId
      fmap (mkRoot now root) $
        withExceptT CreateWalletFirstAccountCreationFailed $ ExceptT $
           Kernel.createAccount spendingPassword
                                (HD.AccountName "Default account")
                                (WalletIdHdRnd rootId)
                                wallet

    mkRoot :: Timestamp -> HD.HdRoot -> HD.HdAccount -> V1.Wallet
    mkRoot now hdRoot _acc = V1.Wallet {
          walId                         = walletId
        , walName                       = v1WalletName
        , walBalance                    = V1 (mkCoin 0)
        , walHasSpendingPassword        = hasSpendingPassword
        , walSpendingPasswordLastUpdate = V1 lastUpdate
        , walCreatedAt                  = V1 createdAt
        , walAssuranceLevel             = v1AssuranceLevel
        , walSyncState                  = V1.Synced
        , walType                       = V1.WalletRegular
        }
      where
        (hasSpendingPassword, mbLastUpdate) =
            case hdRoot ^. HD.hdRootHasPassword of
                 HD.NoSpendingPassword     -> (False, Nothing)
                 HD.HasSpendingPassword lu -> (True, Just (lu ^. fromDb))
        lastUpdate = fromMaybe now mbLastUpdate
        createdAt  = hdRoot ^. HD.hdRootCreatedAt . fromDb
        walletId   = toRootId $ hdRoot ^. HD.hdRootId

    spendingPassword = maybe emptyPassphrase coerce mbSpendingPassword
    hdAssuranceLevel = fromAssuranceLevel v1AssuranceLevel

-- | Updates the 'SpendingPassword' for this wallet.
updateWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.WalletId
             -> V1.WalletUpdate
             -> m (Either UpdateWalletError V1.Wallet)
updateWallet wallet wId (V1.WalletUpdate v1Level v1Name) = runExceptT $ do
    rootId <- withExceptT UpdateWalletWalletIdDecodingFailed $ fromRootId wId
    fmap (uncurry toWallet) $
      withExceptT (UpdateWalletError . V1) $ ExceptT $ liftIO $
        Kernel.updateHdWallet wallet rootId newLevel newName
  where
    newLevel = fromAssuranceLevel v1Level
    newName  = HD.WalletName v1Name

-- | Updates the 'SpendingPassword' for this wallet.
updateWalletPassword :: MonadIO m
                     => Kernel.PassiveWallet
                     -> V1.WalletId
                     -> V1.PasswordUpdate
                     -> m (Either UpdateWalletPasswordError V1.Wallet)
updateWalletPassword wallet
                     wId
                     (V1.PasswordUpdate
                       (V1 oldPwd)
                       (V1 newPwd)) = runExceptT $ do
    rootId <- withExceptT UpdateWalletPasswordWalletIdDecodingFailed $
                fromRootId wId
    fmap (uncurry toWallet) $
      withExceptT UpdateWalletPasswordError $ ExceptT $ liftIO $
        Kernel.updatePassword wallet rootId oldPwd newPwd

-- | Updates the 'SpendingPassword' for this wallet.
deleteWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.WalletId
             -> m (Either DeleteWalletError ())
deleteWallet wallet wId = runExceptT $ do
    rootId <- withExceptT DeleteWalletWalletIdDecodingFailed $ fromRootId wId
    withExceptT (DeleteWalletError . V1) $ ExceptT $ liftIO $
      Kernel.deleteHdWallet wallet rootId

-- | Gets a specific wallet.
getWallet :: V1.WalletId
          -> Kernel.DB
          -> Either GetWalletError V1.Wallet
getWallet wId db = runExcept $ do
    rootId <- withExceptT GetWalletWalletIdDecodingFailed $ fromRootId wId
    fmap (toWallet db) $
      withExceptT (GetWalletError . V1) $ exceptT $
        Kernel.lookupHdRootId db rootId

-- | Gets all the wallets known to this edge node.
--
-- TODO: Avoid IxSet creation [CBR-347].
getWallets :: Kernel.DB -> IxSet V1.Wallet
getWallets db = IxSet.fromList . map (toWallet db) . IxSet.toList $ allRoots
  where
    allRoots = db ^. dbHdWallets . HD.hdWalletsRoots

-- | Gets Utxos per account of a wallet.
getWalletUtxos
    :: V1.WalletId
    -> Kernel.DB
    -> Either GetUtxosError [(V1.Account, Utxo)]
getWalletUtxos wId db = runExcept $ do
    rootId <- withExceptT GetUtxosWalletIdDecodingFailed $
        fromRootId wId

    withExceptT GetUtxosGetAccountsError $ exceptT $ do
        _rootExists <- Kernel.lookupHdRootId db rootId
        return ()

    let accounts = Kernel.accountsByRootId db rootId

    forM (IxSet.toList accounts) $ \account ->
        withExceptT GetUtxosCurrentAvailableUtxoError $ exceptT $ do
            utxo <- Kernel.currentAvailableUtxo db (account ^. HD.hdAccountId)
            return (toAccount db account, utxo)
