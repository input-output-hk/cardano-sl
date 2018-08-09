{-# LANGUAGE LambdaCase #-}
module Cardano.Wallet.WalletLayer.Kernel.Wallets (
      createWallet
    , updateWallet
    , updateWalletPassword
    , deleteWallet
    , getWallet
    , getWallets
    , getWalletUtxos
    , blundToResolvedBlock
    ) where

import           Universum

import           Data.Coerce (coerce)
import qualified Data.Map as M
import           Data.Maybe (fromJust)

import           Pos.Chain.Block (Blund, mainBlockSlot, undoTx)
import           Pos.Chain.Txp (Utxo)
import           Pos.Core (mkCoin)
import           Pos.Core.Slotting (SlotId, Timestamp)
import           Pos.Crypto.Signing

import           Cardano.Wallet.API.V1.Types (V1 (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Accounts as Kernel
import qualified Cardano.Wallet.Kernel.BIP39 as BIP39
import           Cardano.Wallet.Kernel.DB.AcidState (dbHdWallets)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Create as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Decrypt (WalletDecrCredentialsKey (..),
                     keyToWalletDecrCredentials)
import           Cardano.Wallet.Kernel.Internal (walletKeystore,
                     walletRestorationTask)
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor (..))
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock,
                     prefilterBlock)
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Restore (restoreWallet)
import           Cardano.Wallet.Kernel.Types (RawResolvedBlock (..),
                     WalletId (..), fromRawResolvedBlock)
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
      V1.RestoreWallet -> restore
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
      _ <- withExceptT CreateWalletFirstAccountCreationFailed $ ExceptT $
             Kernel.createAccount spendingPassword
                                  (HD.AccountName "Default account")
                                  (WalletIdHdRnd rootId)
                                  wallet
      return (mkRoot now root)

    restore :: IO (Either CreateWalletError V1.Wallet)
    restore = runExceptT $ do
        now  <- liftIO getCurrentTimestamp

        let esk = snd $ safeDeterministicKeyGen (BIP39.mnemonicToSeed mnemonic)
                                                spendingPassword
            rootId = HD.eskToHdRootId esk
            root = HD.initHdRoot rootId
                                 (HD.WalletName v1WalletName)
                                 (if spendingPassword == emptyPassphrase
                                      then HD.NoSpendingPassword
                                      else HD.HasSpendingPassword (InDb now))
                                 hdAssuranceLevel
                                 (InDb now)
            wId = WalletIdHdRnd (root ^. HD.hdRootId)
            wkey = (wId, keyToWalletDecrCredentials (KeyForRegular esk))

        -- Insert the 'EncryptedSecretKey' into the 'Keystore'
        liftIO $ Keystore.insert wId esk (wallet ^. walletKeystore)

        -- Synchronously restore the wallet balance, and begin to
        -- asynchronously reconstruct the wallet's history.
        let prefilter :: Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta])
            prefilter blund =
                blundToResolvedBlock getTimeBySlot blund <&> \case
                    Nothing -> (M.empty, [])
                    Just rb -> prefilterBlock rb wId esk
        coins <- liftIO $ restoreWallet wallet root wkey prefilter

        -- Return the wallet information, with an updated balance.
        updateSyncState wallet wId ((mkRoot now root) { V1.walBalance = V1 coins })

    mkRoot :: Timestamp -> HD.HdRoot -> V1.Wallet
    mkRoot now hdRoot = V1.Wallet {
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

    getTimeBySlot :: SlotId -> IO Timestamp
    getTimeBySlot n = do
        time <- rightToMaybe <$> getSlotStart (wallet ^. Kernel.walletNode) n
        defaultTime <- getCurrentTimestamp
        return $ fromMaybe defaultTime time


-- | Updates the 'SpendingPassword' for this wallet.
updateWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.WalletId
             -> V1.WalletUpdate
             -> m (Either UpdateWalletError V1.Wallet)
updateWallet wallet wId (V1.WalletUpdate v1Level v1Name) = runExceptT $ do
    rootId <- withExceptT UpdateWalletWalletIdDecodingFailed $ fromRootId wId
    v1wal <- fmap (uncurry toWallet) $
               withExceptT (UpdateWalletError . V1) $ ExceptT $ liftIO $
                 Kernel.updateHdWallet wallet rootId newLevel newName
    updateSyncState wallet (WalletIdHdRnd rootId) v1wal
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
    v1wal <- fmap (uncurry toWallet) $
              withExceptT UpdateWalletPasswordError $ ExceptT $ liftIO $
                Kernel.updatePassword wallet rootId oldPwd newPwd
    updateSyncState wallet (WalletIdHdRnd rootId) v1wal

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
getWallet :: MonadIO m
          => Kernel.PassiveWallet
          -> V1.WalletId
          -> Kernel.DB
          -> m (Either GetWalletError V1.Wallet)
getWallet wallet wId db = runExceptT $ do
    rootId <- withExceptT GetWalletWalletIdDecodingFailed (fromRootId wId)
    v1wal <- fmap (toWallet db) $
                withExceptT (GetWalletError . V1) $ exceptT $
                    Kernel.lookupHdRootId db rootId
    updateSyncState wallet (WalletIdHdRnd rootId) v1wal

-- | Gets all the wallets known to this edge node.
--
-- NOTE: The wallet sync state is not set here; use 'updateSyncState' to
--       get a correct result.
--
-- TODO: Avoid IxSet creation [CBR-347].
getWallets :: MonadIO m
           => Kernel.PassiveWallet
           -> Kernel.DB
           -> m (IxSet V1.Wallet)
getWallets wallet db =
    fmap IxSet.fromList $ forM (IxSet.toList allRoots) $ \root -> do
        let rootId = root ^. HD.hdRootId
        updateSyncState wallet (WalletIdHdRnd rootId) (toWallet db root)
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

-- | The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
-- by the invariants established in the 'Blund'.
blundToResolvedBlock :: (SlotId -> IO Timestamp) -> Blund -> IO (Maybe ResolvedBlock)
blundToResolvedBlock getTimeBySlot (b,u) = do
    case b of
        Left _ ->  return Nothing
        Right mainBlock ->  do
            let slot = mainBlock ^. mainBlockSlot
            time <- getTimeBySlot slot
            return . Just $ fromRawResolvedBlock
              (UnsafeRawResolvedBlock mainBlock spentOutputs' time)
  where
      spentOutputs' = map (map fromJust) $ undoTx u

updateSyncState :: MonadIO m
                => Kernel.PassiveWallet
                -> WalletId
                -> V1.Wallet
                -> m V1.Wallet
updateSyncState wallet wId v1wal = do
    wss <- M.lookup wId <$> readMVar (wallet ^. walletRestorationTask)
    return v1wal { V1.walSyncState = toSyncState wss }

