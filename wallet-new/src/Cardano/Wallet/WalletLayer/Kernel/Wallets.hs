{-# LANGUAGE LambdaCase #-}
module Cardano.Wallet.WalletLayer.Kernel.Wallets (
      createWallet
    , createExternalWallet
    , updateWallet
    , updateWalletPassword
    , deleteWallet
    , deleteExternalWallet
    , getWallet
    , getWallets
    , getWalletUtxos
    , blundToResolvedBlock
    , prefilter
    ) where

import           Universum

import           Control.Monad.Except (throwError)
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map

import           Pos.Chain.Block (Blund)
import           Pos.Chain.Txp (Utxo)
import           Pos.Core (mkCoin)
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Core.Slotting (Timestamp)
import           Pos.Crypto.Signing

import           Cardano.Wallet.API.V1.Types (V1 (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Kernel.Addresses (newHdAddress)
import qualified Cardano.Wallet.Kernel.BIP39 as BIP39
import           Cardano.Wallet.Kernel.DB.AcidState (dbHdWallets)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.TxMeta (TxMeta)
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Internal (walletKeystore,
                     walletProtocolMagic, _wriProgress)
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock,
                     prefilterBlock)
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Restore (blundToResolvedBlock,
                     restoreWallet)
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.Kernel.Util.Core (getCurrentTimestamp)
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import           Cardano.Wallet.WalletLayer (CreateExternalWalletError (..),
                     CreateWallet (..), CreateWalletError (..),
                     DeleteExternalWalletError (..), DeleteWalletError (..),
                     GetUtxosError (..), GetWalletError (..),
                     UpdateWalletError (..), UpdateWalletPasswordError (..))
import           Cardano.Wallet.WalletLayer.Kernel.Conv

createWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> CreateWallet
             -> m (Either CreateWalletError V1.Wallet)
createWallet wallet newWalletRequest = liftIO $ do
    let nm = makeNetworkMagic $ wallet ^. walletProtocolMagic
    now  <- liftIO getCurrentTimestamp
    case newWalletRequest of
        CreateWallet newWallet@V1.NewWallet{..} ->
            case newwalOperation of
                V1.RestoreWallet -> restore nm newWallet now
                V1.CreateWallet  -> create newWallet now
        ImportWalletFromESK esk mbSpendingPassword ->
            restoreFromESK nm
                           esk
                           (spendingPassword mbSpendingPassword)
                           now
                           "Imported Wallet"
                           HD.AssuranceLevelNormal
  where
    create :: V1.NewWallet -> Timestamp -> IO (Either CreateWalletError V1.Wallet)
    create newWallet@V1.NewWallet{..} now = runExceptT $ do
      root <- withExceptT CreateWalletError $ ExceptT $
                Kernel.createHdWallet wallet
                                      (mnemonic newWallet)
                                      (spendingPassword newwalSpendingPassword)
                                      (fromAssuranceLevel newwalAssuranceLevel)
                                      (HD.WalletName newwalName)
      return (mkRoot newwalName newwalAssuranceLevel now root)

    restore :: NetworkMagic
            -> V1.NewWallet
            -> Timestamp
            -> IO (Either CreateWalletError V1.Wallet)
    restore nm newWallet@V1.NewWallet{..} now = do
        let esk    = snd $ safeDeterministicKeyGen
                             (BIP39.mnemonicToSeed (mnemonic newWallet))
                             (spendingPassword newwalSpendingPassword)
        restoreFromESK nm
                       esk
                       (spendingPassword newwalSpendingPassword)
                       now
                       newwalName
                       (fromAssuranceLevel newwalAssuranceLevel)

    restoreFromESK :: NetworkMagic
                   -> EncryptedSecretKey
                   -> PassPhrase
                   -> Timestamp
                   -> Text
                   -> HD.AssuranceLevel
                   -> IO (Either CreateWalletError V1.Wallet)
    restoreFromESK nm esk pwd now walletName hdAssuranceLevel = runExceptT $ do
        let rootId = HD.eskToHdRootId nm esk
            wId    = WalletIdHdRnd rootId

        -- Insert the 'EncryptedSecretKey' into the 'Keystore'
        liftIO $ Keystore.insert wId esk (wallet ^. walletKeystore)

        -- Synchronously restore the wallet balance, and begin to
        -- asynchronously reconstruct the wallet's history.
        let mbHdAddress = newHdAddress nm
                                       esk
                                       pwd
                                       (Kernel.defaultHdAccountId rootId)
                                       (Kernel.defaultHdAddressId rootId)
        case mbHdAddress of
            Nothing -> throwError (CreateWalletError Kernel.CreateWalletDefaultAddressDerivationFailed)
            Just hdAddress -> do
                (root, coins) <- withExceptT (CreateWalletError . Kernel.CreateWalletFailed) $ ExceptT $
                    restoreWallet
                      wallet
                      (pwd /= emptyPassphrase)
                      (hdAddress ^. HD.hdAddressAddress . fromDb)
                      (HD.WalletName walletName)
                      hdAssuranceLevel
                      esk

                -- Return the wallet information, with an updated balance.
                let root' = mkRoot walletName (toAssuranceLevel hdAssuranceLevel) now root
                updateSyncState wallet wId (root' { V1.walBalance = V1 coins })

    mkRoot :: Text -> V1.AssuranceLevel -> Timestamp -> HD.HdRoot -> V1.Wallet
    mkRoot v1WalletName v1AssuranceLevel now hdRoot = V1.Wallet {
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

    mnemonic (V1.NewWallet (V1.BackupPhrase m) _ _ _ _) = m
    spendingPassword = maybe emptyPassphrase coerce


-- Synchronously restore the wallet balance, and begin to
-- asynchronously reconstruct the wallet's history.
prefilter :: NetworkMagic
          -> EncryptedSecretKey
          -> Kernel.PassiveWallet
          -> WalletId
          -> Blund
          -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta])
prefilter nm esk wallet wId blund =
    blundToResolvedBlock (wallet ^. Kernel.walletNode) blund <&> \case
        Nothing -> (Map.empty, [])
        Just rb -> prefilterBlock nm rb [(wId,esk)]

createExternalWallet :: MonadIO m
                     => Kernel.PassiveWallet
                     -> V1.NewExternalWallet
                     -> m (Either CreateExternalWalletError V1.ExternalWallet)
createExternalWallet wallet newExternalWalletRequest = runExceptT $ do
    let encodedRootPK = V1.newewalRootPK newExternalWalletRequest
        name = V1.newewalName newExternalWalletRequest
        assuranceLevel = V1.newewalAssuranceLevel newExternalWalletRequest
    rootPK <- withExceptT CreateExternalWalletInvalidRootPK $ ExceptT $
        pure $ V1.mkPublicKeyFromBase58 encodedRootPK
    root <- withExceptT CreateExternalWalletError $ ExceptT $ liftIO $
        Kernel.createExternalHdWallet wallet
                                      rootPK
                                      (fromAssuranceLevel assuranceLevel)
                                      (HD.WalletName name)
    return (mkExternalWallet name assuranceLevel root)
  where
    mkExternalWallet :: Text -> V1.AssuranceLevel -> HD.HdRoot -> V1.ExternalWallet
    mkExternalWallet v1WalletName v1AssuranceLevel hdRoot = V1.ExternalWallet {
          ewalId             = walletId
        , ewalName           = v1WalletName
        , ewalBalance        = V1 (mkCoin 0)
        , ewalAssuranceLevel = v1AssuranceLevel
        }
      where
        walletId = toRootId $ hdRoot ^. HD.hdRootId

-- | Updates the 'SpendingPassword' for this wallet.
updateWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.WalletId
             -> V1.WalletUpdate
             -> m (Either UpdateWalletError V1.Wallet)
updateWallet wallet wId (V1.WalletUpdate v1Level v1Name) = runExceptT $ do
    rootId <- withExceptT UpdateWalletWalletIdDecodingFailed $ fromRootId wId
    v1wal <- fmap (uncurry toWallet) $
               withExceptT UpdateWalletError $ ExceptT $ liftIO $
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

-- | Deletes a wallet, together with every account & addresses belonging to it.
-- If this wallet was restoring, then the relevant async worker is correctly
-- canceled.
deleteWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.WalletId
             -> m (Either DeleteWalletError ())
deleteWallet wallet wId = runExceptT $ do
    rootId <- withExceptT DeleteWalletWalletIdDecodingFailed $ fromRootId wId
    withExceptT DeleteWalletError $ ExceptT $ liftIO $ do
      let nm = makeNetworkMagic (wallet ^. walletProtocolMagic)
      Kernel.removeRestoration wallet (WalletIdHdRnd rootId)
      Kernel.deleteHdWallet nm wallet rootId

-- | Deletes external wallets. Please note that there's no actions in the
-- 'Keystore', because it contains only root secret keys.
deleteExternalWallet :: MonadIO m
                     => Kernel.PassiveWallet
                     -> V1.PublicKeyAsBase58
                     -> m (Either DeleteExternalWalletError ())
deleteExternalWallet wallet encodedRootPK = runExceptT $ do
    rootPK <- withExceptT DeleteExternalWalletInvalidRootPK $ ExceptT $
        pure $ V1.mkPublicKeyFromBase58 encodedRootPK
    let nm = makeNetworkMagic (wallet ^. walletProtocolMagic)
        rootId = HD.pkToHdRootId nm rootPK
    withExceptT DeleteExternalWalletError $ ExceptT $ liftIO $ do
        Kernel.deleteExternalHdWallet wallet rootId

-- | Gets a specific wallet.
getWallet :: MonadIO m
          => Kernel.PassiveWallet
          -> V1.WalletId
          -> Kernel.DB
          -> m (Either GetWalletError V1.Wallet)
getWallet wallet wId db = runExceptT $ do
    rootId <- withExceptT GetWalletWalletIdDecodingFailed (fromRootId wId)
    v1wal <- fmap (toWallet db) $
                withExceptT GetWalletError $ exceptT $
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

updateSyncState :: MonadIO m
                => Kernel.PassiveWallet
                -> WalletId
                -> V1.Wallet
                -> m V1.Wallet
updateSyncState wallet wId v1wal = liftIO $ do
    wss      <- Kernel.lookupRestorationInfo wallet wId
    progress <- traverse _wriProgress wss
    return v1wal { V1.walSyncState = toSyncState progress }
