module Cardano.Wallet.API.V1.LegacyHandlers.Wallets (
      handlers

    -- * Internals, exposed only for testing
    , isNodeSufficientlySynced
    , newWallet
    ) where

import           Universum
import           UnliftIO (MonadUnliftIO)

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods as V0
import qualified Pos.Wallet.Web.State as V0 (WalletSnapshot, askWalletSnapshot)
import qualified Pos.Wallet.Web.State.Storage as V0

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Errors
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Pos.Chain.Update ()
import qualified Pos.Core as Core

import           Pos.Util (HasLens (..))
import qualified Pos.Wallet.WalletMode as V0
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic,
                     MonadWalletLogicRead)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Servant


-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: HasConfigurations
         => ServerT Wallets.API MonadV1
handlers = newWallet
    :<|> listWallets
    :<|> updatePassword
    :<|> deleteWallet
    :<|> getWallet
    :<|> updateWallet
    :<|> getUtxoStatistics
    :<|> checkExternalWallet
    :<|> newExternalWallet
    :<|> deleteExternalWallet

-- | Pure function which returns whether or not the underlying node is
-- \"synced enough\" to allow wallet creation/restoration. The notion of
-- \"synced enough\" is quite vague and if made too stringent could prevent
-- the backend to operate normally for wallets which are on a slow network
-- or are struggling to keep up. Therefore we consider a node to be \"synced
-- enough\" with the blockchain if we are not lagging more than @k@ slots, where
-- @k@ comes from the 'blkSecurityParam'.
isNodeSufficientlySynced :: Core.HasProtocolConstants => V0.SyncProgress -> Bool
isNodeSufficientlySynced spV0 =
    let blockchainHeight = fromMaybe (Core.BlockCount maxBound)
                                     (Core.getChainDifficulty <$> V0._spNetworkCD spV0)
        localHeight = Core.getChainDifficulty . V0._spLocalCD $ spV0
        remainingBlocks = blockchainHeight - localHeight

        in remainingBlocks <= Core.blkSecurityParam

-- | Creates a new or restores an existing @wallet@ given a 'NewWallet' payload.
-- Returns to the client the representation of the created or restored
-- wallet in the 'Wallet' type.
newWallet
    :: (MonadThrow m
       , MonadUnliftIO m
       , MonadWalletLogic ctx m
       , V0.MonadBlockchainInfo m
       , HasLens SyncQueue ctx SyncQueue
       )
    => NewWallet
    -> m (WalletResponse Wallet)
newWallet NewWallet{..} = do

    spV0 <- V0.syncProgress
    syncPercentage <- migrate spV0

    -- Do not allow creation or restoration of wallets if the underlying node
    -- is still catching up.
    unless (isNodeSufficientlySynced spV0) $ throwM (NodeIsStillSyncing syncPercentage)

    let newWalletHandler CreateWallet  = V0.newWalletNoThrow
        newWalletHandler RestoreWallet = V0.restoreWalletFromSeedNoThrow
        (V1 spendingPassword) = fromMaybe (V1 mempty) newwalSpendingPassword
        (BackupPhrase backupPhrase) = newwalBackupPhrase
    initMeta <- V0.CWalletMeta <$> pure newwalName
                              <*> migrate newwalAssuranceLevel
                              <*> pure 0
    let walletInit = V0.CWalletInit initMeta (V0.CBackupPhrase backupPhrase)
    single <$> do
        ev0wallet <- newWalletHandler newwalOperation spendingPassword walletInit
        case ev0wallet of
            Left cidWal -> do
                walletId <- migrate cidWal
                throwM (WalletAlreadyExists walletId)
            Right v0wallet -> do
                ss <- V0.askWalletSnapshot
                addWalletInfo ss v0wallet

-- | Returns the full (paginated) list of wallets.
listWallets :: ( MonadThrow m
               , V0.MonadWalletLogicRead ctx m
               , V0.MonadBlockchainInfo m
               )
            => RequestParams
            -> FilterOperations '[WalletId, Core.Coin] Wallet
            -> SortOperations Wallet
            -> m (WalletResponse [Wallet])
listWallets params fops sops = do
    ws <- V0.askWalletSnapshot
    currentDepth <- V0.networkChainDifficulty
    respondWith params fops sops (IxSet.fromList <$> do
        (V0.getWalletsWithInfo ws >>= (migrate @_ @[V1.Wallet] . map (\(w, i) -> (w,i,currentDepth)))))

updatePassword
    :: ( MonadWalletLogic ctx m
       , V0.MonadBlockchainInfo m
       )
    => WalletId -> PasswordUpdate -> m (WalletResponse Wallet)
updatePassword wid PasswordUpdate{..} = do
    wid' <- migrate wid
    let (V1 old) = pwdOld
        (V1 new) = pwdNew
    _ <- V0.changeWalletPassphrase wid' old new
    single <$> do
        ss <- V0.askWalletSnapshot
        wallet <- V0.getWallet wid'
        addWalletInfo ss wallet

-- | Deletes an exisiting wallet.
deleteWallet
    :: (MonadWalletLogic ctx m)
    => WalletId
    -> m NoContent
deleteWallet = V0.deleteWallet <=< migrate

getWallet :: ( MonadThrow m
             , MonadWalletLogicRead ctx m
             , V0.MonadBlockchainInfo m
             ) => WalletId -> m (WalletResponse Wallet)
getWallet wid = do
    ss <- V0.askWalletSnapshot
    wid' <- migrate wid
    wallet <- V0.getWallet wid'
    single <$> addWalletInfo ss wallet

addWalletInfo
    :: ( MonadThrow m
       , V0.MonadWalletLogicRead ctx m
       , V0.MonadBlockchainInfo m
       )
    => V0.WalletSnapshot
    -> V0.CWallet
    -> m Wallet
addWalletInfo snapshot wallet = do
    case V0.getWalletInfo (V0.cwId wallet) snapshot of
        Nothing ->
            throwM WalletNotFound
        Just walletInfo -> do
            currentDepth <- V0.networkChainDifficulty
            migrate (wallet, walletInfo, currentDepth)

updateWallet
    :: (V0.MonadWalletLogic ctx m
       , V0.MonadBlockchainInfo m
       )
    => WalletId
    -> WalletUpdate
    -> m (WalletResponse Wallet)
updateWallet wid WalletUpdate{..} = do
    ws <- V0.askWalletSnapshot
    wid' <- migrate wid
    assurance <- migrate uwalAssuranceLevel
    walletMeta <- maybe (throwM WalletNotFound) pure $ V0.getWalletMeta wid' ws
    updated <- V0.updateWallet wid' walletMeta
        { V0.cwName = uwalName
        , V0.cwAssurance = assurance
        }
    single <$> do
        -- reacquire the snapshot because we did an update
        ws' <- V0.askWalletSnapshot
        addWalletInfo ws' updated

-- | Gets Utxo statistics for a wallet.
-- | Stub, not calling data layer.
getUtxoStatistics
    :: (MonadWalletLogic ctx m)
    => WalletId
    -> m (WalletResponse UtxoStatistics)
getUtxoStatistics _ = do
    return $ single (V1.computeUtxoStatistics [])

-- | Check if external wallet is presented in node's wallet db.
checkExternalWallet
    :: -- ( V0.MonadWalletLogic ctx m
       -- , V0.MonadWalletHistory ctx m
       -- , MonadUnliftIO m
       -- , HasLens SyncQueue ctx SyncQueue
       -- )
       -- =>
    PublicKeyAsBase58
    -> m (WalletResponse WalletAndTxHistory)
checkExternalWallet _encodedRootPK =
    error "[CHW-54], Cardano Hardware Wallet, check external wallet, legacy handler, unimplemented yet."

-- | Creates a new or restores an existing external @wallet@ given a 'NewExternalWallet' payload.
-- Returns to the client the representation of the created or restored wallet in the 'Wallet' type.
newExternalWallet
    :: -- ( MonadThrow m
       -- , MonadUnliftIO m
       -- , HasLens SyncQueue ctx SyncQueue
       -- , V0.MonadBlockchainInfo m
       -- , V0.MonadWalletLogic ctx m
       -- )
       -- =>
    NewExternalWallet
    -> m (WalletResponse Wallet)
newExternalWallet NewExternalWallet{..} =
    error "[CHW-80], Cardano Hardware Wallet, new external wallet, legacy handler, unimplemented yet."

-- | On the disk, once imported or created, there's so far not much difference
-- between a wallet and an external wallet, except one: node stores a public key
-- for external wallet, there's no secret key.
deleteExternalWallet
    :: -- (V0.MonadWalletLogic ctx m)
       -- =>
    PublicKeyAsBase58
    -> m NoContent
deleteExternalWallet _encodedRootPK =
    error "[CHW-106], Cardano Hardware Wallet, delete external wallet, legacy handler, unimplemented yet."
