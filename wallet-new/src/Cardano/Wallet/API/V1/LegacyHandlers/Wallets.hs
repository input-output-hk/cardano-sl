module Cardano.Wallet.API.V1.LegacyHandlers.Wallets (
      handlers

    -- * Internals, exposed only for testing or in internal endpoints
    , isNodeSufficientlySynced
    , newWallet
    , addWalletInfo
    ) where

import           Universum
import           UnliftIO (MonadUnliftIO)

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods as V0
import qualified Pos.Wallet.Web.State as V0 (WalletSnapshot, askWalletSnapshot)
import qualified Pos.Wallet.Web.State.Storage as V0

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Pos.Chain.Update ()
import           Pos.Core as Core (Config (..), configBlkSecurityParam)
import qualified Pos.Core as Core

import           Pos.Util (HasLens (..))
import qualified Pos.Wallet.WalletMode as V0
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic,
                     MonadWalletLogicRead)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Servant


-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: Core.Config -> ServerT Wallets.API MonadV1
handlers coreConfig = newWallet coreConfig
    :<|> listWallets
    :<|> updatePassword
    :<|> deleteWallet
    :<|> getWallet
    :<|> updateWallet
    :<|> getUtxoStatistics

-- | Pure function which returns whether or not the underlying node is
-- \"synced enough\" to allow wallet creation/restoration. The notion of
-- \"synced enough\" is quite vague and if made too stringent could prevent
-- the backend to operate normally for wallets which are on a slow network
-- or are struggling to keep up. Therefore we consider a node to be \"synced
-- enough\" with the blockchain if we are not lagging more than @k@ slots, where
-- @k@ comes from the 'blkSecurityParam'.
isNodeSufficientlySynced :: Core.BlockCount -> V0.SyncProgress -> Bool
isNodeSufficientlySynced k spV0 =
    let blockchainHeight = fromMaybe (Core.BlockCount maxBound)
                                     (Core.getChainDifficulty <$> V0._spNetworkCD spV0)
        localHeight = Core.getChainDifficulty . V0._spLocalCD $ spV0
        remainingBlocks = blockchainHeight - localHeight

        in remainingBlocks <= k

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
    => Core.Config
    -> NewWallet
    -> m (WalletResponse Wallet)
newWallet coreConfig NewWallet{..} = do

    spV0 <- V0.syncProgress
    syncPercentage <- migrate spV0

    -- Do not allow creation or restoration of wallets if the underlying node
    -- is still catching up.
    unless (isNodeSufficientlySynced (configBlkSecurityParam coreConfig) spV0)
        $ throwM (NodeIsStillSyncing syncPercentage)

    let newWalletHandler CreateWallet  = V0.newWalletNoThrow
        newWalletHandler RestoreWallet = V0.restoreWalletFromSeedNoThrow coreConfig
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
    return $ single (V1.computeUtxoStatistics V1.log10 [])
