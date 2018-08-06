module Cardano.Wallet.API.V1.LegacyHandlers.Wallets (
      handlers

    -- * Internals, exposed only for testing
    , isNodeSufficientlySynced
    , newWallet
    ) where

import           Universum
import           UnliftIO (MonadUnliftIO)
import qualified Data.Map.Strict as Map
import           Formatting (build, sformat)
-- import           System.Wlog (logDebug)

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods as V0
import           Pos.Wallet.Web.State (WalletSnapshot, askWalletDB,
                     askWalletSnapshot, removeHistoryCache, setWalletSyncTip)
import qualified Pos.Wallet.Web.State.Storage as V0

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Errors
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets
import qualified Data.IxSet.Typed as IxSet
import           Pos.Chain.Update ()
import qualified Pos.Core as Core
import           Pos.Crypto (decodeBase58PublicKey)
import           Pos.Client.KeyStorage (addPublicKey)
import           Pos.Infra.StateLock (Priority (..), withStateLockNoMetrics)

import           Pos.Util (HasLens (..), maybeThrow)
import           Pos.Util.Servant (encodeCType)
import qualified Pos.Wallet.WalletMode as V0
import qualified Pos.Wallet.Web.Error.Types as V0
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic,
                     MonadWalletLogicRead)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Wallet.Web.Util (getWalletAccountIds)
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

    let newWalletHandler CreateWallet  = V0.newWallet
        newWalletHandler RestoreWallet = V0.restoreWalletFromSeed
        (V1 spendingPassword) = fromMaybe (V1 mempty) newwalSpendingPassword
        (BackupPhrase backupPhrase) = newwalBackupPhrase
    initMeta <- V0.CWalletMeta <$> pure newwalName
                              <*> migrate newwalAssuranceLevel
                              <*> pure 0
    let walletInit = V0.CWalletInit initMeta (V0.CBackupPhrase backupPhrase)
    single <$> do
        v0wallet <- newWalletHandler newwalOperation spendingPassword walletInit
                        `catch` rethrowDuplicateMnemonic
        ss <- askWalletSnapshot
        migrateWallet ss v0wallet True
  where
    -- NOTE: this is temporary solution until we get rid of V0 error handling and/or we lift error handling into types:
    --   https://github.com/input-output-hk/cardano-sl/pull/2811#discussion_r183469153
    --   https://github.com/input-output-hk/cardano-sl/pull/2811#discussion_r183472103
    rethrowDuplicateMnemonic (e :: V0.WalletError) =
        case e of
            V0.DuplicateWalletError _ -> throwM WalletAlreadyExists
            _ -> throwM e

-- | Returns the full (paginated) list of wallets.
listWallets :: ( MonadThrow m
               , V0.MonadWalletLogicRead ctx m
               , V0.MonadBlockchainInfo m
               )
            => RequestParams
            -> FilterOperations Wallet
            -> SortOperations Wallet
            -> m (WalletResponse [Wallet])
listWallets params fops sops = do
    ws <- askWalletSnapshot
    respondWith params fops sops (IxSet.fromList <$> do
        walletsWithInfo <- V0.getWalletsWithInfo ws
        mapM (migrateOneWallet ws) walletsWithInfo)
  where
    migrateOneWallet :: ( V0.MonadWalletLogicRead ctx m
                        , V0.MonadBlockchainInfo m
                        ) => WalletSnapshot -> (V0.CWallet, V0.WalletInfo) -> m Wallet
    migrateOneWallet ws (wallet, _) = migrateWallet ws wallet True

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
        ss <- askWalletSnapshot
        wallet <- V0.getWallet wid'
        migrateWallet ss wallet True

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
    ss <- askWalletSnapshot
    wid' <- migrate wid
    wallet <- V0.getWallet wid' `catch` rethrowWalletNotFound
    single <$> migrateWallet ss wallet True
  where
    rethrowWalletNotFound (e :: V0.WalletError) =
        case e of
            V0.NoSuchWalletError _ -> throwM WalletNotFound
            _ -> throwM e

migrateWallet
    :: ( V0.MonadWalletLogicRead ctx m
       , V0.MonadBlockchainInfo m
       )
    => WalletSnapshot
    -> V0.CWallet
    -> Bool
    -> m Wallet
migrateWallet snapshot wallet walletIsReady = do
    let walletId = V0.cwId wallet
    walletInfo <- if walletIsReady
        then maybeThrow WalletNotFound $ V0.getWalletInfo walletId snapshot
        else
            -- Wallet is not ready yet (because of restoring),
            -- the only information we can provide is the default one.
            maybeThrow WalletNotFound $ V0.getUnreadyWalletInfo walletId snapshot
    walletIsExternal <- V0.isWalletExternal walletId
    let walletType = if walletIsExternal then WalletExternal else WalletRegular
    currentDepth <- V0.networkChainDifficulty
    migrate (wallet, walletInfo, walletType, currentDepth)

updateWallet
    :: (V0.MonadWalletLogic ctx m
       , V0.MonadBlockchainInfo m
       )
    => WalletId
    -> WalletUpdate
    -> m (WalletResponse Wallet)
updateWallet wid WalletUpdate{..} = do
    ws <- askWalletSnapshot
    wid' <- migrate wid
    assurance <- migrate uwalAssuranceLevel
    walletMeta <- maybe (throwM WalletNotFound) pure $ V0.getWalletMeta wid' ws
    updated <- V0.updateWallet wid' walletMeta
        { V0.cwName = uwalName
        , V0.cwAssurance = assurance
        }
    single <$> do
        -- reacquire the snapshot because we did an update
        ws' <- askWalletSnapshot
        migrateWallet ws' updated True

-- | Check if external wallet is presented in node's wallet db.
checkExternalWallet
    :: ( V0.MonadWalletLogic ctx m
       , V0.MonadWalletHistory ctx m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       )
    => Text
    -> m (WalletResponse WalletAndTxHistory)
checkExternalWallet encodedExtPubKey = do
    publicKey <- case decodeBase58PublicKey encodedExtPubKey of
        Left problem    -> throwM (InvalidPublicKey $ sformat build problem)
        Right publicKey -> return publicKey

    ws <- askWalletSnapshot
    let walletId = encodeCType . Core.makePubKeyAddressBoot $ publicKey
    walletExists <- V0.doesWalletExist walletId
    (v0wallet, transactions, isWalletReady) <- if walletExists
        then do
            -- Wallet is here, it means that user already used this wallet (for example,
            -- hardware device) on this computer, so we have to return stored information
            -- about this wallet and history of transactions (if any transactions was made).
            --
            -- By default we have to specify account and address for getting transactions
            -- history. But currently all we have is 'encodedExtPubKey', so we return
            -- complete history of transactions, for all accounts and addresses.
            let allAccounts = getWalletAccountIds ws walletId
                -- We want to get a complete history, so we shouldn't specify an address.
                address = Nothing

            (V0.WalletHistory history, _) <- V0.getHistory walletId
                                                           (const allAccounts)
                                                           address

            v1Transactions <- mapM (\(_, (v0Tx, _)) -> migrate v0Tx) $ Map.toList history

            (,,) <$> V0.getWallet walletId
                 <*> pure v1Transactions
                 <*> pure True
        else do
            -- No such wallet in db, it means that this wallet (for example, hardware
            -- device) was not used on this computer. But since this wallet _could_ be
            -- used on another computer, we have to (try to) restore this wallet.
            -- Since there's no wallet meta-data, we use default one.
            let largeCurrencyUnit = 0
                defaultMeta = V0.CWalletMeta "ADA external wallet"
                                             V0.CWAStrict
                                             largeCurrencyUnit
                -- This is a new wallet, currently un-synchronized, so there's no
                -- history of transactions yet.
                transactions = []
            (,,) <$> restoreExternalWallet defaultMeta encodedExtPubKey
                 <*> pure transactions
                 <*> pure False -- We restore wallet, so it's unready yet.

    v1wallet <- migrateWallet ws v0wallet isWalletReady
    let walletAndTxs = WalletAndTxHistory v1wallet transactions
    single <$> pure walletAndTxs

-- | Creates a new or restores an existing external @wallet@ given a 'NewExternalWallet' payload.
-- Returns to the client the representation of the created or restored wallet in the 'Wallet' type.
newExternalWallet
    :: ( MonadThrow m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       , V0.MonadBlockchainInfo m
       , V0.MonadWalletLogic ctx m
       )
    => NewExternalWallet
    -> m (WalletResponse Wallet)
newExternalWallet NewExternalWallet{..} = do
    let newWalletHandler CreateWallet  = createNewExternalWallet
        newWalletHandler RestoreWallet = restoreExternalWallet
    walletMeta <- V0.CWalletMeta <$> pure newewalName
                                 <*> migrate newewalAssuranceLevel
                                 <*> pure 0
    single <$> do
        v0wallet <- newWalletHandler newewalOperation walletMeta newewalExtPubKey
        ss <- askWalletSnapshot
        migrateWallet ss v0wallet True

-- | Creates new external wallet.
--
-- There's no spending password, because it's assumed that
-- extenal wallet has its own security mechanism.
-- For example, in Ledger Nano S device it's 4 digits PIN-code.
--
-- There's no backup phrase as well, because it's assumed that
-- external wallet already received backup phrase, created
-- a secret key based on it and stored this key.
createNewExternalWallet
    :: ( MonadThrow m
       , V0.MonadWalletLogic ctx m
       )
    => V0.CWalletMeta
    -> Text
    -> m V0.CWallet
createNewExternalWallet walletMeta encodedExtPubKey = do
    publicKey <- case decodeBase58PublicKey encodedExtPubKey of
        Left problem    -> throwM (InvalidPublicKey $ sformat build problem)
        Right publicKey -> return publicKey

    -- Add this public key in the 'public.key' file. Public key will be used during
    -- synchronization with the blockchain.
    addPublicKey publicKey
    let walletId = encodeCType . Core.makePubKeyAddressBoot $ publicKey
        isReady  = True -- A brand new wallet doesn't need syncing with the blockchain.

    -- Create new external wallet.
    -- This is safe: if the client will try to create an external wallet from the same
    -- extended public key - error will be thrown.
    V0.CWallet{..} <- V0.createWalletSafe walletId walletMeta isReady

    -- Add initial account in this external wallet.
    addInitialAccount cwId

    db <- askWalletDB
    removeHistoryCache db walletId
    -- BListener checks current syncTip before applying update,
    -- thus setting it up to date manually here
    withStateLockNoMetrics HighPriority $ \tip -> setWalletSyncTip db walletId tip
    V0.getWallet walletId

-- | Restore external wallet using it's extended public key and meta-data.
restoreExternalWallet
    :: ( MonadThrow m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       , V0.MonadWalletLogic ctx m
       )
    => V0.CWalletMeta
    -> Text
    -> m V0.CWallet
restoreExternalWallet walletMeta encodedExtPubKey = do
    publicKey <- case decodeBase58PublicKey encodedExtPubKey of
        Left problem    -> throwM (InvalidPublicKey $ sformat build problem)
        Right publicKey -> return publicKey

    let walletId = encodeCType . Core.makePubKeyAddressBoot $ publicKey

    -- Add this public key in the 'public.key' file. Public key will be used during
    -- synchronization with the blockchain.
    addPublicKey publicKey

    let isReady = False -- Because we want to sync this wallet with the blockchain!

    -- Create new external wallet with initial account.
    V0.CWallet{..} <- V0.createWalletSafe walletId walletMeta isReady
    addInitialAccount cwId

    -- Restoring this wallet...
    V0.restoreExternalWallet publicKey

addInitialAccount
    :: ( MonadThrow m
       , V0.MonadWalletLogic ctx m
       )
    => V0.CId V0.Wal
    -> m ()
addInitialAccount cwId = do
    let accountMeta    = V0.CAccountMeta { caName = "Initial account" }
        accountInit    = V0.CAccountInit { caInitWId = cwId, caInitMeta = accountMeta }
        includeUnready = True
    void $ V0.newExternalAccountIncludeUnready includeUnready accountInit

-- | On disk, once imported or created, there's so far not much difference
-- between a wallet and an external wallet, except one: node stores a public key
-- for external wallet, there's no secret key.
deleteExternalWallet
    :: (V0.MonadWalletLogic ctx m)
    => Text
    -> m NoContent
deleteExternalWallet encodedExtPubKey =
    case decodeBase58PublicKey encodedExtPubKey of
        Left problem    -> throwM (InvalidPublicKey $ sformat build problem)
        Right publicKey -> V0.deleteExternalWallet publicKey
