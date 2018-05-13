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
import qualified Pos.Wallet.Web.State as V0 (WalletSnapshot, askWalletSnapshot, askWalletDB)
import           Pos.Wallet.Web.State (setWalletSyncTip, removeHistoryCache)
import qualified Pos.Wallet.Web.State.Storage as V0

import           Cardano.Crypto.Wallet (xpub)
import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Errors
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets
import qualified Data.IxSet.Typed as IxSet
import qualified Pos.Core as Core
import           Pos.Crypto (PublicKey (..))
import           Pos.Update.Configuration ()
import           Pos.Client.KeyStorage (addPublicKey)
import           Pos.StateLock (Priority (..), withStateLockNoMetrics)

import           Pos.Util (HasLens (..))
import           Pos.Util.Servant (encodeCType)
import qualified Pos.Wallet.WalletMode as V0
import qualified Pos.Wallet.Web.Error.Types as V0
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic, MonadWalletLogicRead)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)

import           Servant
import           Data.Maybe (fromJust)
import           Data.ByteString.Base58 (bitcoinAlphabet, decodeBase58)
import           Test.QuickCheck (arbitrary, generate)

-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => ServerT Wallets.API MonadV1
handlers = newWallet
    :<|> listWallets
    :<|> updatePassword
    :<|> deleteWallet
    :<|> getWallet
    :<|> updateWallet
    :<|> newExternalWallet
    :<|> newAddressPath


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
        (V1 backupPhrase) = newwalBackupPhrase
    initMeta <- V0.CWalletMeta <$> pure newwalName
                              <*> migrate newwalAssuranceLevel
                              <*> pure 0
    let walletInit = V0.CWalletInit initMeta backupPhrase
    single <$> do
        v0wallet <- newWalletHandler newwalOperation spendingPassword walletInit
                        `catch` rethrowDuplicateMnemonic
        ss <- V0.askWalletSnapshot
        addWalletInfo ss v0wallet
  where
    -- NOTE: this is temporary solution until we get rid of V0 error handling and/or we lift error handling into types:
    --   https://github.com/input-output-hk/cardano-sl/pull/2811#discussion_r183469153
    --   https://github.com/input-output-hk/cardano-sl/pull/2811#discussion_r183472103
    rethrowDuplicateMnemonic (e :: V0.WalletError) =
        case e of
            V0.RequestError "Wallet with that mnemonics already exists" -> throwM WalletAlreadyExists
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

-- | Creates a new or restores an existing external @wallet@ given a 'NewExternalWallet' payload.
-- Returns to the client the representation of the created or restored wallet in the 'Wallet' type.
newExternalWallet
    :: ( MonadThrow m
       , V0.MonadBlockchainInfo m
       , V0.MonadWalletLogic ctx m
       )
    => NewExternalWallet
    -> m (WalletResponse Wallet)
newExternalWallet NewExternalWallet{..} = do
    let newWalletHandler CreateWallet  = createNewExternalWallet
        newWalletHandler RestoreWallet = error "Restore external wallet, TODO"
    walletMeta <- V0.CWalletMeta <$> pure newewalName
                                 <*> migrate newewalAssuranceLevel
                                 <*> pure 0
    single <$> do
        v0wallet <- newWalletHandler newewalOperation walletMeta newewalExtPubKey
        ss <- V0.askWalletSnapshot
        addWalletInfo ss v0wallet

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
       , V0.MonadBlockchainInfo m
       , V0.MonadWalletLogic ctx m
       )
    => V0.CWalletMeta
    -> Text
    -> m V0.CWallet
createNewExternalWallet walletMeta encodedExtPubKey = do
    -- 'encodedExtPubKey' is Base58-encoded binary string that contains extended public key
    -- of external wallet.
    publicKey <- makePublicKeyFrom encodedExtPubKey
    -- Add this public key in the 'public.key' file. Public key will be used during
    -- synchronization with the blockchain.
    addPublicKey publicKey
    walletId <- makeWalletIdFrom publicKey
    let isReady = True -- A brand new wallet doesn't need syncing with the blockchain.

    -- Create new external wallet.
    -- This is safe: if the client will try to create an external wallet from the same
    -- extended public key - error will be thrown.
    V0.CWallet{..} <- V0.createWalletSafe walletId walletMeta isReady

    -- Add initial account in this external wallet.
    let accountMeta    = V0.CAccountMeta { caName = "Initial account" }
        accountInit    = V0.CAccountInit { caInitWId = cwId, caInitMeta = accountMeta }
        includeUnready = True
    void $ V0.newExternalAccountIncludeUnready includeUnready accountInit

    db <- V0.askWalletDB
    removeHistoryCache db walletId
    -- BListener checks current syncTip before applying update,
    -- thus setting it up to date manually here
    withStateLockNoMetrics HighPriority $ \tip -> setWalletSyncTip db walletId tip
    V0.getWallet walletId
  where
    makePublicKeyFrom encodedXPub = do
        let rawExtPubKey = decodeBase58 bitcoinAlphabet . encodeUtf8 $ encodedXPub
        when (isNothing rawExtPubKey) $
            throwM (UnknownError "Ext public key is not in proper Base58-form.")

        let extPubKey = xpub $ fromJust rawExtPubKey
        when (isLeft extPubKey) $ do
            let (problem:_) = lefts [extPubKey]
            throwM (UnknownError $ "Invalid ext public key: " <> toText problem)

        let (xPub:_) = rights [extPubKey] -- We already know that it's 'Right'.
        return $ PublicKey xPub

    makeWalletIdFrom publicKey =
        return $ encodeCType . Core.makePubKeyAddressBoot $ publicKey

-- | Creates a new BIP44 derivation path for an external wallet.
newAddressPath
    :: ( MonadThrow m
       , V0.MonadBlockchainInfo m
       , V0.MonadWalletLogic ctx m
       )
    => WalletId
    -> m (WalletResponse AddressPath)
newAddressPath _ =
    single <$> (liftIO $ generate arbitrary)
