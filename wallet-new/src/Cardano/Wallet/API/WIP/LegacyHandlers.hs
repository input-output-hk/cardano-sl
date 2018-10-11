{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}


module Cardano.Wallet.API.WIP.LegacyHandlers (
      handlers
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map.Strict as Map
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Time.Units (fromMicroseconds, toMicroseconds)
import           Formatting (build, sformat)
import           Universum
import           UnliftIO (MonadUnliftIO)

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods as V0
import qualified Pos.Wallet.Web.State as V0 (WalletSnapshot, askWalletSnapshot)
import qualified Pos.Wallet.Web.State.Storage as V0

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.WIP as WIP
import           Pos.Chain.Genesis as Genesis (Config)
import           Pos.Chain.Txp (TxAux, TxpConfiguration)
import qualified Pos.Chain.Txp as Txp
import           Pos.Chain.Update ()
import           Pos.Client.KeyStorage (addPublicKey)
import qualified Pos.Core as Core
import           Pos.Crypto (PublicKey, decodeHash)

import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Util (HasLens, maybeThrow)
import           Pos.Util.Servant (decodeCType, encodeCType)
import qualified Pos.Wallet.WalletMode as V0
import           Pos.Wallet.Web.ClientTypes.Instances ()
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.Tracking.Sync (calculateEstimatedRemainingTime)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Wallet.Web.Util (getWalletAccountIds)
import           Servant


handlers :: (forall a. WalletWebMode a -> Handler a)
            -> Genesis.Config
            -> TxpConfiguration
            -> Diffusion WalletWebMode
            -> Server WIP.API
handlers naturalTransformation genesisConfig txpConfig diffusion =
         hoist' (Proxy @WIP.API) (handlersPlain genesisConfig txpConfig submitTx)
  where
    hoist'
        :: forall (api :: *). HasServer api '[]
        => Proxy api
        -> ServerT api WalletWebMode
        -> Server api
    hoist' p = hoistServer p naturalTransformation
    submitTx = sendTx diffusion

-- | All the @Servant@ handlers for wallet-specific operations.
handlersPlain :: Genesis.Config
         -> TxpConfiguration
         -> (TxAux -> WalletWebMode Bool)
         -> ServerT WIP.API WalletWebMode
handlersPlain genesisConfig txpConfig submitTx = checkExternalWallet genesisConfig
    :<|> newExternalWallet genesisConfig
    :<|> deleteExternalWallet
    :<|> newUnsignedTransaction
    :<|> newSignedTransaction txpConfig submitTx

newtype MigrationError
    = MigrationFailed Text
    deriving (Eq, Show)

instance Exception MigrationError

toTransactionEither :: V0.CTx -> Either MigrationError Transaction
toTransactionEither V0.CTx{..} = do
        txId      <- toTxId ctId
        let txConfirmations = ctConfirmations
        txAmount  <- toCoin ctAmount
        txInputs  <- toPaymentDistributions ctInputs
        txOutputs <- toPaymentDistributions ctOutputs

        let txType = if ctIsLocal
            then V1.LocalTransaction
            else V1.ForeignTransaction

        let txDirection = if ctIsOutgoing
            then V1.OutgoingTransaction
            else V1.IncomingTransaction

        let V0.CTxMeta{..} = ctMeta
        txCreationTime <- toTimestamp ctmDate
        txStatus <- toTransactionStatus ctCondition

        pure V1.Transaction{..}

toTransaction :: MonadThrow m  => V0.CTx -> m Transaction
toTransaction from = case toTransactionEither from of
    Left e   -> throwM e
    Right to -> pure to

toTxId :: V0.CTxId -> Either MigrationError (V1 Txp.TxId)
toTxId (V0.CTxId (V0.CHash h)) =
        let err = Left . MigrationFailed . mappend "Error migrating a TxId: "
        in either err (pure . V1) (decodeHash h)

toCoin :: V0.CCoin -> Either MigrationError (V1 Core.Coin)
toCoin c =
    let err = Left . MigrationFailed . mappend "error migrating V0.CCoin -> Core.Coin, mkCoin failed: "
    in either err (pure . V1) (decodeCType c)

toAddress :: V0.CId V0.Addr -> Either MigrationError (V1 Core.Address)
toAddress (V0.CId (V0.CHash h)) =
    let err = Left . MigrationFailed . mappend "Error migrating (V0.CId V0.Addr) -> Core.Address failed."
    in either err (pure . V1) (Core.decodeTextAddress h)

toPaymentDistribution :: (V0.CId V0.Addr, V0.CCoin) -> Either MigrationError V1.PaymentDistribution
toPaymentDistribution (cIdAddr, cCoin) = do
    pdAddress <- toAddress cIdAddr
    pdAmount  <- toCoin cCoin
    pure V1.PaymentDistribution {..}

toPaymentDistributions :: [(V0.CId V0.Addr, V0.CCoin)] -> Either MigrationError (NonEmpty V1.PaymentDistribution)
toPaymentDistributions [] = Left $ MigrationFailed "Failed migrating because the list was empty."
toPaymentDistributions (x:xs) =
    (:|) <$> toPaymentDistribution x <*> mapM toPaymentDistribution xs

toTimestamp :: POSIXTime -> Either MigrationError (V1 Core.Timestamp)
toTimestamp time =
    pure . V1 $ view (Lens.from Core.timestampSeconds) time

toTransactionStatus :: V0.CPtxCondition -> Either MigrationError (V1.TransactionStatus)
toTransactionStatus = Right . \case
    V0.CPtxCreating{} ->
        V1.Creating
    V0.CPtxApplying{} ->
        V1.Applying
    V0.CPtxInBlocks{} ->
        V1.InNewestBlocks
    V0.CPtxWontApply{} ->
        V1.WontApply
    V0.CPtxNotTracked ->
        V1.Persisted

fromAssuranceLevelEither :: V1.AssuranceLevel -> Either MigrationError V0.CWalletAssurance
fromAssuranceLevelEither V1.StrictAssurance = pure V0.CWAStrict
fromAssuranceLevelEither V1.NormalAssurance = pure V0.CWANormal

fromAssuranceLevel :: MonadThrow m  => V1.AssuranceLevel -> m V0.CWalletAssurance
fromAssuranceLevel from = case fromAssuranceLevelEither from of
    Left e   -> throwM e
    Right to -> pure to

-- | Check if external wallet is presented in node's wallet db.
checkExternalWallet
    :: ( V0.MonadWalletLogic ctx m
       , V0.MonadWalletHistory ctx m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       )
    => Genesis.Config
    -> PublicKeyAsBase58
    -> m (WalletResponse WalletAndTxHistory)
checkExternalWallet genesisConfig encodedRootPK = do
    rootPK <- mkPublicKeyOrFail encodedRootPK

    ws <- V0.askWalletSnapshot
    let walletId = encodeCType . Core.makePubKeyAddressBoot $ rootPK
    walletExists <- V0.doesWalletExist walletId
    (v0wallet, transactions, isWalletReady) <- if walletExists
        then do
            -- Wallet is here, it means that user already used this wallet (for example,
            -- hardware device) on this computer, so we have to return stored information
            -- about this wallet and history of transactions (if any transactions was made).
            --
            -- By default we have to specify account and address for getting transactions
            -- history. But currently all we have is root PK, so we return complete history
            -- of transactions, for all accounts and addresses.
            let allAccounts = getWalletAccountIds ws walletId
                -- We want to get a complete history, so we shouldn't specify an address.
                address = Nothing
            (V0.WalletHistory history, _) <- V0.getHistory walletId
                                                           (const allAccounts)
                                                           address
            v1Transactions <- mapM (\(_, (v0Tx, _)) -> toTransaction v0Tx) $ Map.toList history
            (,,) <$> V0.getWallet walletId
                 <*> pure v1Transactions
                 <*> pure True
        else do
            -- No such wallet in db, it means that this wallet (for example, hardware
            -- device) was not used on this computer. But since this wallet _could_ be
            -- used on another computer, we have to (try to) restore this wallet.
            -- Since there's no wallet meta-data, we use default one.
            let largeCurrencyUnit = 0
                defaultMeta = V0.CWalletMeta "External wallet"
                                             V0.CWAStrict
                                             largeCurrencyUnit
                -- This is a new wallet, currently un-synchronized, so there's no
                -- history of transactions yet.
                transactions = []
            (,,) <$> restoreExternalWallet genesisConfig defaultMeta encodedRootPK
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
    => Genesis.Config
    -> NewExternalWallet
    -> m (WalletResponse Wallet)
newExternalWallet genesisConfig (NewExternalWallet rootPK assuranceLevel name operation) = do
    let newExternalWalletHandler CreateWallet  = createNewExternalWallet
        newExternalWalletHandler RestoreWallet = restoreExternalWallet genesisConfig
    walletMeta <- V0.CWalletMeta <$> pure name
                                 <*> fromAssuranceLevel assuranceLevel
                                 <*> pure 0
    single <$> do
        v0wallet <- newExternalWalletHandler operation walletMeta rootPK
        ws <- V0.askWalletSnapshot
        migrateWallet ws v0wallet True

-- | Creates new external wallet.
createNewExternalWallet
    :: ( MonadThrow m
       , V0.MonadWalletLogic ctx m
       )
    => V0.CWalletMeta
    -> PublicKeyAsBase58
    -> m V0.CWallet
createNewExternalWallet walletMeta encodedRootPK = do
    rootPK <- mkPublicKeyOrFail encodedRootPK

    -- This extended public key will be used during synchronization
    -- with the blockchain.
    addPublicKey rootPK

    let walletId = encodeCType . Core.makePubKeyAddressBoot $ rootPK
        isReady  = True -- We don't need to sync new wallet with the blockchain.

    -- Create new external wallet.
    -- This is safe: if the client will try to create an external wallet from the same
    -- root public key - error will be thrown.
    void $ V0.createWalletSafe walletId walletMeta isReady

    addInitAccountInExternalWallet walletId

    V0.getWallet walletId

-- | Restore external wallet using it's root public key and metadata.
restoreExternalWallet
    :: ( MonadThrow m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       , V0.MonadWalletLogic ctx m
       )
    => Genesis.Config
    -> V0.CWalletMeta
    -> PublicKeyAsBase58
    -> m V0.CWallet
restoreExternalWallet genesisConfig walletMeta encodedRootPK = do
    rootPK <- mkPublicKeyOrFail encodedRootPK

    let walletId = encodeCType . Core.makePubKeyAddressBoot $ rootPK
     -- Public key will be used during synchronization with the blockchain.
    addPublicKey rootPK

    let isReady = False -- Because we want to sync this wallet with the blockchain!

    -- Create new external wallet with initial account.
    void $ V0.createWalletSafe walletId walletMeta isReady
    addInitAccountInExternalWallet walletId

    -- Restoring this wallet.
    V0.restoreExternalWallet genesisConfig rootPK

addInitAccountInExternalWallet
    :: ( MonadThrow m
       , V0.MonadWalletLogic ctx m
       )
    => V0.CId V0.Wal
    -> m ()
addInitAccountInExternalWallet walletId = do
    let accountName = "Initial account"
        accountMeta = V0.CAccountMeta accountName
        accountInit = V0.CAccountInit accountMeta walletId
        includeUnready = True
    void $ V0.newExternalAccountIncludeUnready includeUnready accountInit

-- | On the disk, once imported or created, there's so far not much difference
-- between a wallet and an external wallet, except one: node stores a public key
-- for external wallet, there's no secret key.
deleteExternalWallet
    :: (V0.MonadWalletLogic ctx m)
    => PublicKeyAsBase58
    -> m NoContent
deleteExternalWallet encodedRootPK = do
    rootPK <- mkPublicKeyOrFail encodedRootPK
    V0.deleteExternalWallet rootPK


toWalletId :: (V0.CId V0.Wal) -> Either MigrationError V1.WalletId
toWalletId (V0.CId (V0.CHash h)) = pure (V1.WalletId h)

toAssuranceLevel :: V0.CWalletAssurance -> Either MigrationError V1.AssuranceLevel
toAssuranceLevel V0.CWAStrict = pure V1.StrictAssurance
toAssuranceLevel V0.CWANormal = pure V1.NormalAssurance

toSyncState :: (V0.WalletSyncState, V0.SyncStatistics, Maybe Core.ChainDifficulty) -> Either MigrationError V1.SyncState
toSyncState (wss, stats, currentBlockchainHeight) =
    case wss of
        V0.NotSynced         -> V1.Restoring <$> toSyncProgress (stats, currentBlockchainHeight)
        V0.RestoringFrom _ _ -> V1.Restoring <$> toSyncProgress (stats, currentBlockchainHeight)
        V0.SyncedWith _      -> pure V1.Synced

toSyncProgress :: (V0.SyncStatistics, Maybe Core.ChainDifficulty) -> Either MigrationError V1.SyncProgress
toSyncProgress (V0.SyncStatistics{..}, currentBlockchainHeight) =
    let unknownCompletionTime = Core.Timestamp $ fromMicroseconds (fromIntegral (maxBound :: Int))
        percentage = case currentBlockchainHeight of
            Nothing -> 0.0
            Just nd | wspCurrentBlockchainDepth >= nd -> 100
            Just nd -> (fromIntegral wspCurrentBlockchainDepth / max 1.0 (fromIntegral nd)) * 100.0
        toMs (Core.Timestamp microsecs) =
            V1.mkEstimatedCompletionTime (round @Double (realToFrac (toMicroseconds microsecs) / 1000.0))
        tput (V0.SyncThroughput blocks) = V1.mkSyncThroughput blocks
        remainingBlocks = fmap (\total -> total - wspCurrentBlockchainDepth) currentBlockchainHeight
        in V1.SyncProgress <$> pure (toMs (maybe unknownCompletionTime
                                                 (calculateEstimatedRemainingTime wspThroughput)
                                                 remainingBlocks))
                           <*> pure (tput wspThroughput)
                           <*> pure (V1.mkSyncPercentage (floor @Double percentage))


toNewWalletEither :: (V0.CWallet, V0.WalletInfo, V1.WalletType, Maybe Core.ChainDifficulty) -> Either MigrationError V1.Wallet
toNewWalletEither (V0.CWallet{..}, V0.WalletInfo{..}, walletType, currentBlockchainHeight) =
    V1.Wallet <$> toWalletId cwId
              <*> pure (V0.cwName cwMeta)
              <*> toCoin cwAmount
              <*> pure cwHasPassphrase
              <*> toTimestamp cwPassphraseLU
              <*> toTimestamp _wiCreationTime
              <*> toAssuranceLevel (V0.cwAssurance _wiMeta)
              <*> toSyncState (_wiSyncState, _wiSyncStatistics, currentBlockchainHeight)
              <*> pure walletType

toNewWallet :: MonadThrow m  => (V0.CWallet, V0.WalletInfo, V1.WalletType, Maybe Core.ChainDifficulty) -> m V1.Wallet
toNewWallet from = case toNewWalletEither from of
    Left e   -> throwM e
    Right to -> pure to


migrateWallet
    :: ( V0.MonadWalletLogicRead ctx m
       , V0.MonadBlockchainInfo m
       )
    => V0.WalletSnapshot
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
            pure $ V0.getUnreadyWalletInfo snapshot
    walletIsExternal <- V0.isWalletExternal walletId
    let walletType = if walletIsExternal then WalletExternal else WalletRegular
    currentDepth <- V0.networkChainDifficulty
    toNewWallet (wallet, walletInfo, walletType, currentDepth)

mkPublicKeyOrFail
    :: MonadThrow m
    => PublicKeyAsBase58
    -> m PublicKey
mkPublicKeyOrFail encodedRootPK =
    case mkPublicKeyFromBase58 encodedRootPK of
        Left problem -> throwM (InvalidPublicKey $ sformat build problem)
        Right rootPK -> return rootPK

newUnsignedTransaction
    :: Payment
    -> m (WalletResponse UnsignedTransaction)
newUnsignedTransaction _payment =
    error "[CHW-57], Cardano Hardware Wallet, legacy handler. Please see new one."

-- | It is assumed that we received a transaction which was signed
-- on the client side (mobile client or hardware wallet).
-- Now we have to submit this transaction as usually.
newSignedTransaction
    :: -- forall ctx m . (V0.MonadWalletTxFull ctx m)
       -- =>
    TxpConfiguration
    -> (TxAux -> m Bool)
    -> SignedTransaction
    -> m (WalletResponse Transaction)
newSignedTransaction _txpConfig _submitTx _signedTx =
    error "[CHW-57], Cardano Hardware Wallet, unimplemented yet."
