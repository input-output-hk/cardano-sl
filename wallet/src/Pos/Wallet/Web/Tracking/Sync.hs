{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- To support actual wallet accounts we listen to applications and rollbacks
-- of blocks, extract transactions from block and extract our
-- accounts (such accounts which we can decrypt).
-- We synchronise wallet-db (acidic-state) with node-db
-- and support last seen tip for each walletset.
-- There are severals cases when we must  synchronise wallet-db and node-db:
-- • When we relaunch wallet. Desynchronization can be caused by interruption
--   during blocks application/rollback at the previous launch,
--   then wallet-db can fall behind from node-db (when interrupted during rollback)
--   or vice versa (when interrupted during application)
-- • When a user wants to import a secret key. Then we must rely on the
--   Utxo (GStateDB), because blockchain can be large.

module Pos.Wallet.Web.Tracking.Sync
       ( syncWallet
       , processSyncRequest
       , processSyncError
       , trackingApplyTxs
       , trackingRollbackTxs
       , applyModifierToWallet
       , rollbackModifierFromWallet
       , firstGenesisHeader

       , txMempoolToModifier

       , buildTHEntryExtra
       , isTxEntryInteresting

       -- * Utility functions
       , calculateEstimatedRemainingTime

       -- Internal & test-use only
       , evalChange
       , syncWalletWithBlockchain
       , calculateThroughput
       , BoundedSyncTime (..)
       ) where

import           Control.Monad.Except (MonadError (throwError))
import           Universum
import           UnliftIO (MonadUnliftIO)
import           Unsafe (unsafeLast)

import           Control.Concurrent.STM (readTQueue)
import           Control.Exception.Safe (handleAny)
import           Control.Lens (to)
import qualified Data.DList as DL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (Microsecond, TimeUnit (..))
import           Formatting (build, float, sformat, shown, (%))
import           Pos.Block.Types (Blund, undoTx)
import           Pos.Client.Txp.History (TxHistoryEntry (..), txHistoryListToMap)
import           Pos.Core (Address, BlockCount (..), ChainDifficulty (..), HasConfiguration, HasGenesisHash,
                           HasDifficulty (..), HasProtocolConstants, HeaderHash, Timestamp (..),
                           blkSecurityParam, genesisHash, headerHash, headerSlotL, timestampToPosix)
import           Pos.Core.Block (BlockHeader (..), getBlockHeader, mainBlockTxPayload)
import           Pos.Core.Txp (TxAux (..), TxId, TxUndo)
import           Pos.Crypto (WithHash (..), shortHashF, withHash)
import           Pos.DB.Block (getBlund)
import qualified Pos.DB.Block.Load as GS
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead (..))
import qualified Pos.GState as GS
import           Pos.GState.BlockExtra (resolveForwardLink)
import           Pos.Slotting (MonadSlots (..), MonadSlotsData, getSlotStartPure, getSystemStartM)
import           Pos.Slotting.Types (SlottingData)
import           Pos.StateLock (Priority (..), StateLock, withStateLockNoMetrics)
import           Pos.Txp (UndoMap, flattenTxPayload, topsortTxs, _txOutputs)
import           Pos.Util.Chrono (getNewestFirst)
import           Pos.Util.LogSafe (buildSafe, logDebugSP, logErrorSP, logInfoSP, logWarningSP,
                                   secretOnlyF, secure)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Servant (encodeCType)
import           Pos.Util.Util (HasLens (..), getKeys, timed)
import           System.Wlog (CanLog, HasLoggerName, WithLogger, logDebug, logError, logInfo,
                              logWarning, modifyLoggerName)

import           Pos.Wallet.Web.ClientTypes (CId, CTxMeta (..), Wal)
import           Pos.Wallet.Web.Error.Types (WalletError (..))
import           Pos.Wallet.Web.Pending.Types (PtxBlockInfo,
                                               PtxCondition (PtxApplying, PtxInNewestBlocks))
import           Pos.Wallet.Web.State (CustomAddressType (..), SyncStatistics (..), WAddressMeta,
                                       WalletDB, WalletDbReader, WalletSnapshot,
                                       WalletSyncState (..))
import qualified Pos.Wallet.Web.State as WS
import qualified Pos.Wallet.Web.State.State as WS
import           Pos.Wallet.Web.Tracking.Decrypt (THEntryExtra (..), WalletDecrCredentials,
                                                  buildTHEntryExtra, isTxEntryInteresting)
import           Pos.Wallet.Web.Tracking.Modifier (CAccModifier (..), VoidModifier,
                                                   deleteAndInsertIMM, deleteAndInsertMM,
                                                   deleteAndInsertVM, indexedDeletions,
                                                   sortedInsertions)
import           Pos.Wallet.Web.Tracking.Types


-- | Sync a wallet with the last state of the blockchain, given its 'WalletDecrCredentials'.
-- The update of the balance will be done immediately and synchronously, the transaction history
-- will instead be recovered asynchronously.
syncWallet :: ( WalletDbReader ctx m
              , MonadDBRead m
              , WithLogger m
              , HasLens StateLock ctx StateLock
              , HasLens SyncQueue ctx SyncQueue
              , MonadMask m
              , MonadSlotsData ctx m
              , MonadUnliftIO m
              ) => WalletDecrCredentials -> m ()
syncWallet credentials = submitSyncRequest (newSyncRequest credentials)

-- | Asynchronously process a 'SyncRequest' by reading incoming
-- requests from a 'SyncQueue', in an infinite loop.
processSyncRequest :: ( WalletDbReader ctx m
                      , BlockLockMode ctx m
                      , MonadSlotsData ctx m
                      , HasConfiguration
                      , MonadIO m
                      ) => SyncQueue -> m ()
processSyncRequest syncQueue = do
    newRequest <- atomically (readTQueue syncQueue)
    syncWalletWithBlockchain newRequest >>= either processSyncError (const (logSuccess newRequest))
    processSyncRequest syncQueue

-- | Yields a new 'CAccModifier' using the information retrieved from the mempool, if any.
txMempoolToModifier :: WalletTrackingEnv ctx m
                    => WalletSnapshot
                    -> ([(TxId, TxAux)], UndoMap) -- ^ Transactions and UndoMap from mempool
                    -> WalletDecrCredentials
                    -> m CAccModifier
txMempoolToModifier ws (txs, undoMap) credentials = do
    let wHash (i, TxAux {..}, _) = WithHash taTx i
        getDiff       = const Nothing  -- no difficulty (mempool txs)
        getTs         = const Nothing  -- don't give any timestamp
        getPtxBlkInfo = const Nothing  -- no slot of containing block

    txsWUndo <- forM txs $ \(id, tx) -> case HM.lookup id undoMap of
        Just undo -> pure (id, tx, undo)
        Nothing -> do
            let errMsg sl = sformat ("There is no undo corresponding to TxId #"%secretOnlyF sl build%" from txp mempool") id
            logErrorSP errMsg
            throwM $ InternalError (errMsg secure)

    case topsortTxs wHash txsWUndo of
        Nothing      -> mempty <$ logWarning "txMempoolToModifier: couldn't topsort mempool txs"
        Just ordered -> do
            tipH <- DB.getTipHeader
            let usedAddresses = WS.getCustomAddresses ws WS.UsedAddr
            pure $
                trackingApplyTxs credentials usedAddresses getDiff getTs getPtxBlkInfo $
                map (\(_, tx, undo) -> (tx, undo, tipH)) ordered

-- | Process each 'SyncError'.
-- The current implementation just logs the errors without exposing it to the upper layers.
processSyncError :: ( WithLogger m , MonadIO m ) => SyncError -> m ()
processSyncError sr = case sr of
    GenesisBlockHeaderNotFound -> logError "Couldn't extract the genesis block header from the database."
    GenesisHeaderHashNotFound  -> logError "Couldn't extract the genesis header hash from the database."
    NoSyncStateAvailable walletId ->
        logWarningSP $ \sl -> sformat ("There is no sync state corresponding to wallet #"%secretOnlyF sl build) walletId
    NotSyncable walletId walletError -> do
        logErrorSP   $ \sl -> sformat ("Wallet #" % secretOnlyF sl build
                                                  % " is not syncable. Error was: "
                                                  % build) walletId walletError
    SyncFailed  walletId exception -> do
        let errMsg sl = "Sync failed for Wallet #" % secretOnlyF sl build
                                                   % ". An exception was raised during the sync process: "
                                                   % build
        logErrorSP $ \sl -> sformat (errMsg sl) walletId exception
    RestorationInvariantViolated walletId expectedRbd actualRbd -> do
        let errMsg sl = "Restoration invariant violated for Wallet #" % secretOnlyF sl build
                      % ". Expected restoration header hash was " % build % " , but this one was passed: " % build
        logErrorSP $ \sl -> sformat (errMsg sl) walletId (WS.getRestorationBlockDepth expectedRbd)
                                                         (WS.getRestorationBlockDepth actualRbd)
    StateTransitionNotAllowed walletId _ _ -> do
        let errMsg sl = "SyncState transition for Wallet #" % secretOnlyF sl build % " is not allowed."
        logErrorSP $ \sl -> sformat (errMsg sl) walletId


-- | Simply log that the wallet syncing has been completed.
logSuccess :: (WithLogger m, MonadIO m) => SyncRequest -> m ()
logSuccess SyncRequest{..} = do
  let (_, walletId) = srCredentials
  logInfoSP   $ \sl -> sformat ("Wallet #" % secretOnlyF sl build
                                           % " is now 100% synced.") walletId

-- | Iterates over blocks (using forward links) and reconstructs the transaction
-- history and the balance for the given wallet. In case of a restore, we deliberately ignore changes in the
-- balance happened _before_ the 'RestorationBlockDepth', so that the control can be yielded immediately to the
-- @BListener@ and transactions submitted whilst a wallet is restoring can be processed regularly.
syncWalletWithBlockchain
    :: forall ctx m.
    ( WalletDbReader ctx m
    , BlockLockMode ctx m
    , MonadSlotsData ctx m
    , HasConfiguration
    )
    => SyncRequest
    -> m SyncResult
syncWalletWithBlockchain syncRequest@SyncRequest{..} = setLogger $ do
    ws <- WS.askWalletSnapshot
    let (_, walletId) = srCredentials
    let onError       = pure . Left . SyncFailed walletId
    let internalError = InternalError . sformat ("Couldn't get block header of wallet by last synced hh: "%build)
    handleAny onError $ do
        case WS.getWalletSyncState ws walletId of
            Nothing                -> pure $ Left (NoSyncStateAvailable walletId)
            Just NotSynced         -> do
                genesisHeader <- firstGenesisHeader
                either (pure . Left . identity) (syncDo srOperation) genesisHeader

            -- FIXME(adn): There is a bit of duplication in these two paths.
            Just (SyncedWith wTip) -> do
                logDebugSP $ \sl ->
                    sformat ( "Resuming syncing of Wallet "
                            % secretOnlyF sl build
                            % " from HeaderHash "
                            % build
                            % "...") walletId wTip
                wHeaderMb <- DB.getHeader wTip
                -- We compare the syncRequest's 'TrackingOperation' expecting it to be a 'SyncRequest',
                -- and we abort if that's not the case.
                case srOperation of
                    RestoreWallet rbd -> pure $ Left (StateTransitionNotAllowed walletId SyncWallet rbd)
                    SyncWallet -> maybe (pure . Left . NotSyncable walletId $ internalError wTip) (syncDo srOperation) wHeaderMb
            Just (RestoringFrom expectedRbd wTip) -> do
                logDebugSP $ \sl ->
                    sformat ( "Wallet "
                            % secretOnlyF sl build
                            % " is restoring from a blockchain depth of "
                            % build
                            % "...") walletId (WS.getRestorationBlockDepth expectedRbd)
                wHeaderMb <- DB.getHeader wTip
                -- If we are trying to restore this wallet, we first check our internal model state is
                -- consistent.
                case srOperation of
                    -- A "double restore" is unnecessary and generally a violation of some internal invariant,
                    -- as the backend will automatically resume syncing upon restart. We try to separate "benign"
                    -- requests by checking if the 'RestorationBlockDepth' we got as input matches the one stored
                    -- in the model. If so, wo continue normally, otherwise we call out the mistake.
                    RestoreWallet actualRbd ->
                        if expectedRbd /= actualRbd
                            then pure $ Left $ RestorationInvariantViolated walletId expectedRbd actualRbd
                            else maybe (pure . Left . NotSyncable walletId $ internalError wTip)
                                       (syncDo (RestoreWallet expectedRbd)) wHeaderMb
                    SyncWallet -> do
                        -- If we are requesting to sync the wallet, this happens when we restart the backend and
                        -- we know nothing about the state of a wallet (up until now, anyway). We handle this
                        -- request by overriding the 'srOperation' field of the input 'SyncRequest' by treating this
                        -- as a continuation of the restoration process.
                        maybe (pure . Left . NotSyncable walletId $ internalError wTip)
                              (syncDo (RestoreWallet expectedRbd)) wHeaderMb
  where
    syncDo :: TrackingOperation -> BlockHeader -> m SyncResult
    syncDo trackingOp walletTipHeader = do
        let wdiff = (fromIntegral . heightOf $ walletTipHeader) :: Word32
        gstateTipH <- DB.getTipHeader
        let currentBlockchainDepth = heightOf gstateTipH

        -- First of all we check how far we are lagging behind with respect to
        -- the @currentBlockchainDepth@. If we are lagging _at least_ k blocks
        -- behind, it means we can sync without the block lock. To do so, we
        -- fetch from the database a @stableBlockHeader@, which is a 'BlockHeader'
        -- guaranteed (by the protocol) to not be rolled-back and which can
        -- be considered stable and fully persisted into the blockchain.
        (syncResult, wNewTip) <-
            if (currentBlockchainDepth > fromIntegral blkSecurityParam + fromIntegral wdiff) then do
                -- Wallet tip is "far" from gState tip,
                -- rollback can't occur more then @blkSecurityParam@ blocks,
                -- so we can sync wallet and GState without the block lock
                -- to avoid blocking of blocks verification/application.
                stableBlockHeader <- unsafeLast . getNewestFirst <$> GS.loadHeadersByDepth (blkSecurityParam + 1) (headerHash gstateTipH)
                logInfo $
                    sformat ( "Wallet's tip is far from GState tip. Syncing with the last stable known header " %build%
                              " (the tip of the blockchain - k blocks) without the block lock"
                            ) (headerHash stableBlockHeader)
                result <- syncWalletWithBlockchainUnsafe (syncRequest { srOperation = trackingOp }) walletTipHeader stableBlockHeader
                pure $ (Just result, stableBlockHeader)
            else pure (Nothing, walletTipHeader)

        let finaliseSyncUnderBlockLock = withStateLockNoMetrics HighPriority $ \tip -> do
                logInfo $ sformat ("Syncing wallet with "%build%" under the block lock") tip
                tipH <- maybe (error "No block header corresponding to tip") pure =<< DB.getHeader tip
                syncWalletWithBlockchainUnsafe (syncRequest { srOperation = SyncWallet }) wNewTip tipH

        case syncResult of
            Nothing         -> finaliseSyncUnderBlockLock
            Just (Right _)  -> finaliseSyncUnderBlockLock
            Just failedSync -> pure failedSync

----------------------------------------------------------------------------
-- Unsafe operations. Core logic.
----------------------------------------------------------------------------
-- These operation aren't atomic and don't take the block lock.

-- BE CAREFUL! This function iterates over blockchain, the blockchain can be large.
syncWalletWithBlockchainUnsafe
    :: forall ctx m .
    ( WalletDbReader ctx m
    , MonadDBRead m
    , WithLogger m
    , MonadSlotsData ctx m
    , HasConfiguration
    )
    => SyncRequest
    -> BlockHeader
    -- ^ Block header corresponding to wallet's tip. It can map
    -- to the genesis BlockHeader if this is a brand new wallet being
    -- synced or restored.
    -> BlockHeader
    -- ^ Blockchain's tip header hash
    -> m SyncResult
syncWalletWithBlockchainUnsafe syncRequest walletTip blockchainTip = setLogger $ do
    let credentials@(_, walletId) = srCredentials syncRequest
    systemStart  <- getSystemStartM
    slottingData <- GS.getSlottingData
    db <- WS.askWalletDB
    ws <- WS.getWalletSnapshot db

    let getBlockHeaderTimestamp = blockHeaderTimestamp systemStart slottingData

    let usedAddresses = WS.getCustomAddresses ws WS.UsedAddr

    -- Compute the next 'CAccModifier' and tentatively assess the throughput.
    ((mapModifier, newSyncTip), timeTook) <-
        timed "syncWalletWithBlockchainUnsafe.computeAccModifier" $
            computeAccModifier credentials getBlockHeaderTimestamp walletTip usedAddresses mempty 0

    let syncTime = BoundedSyncTime 10000 timeTook
    WS.updateSyncStatistics db walletId (SyncStatistics (calculateThroughput syncTime) (heightOf newSyncTip))

    -- Apply the 'CAccModifier' to the wallet state.
    applyModifierToWallet db (srOperation syncRequest) walletId newSyncTip mapModifier
    logDebugSP $ \sl -> sformat ("Applied " %buildSafe sl) mapModifier

    case headerHash newSyncTip == headerHash blockchainTip of
        True -> do
            logInfoSP $ \sl ->
                sformat ("Wallet "%secretOnlyF sl build%" has been synced with tip "
                        %shortHashF%", "%buildSafe sl)
                        walletId (headerHash newSyncTip) mapModifier
            -- If we have here is means we are fully synced with the blockchain,
            -- at which point we can forget about the distinction @restoration vs sync@. Even if a
            -- rollback occurs, we don't need to transition back the old "restoring" state,
            -- as it won't matter anymore.
            WS.setWalletSyncTip db walletId (headerHash newSyncTip)
            pure $ Right ()
        False -> syncWalletWithBlockchainUnsafe syncRequest newSyncTip blockchainTip

    where
        -- | Main workhorse which iterates over the blockchain and reconstruct the transaction
        -- history. Yields a new 'CAccModifier', give or take, every 10000 blocks.
        computeAccModifier :: WalletDecrCredentials
                           -> (BlockHeader -> Maybe Timestamp)
                           -> BlockHeader
                           -- ^ The current wallet 'HeaderHash'.
                           -> [(Address, HeaderHash)]
                           -- ^ Used addresses.
                           -> CAccModifier
                           -- ^ The initial CAccModifier we will fold on.
                           -> Int
                           -- ^ The initial block count accumulator. Every 10000 blocks we will stop the recursion.
                           -> m (CAccModifier, BlockHeader)
                           -- ^ The new wallet modifier and the new sync tip for the wallet.
        computeAccModifier credentials getBlockTimestamp wHeader usedAddresses currentModifier currentBlockCount = do
            case currentBlockCount >= 10000 of
                True -> do
                    let progress localDepth totalDepth = ((fromIntegral localDepth) * 100.0) /
                                                         (max 1.0 (fromIntegral totalDepth))
                    let renderProgress = progress (heightOf wHeader) (heightOf blockchainTip)
                    logDebug $ sformat ("Progress: " % float @Double % "%") renderProgress
                    pure (currentModifier, wHeader)
                False -> do
                    let walletId = snd credentials
                    if | heightOf blockchainTip > heightOf wHeader -> do
                             -- If wallet's syncTip is before than the current tip in the blockchain,
                             -- then it loads wallets starting with @wHeader@.
                             -- Sync tip can be before the current tip
                             -- when we call @syncWalletSetWithTip@ at the first time
                             -- or if the application was interrupted during rollback.
                             -- We don't load blocks explicitly, because blockain can be long.
                             nextBlund <- resolveForwardLink wHeader >>= (maybe (pure Nothing) getBlund)
                             case nextBlund of
                                 Nothing -> pure (currentModifier, blockchainTip)
                                 Just blund@(currentBlock, _) -> do
                                     let !newModifier = currentModifier <> applyBlock credentials usedAddresses blund getBlockTimestamp
                                     computeAccModifier credentials
                                                        getBlockTimestamp
                                                        (getBlockHeader currentBlock)
                                                        usedAddresses
                                                        newModifier
                                                        (currentBlockCount + 1)

                       | heightOf blockchainTip < heightOf wHeader -> do
                             -- This rollback can occur
                             -- if the application was interrupted during blocks application.
                             blunds <- getNewestFirst <$> GS.loadBlundsWhile (\b -> getBlockHeader b /= blockchainTip) (headerHash wHeader)
                             let newModifier = foldl' (\r b -> r <> rollbackBlock credentials usedAddresses b getBlockTimestamp) currentModifier blunds
                             pure (newModifier, getBlockHeader . fst . unsafeLast $ blunds)
                       | otherwise -> do
                             logInfoSP $ \sl -> sformat ("Wallet " % secretOnlyF sl build %" has finally caught up with the blockchain.") walletId
                             pure (currentModifier, blockchainTip)

        gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)

        blockHeaderTimestamp :: Timestamp -> SlottingData -> BlockHeader -> Maybe Timestamp
        blockHeaderTimestamp systemStart slottingData = \case
            BlockHeaderGenesis _ -> Nothing
            BlockHeaderMain h -> getSlotStartPure systemStart (h ^. headerSlotL) slottingData

        -- assuming that transactions are not created until syncing is complete
        ptxBlkInfo = const Nothing

        rollbackBlock :: WalletDecrCredentials
                      -> [(Address, HeaderHash)]
                      -> Blund
                      -> (BlockHeader -> Maybe Timestamp)
                      -> CAccModifier
        rollbackBlock credentials allAddresses (b, u) blkHeaderTs =
            trackingRollbackTxs credentials allAddresses (Just . heightOf) blkHeaderTs $
            zip3 (gbTxs b) (undoTx u) (repeat $ getBlockHeader b)

        applyBlock :: WalletDecrCredentials
                   -> [(Address, HeaderHash)]
                   -> Blund
                   -> (BlockHeader -> Maybe Timestamp)
                   -> CAccModifier
        applyBlock credentials allAddresses (b, u) blkHeaderTs =
            trackingApplyTxs credentials allAddresses (Just . heightOf) blkHeaderTs ptxBlkInfo $
            zip3 (gbTxs b) (undoTx u) (repeat $ getBlockHeader b)


-- | Gets the 'ChainDifficulty' for either a 'GenesisBlock' or a 'MainBlock'.
-- See: https://bitcoin.org/en/glossary/block-height
heightOf :: HasDifficulty a => a -> ChainDifficulty
heightOf = view difficultyL

-- | Retrieves the 'BlockHeader' correspending to the first 'GenesisBlock' of
-- this blockchain.
firstGenesisHeader :: (HasGenesisHash, MonadDBRead m) => m (Either SyncError BlockHeader)
firstGenesisHeader = runExceptT $ do
    genesisHeaderHash  <- resolveForwardLink (genesisHash @BlockHeader)
    case genesisHeaderHash of
        Nothing  -> throwError GenesisHeaderHashNotFound
        Just ghh -> do
            genesisBlockHeader <- DB.getHeader ghh
            maybe (throwError GenesisBlockHeaderNotFound) pure genesisBlockHeader

constructAllUsed
    :: [(Address, HeaderHash)]
    -> VoidModifier (Address, HeaderHash)
    -> HashSet Address
constructAllUsed usedAddresses modif =
    HS.map fst $
    getKeys $
    MM.modifyHashMap modif $
    HM.fromList $
    zip usedAddresses (repeat ()) -- not so good performance :(

-- Process transactions on block application,
-- decrypt our addresses, and add/delete them to/from wallet-db.
-- Addresses are used in TxIn's will be deleted,
-- in TxOut's will be added.
trackingApplyTxs
    :: HasConfiguration
    => WalletDecrCredentials                 -- ^ Wallet's decryption credentials
    -> [(Address, HeaderHash)]               -- ^ All used addresses from db along with their HeaderHashes
    -> (BlockHeader -> Maybe ChainDifficulty) -- ^ Function to determine tx chain difficulty
    -> (BlockHeader -> Maybe Timestamp)       -- ^ Function to determine tx timestamp in history
    -> (BlockHeader -> Maybe PtxBlockInfo)    -- ^ Function to determine pending tx's block info
    -> [(TxAux, TxUndo, BlockHeader)]         -- ^ Txs of blocks and corresponding header hash
    -> CAccModifier
trackingApplyTxs credentials usedAddresses getDiff getTs getPtxBlkInfo txs =
    foldl' applyTx mempty txs
  where
    applyTx :: CAccModifier -> (TxAux, TxUndo, BlockHeader) -> CAccModifier
    applyTx CAccModifier{..} (tx, undo, blkHeader) = do
        let hh = headerHash blkHeader
            hhs = repeat hh
            wh@(WithHash _ txId) = withHash (taTx tx)
        let thee@THEntryExtra{..} =
                buildTHEntryExtra credentials (wh, undo) (getDiff blkHeader, getTs blkHeader)

            ownTxIns = map (fst . fst) theeInputs
            ownTxOuts = map fst theeOutputs

            addedHistory = maybe camAddedHistory (flip DL.cons camAddedHistory) (isTxEntryInteresting thee)

            usedAddrs = map (WS._wamAddress . snd) theeOutputs
            changeAddrs = evalChange
                              (constructAllUsed usedAddresses camUsed)
                              (map snd theeInputs)
                              (map snd theeOutputs)
                              (length theeOutputs == NE.length (_txOutputs $ taTx tx))

            mPtxBlkInfo = getPtxBlkInfo blkHeader
            addedPtxCandidates =
                if | Just ptxBlkInfo <- mPtxBlkInfo
                     -> DL.cons (txId, ptxBlkInfo) camAddedPtxCandidates
                   | otherwise
                     -> camAddedPtxCandidates
        CAccModifier
            (deleteAndInsertIMM [] (map snd theeOutputs) camAddresses)
            (deleteAndInsertVM [] (zip usedAddrs hhs) camUsed)
            (deleteAndInsertVM [] (zip changeAddrs hhs) camChange)
            (deleteAndInsertMM ownTxIns ownTxOuts camUtxo)
            addedHistory
            camDeletedHistory
            addedPtxCandidates
            camDeletedPtxCandidates

-- Process transactions on block rollback.
-- Like @trackingApplyTxs@, but vise versa.
trackingRollbackTxs
    :: HasConfiguration
    => WalletDecrCredentials                  -- ^ Wallet's decryption credentials
    -> [(Address, HeaderHash)]                -- ^ All used addresses from db along with their HeaderHashes
    -> (BlockHeader -> Maybe ChainDifficulty) -- ^ Function to determine tx chain difficulty
    -> (BlockHeader -> Maybe Timestamp)       -- ^ Function to determine tx timestamp in history
    -> [(TxAux, TxUndo, BlockHeader)]         -- ^ Txs of blocks and corresponding header hash
    -> CAccModifier
trackingRollbackTxs credentials usedAddresses getDiff getTs txs =
    foldl' rollbackTx mempty txs
  where
    rollbackTx :: CAccModifier -> (TxAux, TxUndo, BlockHeader) -> CAccModifier
    rollbackTx CAccModifier{..} (tx, undo, blkHeader) = do
        let wh@(WithHash _ txId) = withHash (taTx tx)
            hh = headerHash blkHeader
            hhs = repeat hh
            thee@THEntryExtra{..} =
                buildTHEntryExtra credentials (wh, undo) (getDiff blkHeader, getTs blkHeader)

            ownTxOutIns = map (fst . fst) theeOutputs
            deletedHistory = maybe camDeletedHistory (DL.snoc camDeletedHistory) (isTxEntryInteresting thee)
            deletedPtxCandidates = DL.cons (txId, theeTxEntry) camDeletedPtxCandidates

        -- Rollback isn't needed, because we don't use @utxoGet@
        -- (undo contains all required information)
        let usedAddrs = map (WS._wamAddress . snd) theeOutputs
            changeAddrs =
                evalChange
                    (constructAllUsed usedAddresses camUsed)
                    (map snd theeInputs)
                    (map snd theeOutputs)
                    (length theeOutputs == NE.length (_txOutputs $ taTx tx))
        CAccModifier
            (deleteAndInsertIMM (map snd theeOutputs) [] camAddresses)
            (deleteAndInsertVM (zip usedAddrs hhs) [] camUsed)
            (deleteAndInsertVM (zip changeAddrs hhs) [] camChange)
            (deleteAndInsertMM ownTxOutIns (map fst theeInputs) camUtxo)
            camAddedHistory
            deletedHistory
            camAddedPtxCandidates
            deletedPtxCandidates

-- | A 'BoundedSyncTime' (by lack of better names) stores time stats of a
-- smaller (bounded, finite) portion of the syncing process. These information
-- can be used to compute the 'SyncThroughput'.
data BoundedSyncTime = BoundedSyncTime {
    bscProcessedBlocks :: !BlockCount
    -- ^ How many blocks we did process?
  , bscProcessingTime  :: !Microsecond
    -- ^ How many microseconds it took to do so?
  }

-- | Calculates the 'SyncThroughput' in terms of blocks/sec.
-- processedBlockCount : processingTime = X : 1000000
-- X = (processedBlockCount * 1000000) / processingTime
calculateThroughput :: BoundedSyncTime -> WS.SyncThroughput
calculateThroughput BoundedSyncTime{..} =
    let processingTime = max 1.0 (realToFrac @Integer @Double $ toMicroseconds bscProcessingTime)
        (microseconds :: Integer) = 1000000
        (processedBlocks :: Integer) = fromIntegral . getBlockCount $ bscProcessedBlocks
        (tput :: Double) = realToFrac (processedBlocks * microseconds) / realToFrac processingTime
     in WS.SyncThroughput . BlockCount . round $ tput

-- | Calculates the estimated remaining time to restore the wallet via the
-- provided 'SyncThroughput' and the @remaining@ height of the blockchain.
-- This is given by the simple proportion:
-- syncedBlocks : 1_second = remainingHeight : X
-- X = (1.0 * remainingHeight) / syncedBlocks
calculateEstimatedRemainingTime :: WS.SyncThroughput -> ChainDifficulty -> Timestamp
calculateEstimatedRemainingTime (WS.SyncThroughput blocks) remainingBlocks =
    let toDouble                = realToFrac @Integer @Double . fromIntegral . getBlockCount
        (actualDepth :: Double) = toDouble (getChainDifficulty remainingBlocks)
        (throughput :: Double)  = max 1.0 (toDouble blocks) -- Avoids division by 0.
        (microseconds :: Integer) = 1000000
        eta = actualDepth / throughput
     in Timestamp . fromMicroseconds . (* microseconds) . round $ eta

-- | Apply the given 'CAccModifier' to a wallet.
applyModifierToWallet
    :: ( CanLog m
       , HasLoggerName m
       , MonadIO m
       , HasConfiguration
       )
    => WalletDB
    -> TrackingOperation
    -> CId Wal
    -> BlockHeader
    -> CAccModifier
    -> m ()
applyModifierToWallet db trackingOperation wid newBlockHeaderTip CAccModifier{..} = do

    let newTip = headerHash newBlockHeaderTip

    let newSyncState = case trackingOperation of
            SyncWallet        -> SyncedWith newTip
            RestoreWallet rbd -> RestoringFrom rbd newTip
    logDebug $ sformat ("applyModifierToWallet: new SyncState = " % shown) trackingOperation

    let cMetas = mapMaybe (\THEntry {..} -> (\mts -> (encodeCType _thTxId
                                                     , CTxMeta . timestampToPosix $ mts)
                                            ) <$> _thTimestamp)
               $ DL.toList camAddedHistory

    WS.applyModifierToWallet
      db
      wid
      (sortedInsertions camAddresses)
      [ (UsedAddr, fst <$> MM.insertions camUsed)
      , (ChangeAddr, fst <$> MM.insertions camChange)
      ]
      camUtxo
      cMetas
      (txHistoryListToMap $ DL.toList camAddedHistory)
      (DL.toList $ second PtxInNewestBlocks <$> camAddedPtxCandidates)
      (heightOf newBlockHeaderTip)
      newSyncState

rollbackModifierFromWallet
    :: ( CanLog m
       , HasLoggerName m
       , MonadSlots ctx m
       , HasProtocolConstants
       , HasConfiguration
       )
    => WalletDB
    -> TrackingOperation
    -> CId Wal
    -> HeaderHash
    -> CAccModifier
    -> m ()
rollbackModifierFromWallet db trackingOperation wid newTip CAccModifier{..} = do

    let newSyncState = case trackingOperation of
            SyncWallet        -> SyncedWith newTip
            RestoreWallet rbd -> RestoringFrom rbd newTip
    logDebug $ sformat ("rollbackModifierFromWallet: new SyncState = " % shown) trackingOperation

    curSlot <- getCurrentSlotInaccurate

    WS.rollbackModifierFromWallet
      db
      wid
      (indexedDeletions camAddresses)
      [ (UsedAddr, MM.deletions camUsed)
      , (ChangeAddr, MM.deletions camChange)
      ]
      camUtxo
      (txHistoryListToMap (DL.toList camDeletedHistory))
      ((\(txId, poolInfo) -> ( txId, PtxApplying poolInfo
                             , WS.PtxResetSubmitTiming curSlot))
        <$> DL.toList camDeletedPtxCandidates
      )
      newSyncState

-- Change address is an address which money remainder is sent to.
-- We will consider output address as "change" if:
-- 1. it belongs to source account (taken from one of source addresses)
-- 2. it's not mentioned in the blockchain (aka isn't "used" address)
-- 3. there is at least one non "change" address among all outputs ones

-- The first point is very intuitive and needed for case when we
-- send tx to somebody, i.e. to not our address.
-- The second point is needed for case when
-- we send a tx from our account to the same account.
-- The third point is needed for case when we just created address
-- in an account and then send a tx from the address belonging to this account
-- to the created.
-- In this case both output addresses will be treated as "change"
-- by the first two rules.
-- But the third rule will make them not "change".
-- This decision is controversial, but we can't understand from the blockchain
-- which of them is really "change".
-- There is an option to treat both of them as "change", but it seems to be more puzzling.
evalChange
    :: HashSet Address
    -> [WAddressMeta] -- ^ Own input addresses of tx
    -> [WAddressMeta] -- ^ Own outputs addresses of tx
    -> Bool            -- ^ Whether all tx's outputs are our own
    -> [Address]
evalChange allUsed inputs outputs allOutputsOur
    | [] <- inputs = [] -- It means this transaction isn't our outgoing transaction.
    | inp : _ <- inputs =
        let srcAccount = inp ^. WS.wamAccount in
        -- Apply the first point.
        let addrFromSrcAccount = HS.fromList $ map WS._wamAddress
              $ filter ((== srcAccount) . view WS.wamAccount) outputs in
        -- Apply the second point.
        let potentialChange = addrFromSrcAccount `HS.difference` allUsed in
        -- Apply the third point.
        if allOutputsOur && potentialChange == HS.fromList (map WS._wamAddress outputs) then []
        else HS.toList potentialChange

setLogger :: HasLoggerName m => m a -> m a
setLogger = modifyLoggerName (const "syncWalletWorker")
