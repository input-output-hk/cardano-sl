{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | A module which contains logic for collecting and applying changes
-- introduced by transaction application to wallet DB, as well as logic
-- for syncronization of wallet DB with core DB.
--
-- In order to maintain actual information in wallet DB we watch for
-- block applications and rollbacks, deriving subsequent changes to
-- wallet DB. Such changes include updates to transaction history cache,
-- 'Utxo' cache, sets of known addresses which belong to our wallet, etc.
--
-- Each module stores 'WalletTip' datatype, which contains header has of
-- last block until which a wallet was synchronized with blockchain (or no such
-- hash, if a wallet was never synchronized with blockchain).
--
-- In order to detect transactions which relate to our wallet we need a mechanism
-- of detecting addresses which belong to our wallet. Transactions which include
-- at least one of wallet's addresses as in their inputs or outputs relate to our
-- wallet and should be added to transaction history and applied to 'Utxo' cache.
-- As long as we use random generation of addresses, we have no way to predict which
-- of wallet's addresses should appear in blockchain next, so we use encrypted
-- /address attributes/ for own addresses detection. See module
-- "Pos.Wallet.Web.Tracking.Decrypt" for more info.
--
-- Normally wallet DB is keeped in sync with core DB using @BListener@s -- special
-- methods which get called every time a new block gets applied or rolled back.
-- See module "Pos.Wallet.Web.Tracking.BListener" for more info.
--
-- However, there are cases when we should trigger wallet sync process by hand:
--
--   * When user imports a wallet or restores it from a mnemonic phrase. In this case
--     we should traverse the whole blockchain to reestablish full wallet transaction history.
--     Note that we don't perform syncing for newly generated wallets (because they can't
--     have any transaction history).
--   * When we detect that wallet's current sync tip has fallen behind current blockchain tip.
--     This may happen e. g. if previous sync process has been interrupted. We compare wallet tips
--     with current blockchain tip every time wallet restarts.
--
module Pos.Wallet.Web.Tracking.Sync
       ( syncWalletsWithGState
       , trackingApplyTxs
       , trackingRollbackTxs
       , applyModifierToWallet
       , rollbackModifierFromWallet
       , BlockLockMode
       , WalletTrackingEnv

       , syncWalletOnImport
       , txMempoolToModifier

       , fixingCachedAccModifier
       , fixCachedAccModifierFor

       , buildTHEntryExtra
       , isTxEntryInteresting

       -- For tests
       , evalChange
       ) where

import           Universum
import           Unsafe (unsafeLast)

import           Control.Lens (to)
import           Control.Monad.Catch (handleAll)
import qualified Data.DList as DL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Ether.Internal (HasLens (..))
import           Formatting (build, sformat, (%))
import           System.Wlog (HasLoggerName, WithLogger, logError, logInfo, logWarning,
                              modifyLoggerName)

import           Pos.Block.Types (Blund, undoTx)
import           Pos.Client.Txp.History (TxHistoryEntry (..), txHistoryListToMap)
import           Pos.Core (BlockHeaderStub, ChainDifficulty, HasConfiguration, HasDifficulty (..),
                           HeaderHash, Timestamp, blkSecurityParam, genesisHash, headerHash,
                           headerSlotL, timestampToPosix)
import           Pos.Core.Block (BlockHeader, getBlockHeader, mainBlockTxPayload)
import           Pos.Core.Txp (TxAux (..), TxOutAux (..), TxUndo, toaOut, txOutAddress)
import           Pos.Crypto (EncryptedSecretKey, WithHash (..), shortHashF, withHash)
import           Pos.DB.Block (getBlund)
import qualified Pos.DB.Block.Load as GS
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead (..))
import qualified Pos.GState as GS
import           Pos.GState.BlockExtra (foldlUpWhileM, resolveForwardLink)
import           Pos.Slotting (MonadSlots (..), MonadSlotsData, getSlotStartPure, getSystemStartM)
import           Pos.StateLock (Priority (..), StateLock, withStateLockNoMetrics)
import           Pos.Txp (MonadTxpMem, flattenTxPayload, genesisUtxo, getLocalTxsNUndo, topsortTxs,
                          unGenesisUtxo, utxoToModifier, _txOutputs)
import           Pos.Util.Chrono (getNewestFirst)
import           Pos.Util.LogSafe (logInfoS, logWarningS)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Servant (encodeCType)
import           Pos.Util.Util (getKeys)

import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Account (MonadKeySearch (..))
import           Pos.Wallet.Web.ClientTypes (Addr, CId, CTxMeta (..), CWAddressMeta (..), Wal,
                                             addrMetaToAccount, encToCId)
import           Pos.Wallet.Web.Error.Types (WalletError (..))
import           Pos.Wallet.Web.Pending.Types (PtxBlockInfo,
                                               PtxCondition (PtxApplying, PtxInNewestBlocks))
import           Pos.Wallet.Web.State (CustomAddressType (..), MonadWalletDB, WalletTip (..))
import qualified Pos.Wallet.Web.State as WS
import           Pos.Wallet.Web.Tracking.Decrypt (THEntryExtra (..), buildTHEntryExtra,
                                                  eskToWalletDecrCredentials, isTxEntryInteresting,
                                                  selectOwnAddresses)
import           Pos.Wallet.Web.Tracking.Modifier (CAccModifier (..), CachedCAccModifier,
                                                   VoidModifier, deleteAndInsertIMM,
                                                   deleteAndInsertMM, deleteAndInsertVM,
                                                   indexedDeletions, sortedInsertions)

-- | Type constraint which allows to take a block semaphore.
type BlockLockMode ctx m =
     ( WithLogger m
     , MonadDBRead m
     , MonadReader ctx m
     , HasLens StateLock ctx StateLock
     , MonadMask m
     )

-- | Type constraint which provides read access to mempool and DB,
-- allowing to use 'txMempoolToModifier'.
type WalletTrackingEnvRead ctx m =
     ( BlockLockMode ctx m
     , MonadTxpMem WalletMempoolExt ctx m
     , WS.MonadWalletDBRead ctx m
     , MonadSlotsData ctx m
     , WithLogger m
     , HasConfiguration
     )

-- | Type constraint which allows performing wallet sync.
type WalletTrackingEnv ctx m =
     ( WalletTrackingEnvRead ctx m
     , MonadWalletDB ctx m
     )

-- | Helper function for syncing only one wallet.
syncWalletOnImport :: WalletTrackingEnv ctx m => EncryptedSecretKey -> m ()
syncWalletOnImport = syncWalletsWithGState . one

-- | Given a root secret key of wallet, obtain 'CAccModifier' for this wallet
-- which reflect changes caused by transactions currently stored in mempool.
txMempoolToModifier :: WalletTrackingEnvRead ctx m => EncryptedSecretKey -> m CAccModifier
txMempoolToModifier encSK = do
    let wHash (i, TxAux {..}, _) = WithHash taTx i
        getDiff       = const Nothing  -- no difficulty (mempool txs)
        getTs         = const Nothing  -- don't give any timestamp
        getPtxBlkInfo = const Nothing  -- no slot of containing block
    (txs, undoMap) <- getLocalTxsNUndo

    txsWUndo <- forM txs $ \(id, tx) -> case HM.lookup id undoMap of
        Just undo -> pure (id, tx, undo)
        Nothing -> do
            let errMsg = sformat ("There is no undo corresponding to TxId #"%build%" from txp mempool") id
            logError errMsg
            throwM $ InternalError errMsg

    case topsortTxs wHash txsWUndo of
        Nothing      -> mempty <$ logWarning "txMempoolToModifier: couldn't topsort mempool txs"
        Just ordered -> do
            tipH <- DB.getTipHeader
            dbUsed <- WS.getCustomAddresses WS.UsedAddr
            pure $
                trackingApplyTxs encSK dbUsed getDiff getTs getPtxBlkInfo $
                map (\(_, tx, undo) -> (tx, undo, tipH)) ordered

----------------------------------------------------------------------------
-- Logic
----------------------------------------------------------------------------

-- | Check whether or not provided wallets are fully synchronized with current
-- blockchain, and if not, initialize sync process.
syncWalletsWithGState
    :: forall ctx m.
    ( MonadWalletDB ctx m
    , BlockLockMode ctx m
    , MonadSlotsData ctx m
    , HasConfiguration
    )
    => [EncryptedSecretKey] -> m ()
syncWalletsWithGState encSKs = forM_ encSKs $ \encSK -> handleAll (onErr encSK) $ do
    let wAddr = encToCId encSK
    WS.getWalletSyncTip wAddr >>= \case
        Nothing                -> logWarningS $ sformat ("There is no syncTip corresponding to wallet #"%build) wAddr
        Just NotSynced         -> syncDo encSK Nothing
        Just (SyncedWith wTip) -> DB.getHeader wTip >>= \case
            Nothing ->
                throwM $ InternalError $
                    sformat ("Couldn't get block header of wallet by last synced hh: "%build) wTip
            Just wHeader -> syncDo encSK (Just wHeader)
  where
    onErr encSK = logWarningS . sformat fmt (encToCId encSK)
    fmt = "Sync of wallet "%build%" failed: "%build
    syncDo :: EncryptedSecretKey -> Maybe BlockHeader -> m ()
    syncDo encSK wTipH = do
        let wdiff = maybe (0::Word32) (fromIntegral . ( ^. difficultyL)) wTipH
        gstateTipH <- DB.getTipHeader

        -- Here we check how far away wallet tip is from current
        -- blockchain tip.
        wNewTip <-
            if (gstateTipH ^. difficultyL > fromIntegral blkSecurityParam + fromIntegral wdiff) then do
                -- If wallet tip is deeper than 'blkSecurityParam' blocks away from blockchain tip,
                -- then we can sync wallet until block with depth 'blkSecurityParam' without
                -- taking block semaphore. It's safe because blocks deeper than 'blkSecurityParam'
                -- cannot be rolled back.
                -- We don't want to take block semaphore for syncing
                -- blocks deeper than 'blkSecurityParam', because blockchain can be large,
                -- and syncing may take a lot of time, and holding block semaphore for
                -- extended period of time means no regular block processing
                -- can proceed for extended period of time, which leads to poor performance
                -- and various bugs.
                bh <- unsafeLast . getNewestFirst <$> GS.loadHeadersByDepth (blkSecurityParam + 1) (headerHash gstateTipH)
                logInfo $
                    sformat ("Wallet's tip is far from GState tip. Syncing with "%build%" without the block lock")
                    (headerHash bh)
                syncWalletWithGStateUnsafe encSK wTipH bh
                pure $ Just bh
            else pure wTipH

        -- All blocks not deeper than 'blkSecurityParam' must be processed under
        -- block semaphore to avoid race conditions with possible rollbacks.
        withStateLockNoMetrics HighPriority $ \tip -> do
            logInfo $ sformat ("Syncing wallet with "%build%" under the block lock") tip
            tipH <- maybe (error "No block header corresponding to tip") pure =<< DB.getHeader tip
            syncWalletWithGStateUnsafe encSK wNewTip tipH

----------------------------------------------------------------------------
-- Unsafe operations. Core logic.
----------------------------------------------------------------------------
-- These operation aren't atomic and don't take the block lock.


-- | Synchronize given wallet with core database.
-- BE CAREFUL! This function iterates over blockchain, the blockchain can be large.
syncWalletWithGStateUnsafe
    :: forall ctx m .
    ( MonadWalletDB ctx m
    , MonadDBRead m
    , WithLogger m
    , MonadSlotsData ctx m
    , HasConfiguration
    )
    => EncryptedSecretKey      -- ^ Secret key for decoding our addresses
    -> Maybe BlockHeader       -- ^ Block header corresponding to wallet's tip.
                               --   Nothing when wallet's tip is genesisHash
    -> BlockHeader             -- ^ GState header hash
    -> m ()
syncWalletWithGStateUnsafe encSK wTipHeader gstateH = setLogger $ do
    systemStart  <- getSystemStartM
    slottingData <- GS.getSlottingData

    let gstateHHash = headerHash gstateH
        -- Passed to 'foldlUpWhileM', means "fetch blocks until their
        -- difficulty is less than or equal to difficulty of @gstateH@"
        loadCond (b, _) _ = b ^. difficultyL <= gstateH ^. difficultyL
        wAddr = encToCId encSK
        mappendR r mm = pure (r <> mm)
        -- Shorthands for getting block difficulty.
        diff = (^. difficultyL)
        mDiff = Just . diff
        -- Shorthand for getting tx payload of block.
        gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)

        -- Function which, given a block header, returns a timestamp of
        -- this block's slot start.
        mainBlkHeaderTs mBlkH =
          getSlotStartPure systemStart (mBlkH ^. headerSlotL) slottingData
        blkHeaderTs = either (const Nothing) mainBlkHeaderTs

        -- Here we assume that transactions are not created until syncing is complete.
        ptxBlkInfo = const Nothing

        -- Wrapper around 'trackingRollbackTxs', which accepts 'Blund'
        -- and unpacks its transaction payload.
        rollbackBlock :: [(CId Addr, HeaderHash)] -> Blund -> CAccModifier
        rollbackBlock dbUsed (b, u) =
            trackingRollbackTxs encSK dbUsed mDiff blkHeaderTs $
            zip3 (gbTxs b) (undoTx u) (repeat $ getBlockHeader b)

        -- Wrapper around 'trackingApplyTxs', which accepts 'Blund' and
        -- unpacks its transaction payload.
        applyBlock :: [(CId Addr, HeaderHash)] -> Blund -> m CAccModifier
        applyBlock dbUsed (b, u) = pure $
            trackingApplyTxs encSK dbUsed mDiff blkHeaderTs ptxBlkInfo $
            zip3 (gbTxs b) (undoTx u) (repeat $ getBlockHeader b)

        -- Fetch all blocks since 'wHeader' and until 'gstateH',
        -- process their transactions and compute corresponding
        -- 'CAccModifier'.
        -- TODO: CSL-2039 It seems to be not working in case if some blocks has been
        -- rolled back and some other blocks has been already applied:
        --
        --   V current blockchain tip
        --   o
        --   |   V wallet sync tip
        --   o   o
        --   |   |
        --   o   o
        --    \ /
        --     o
        --     |
        --    ...
        --
        -- In case such as on illustration an attempt to apply blocks
        -- between wallet sync tip and blockchain tip will be made, which will
        -- fail. In this case a rollback processing followed by block application
        -- processing should be performed.
        computeAccModifier :: BlockHeader -> m CAccModifier
        computeAccModifier wHeader = do
            dbUsed <- WS.getCustomAddresses WS.UsedAddr
            logInfoS $
                sformat ("Wallet "%build%" header: "%build%", current tip header: "%build)
                wAddr wHeader gstateH
            if | diff gstateH > diff wHeader -> do
                     -- Wallet sync tip is deeper than current blockchain tip:
                     -- we need to apply transactions in blocks from @wHeader@ to
                     -- @gstateH@. We start from block right next to @wHeader@
                     -- call @applyBlock@ for every block and consequentially
                     -- combine resulting 'CAccModifier's from bottom to top.
                     maybe (pure mempty)
                         (\wNextH ->
                            foldlUpWhileM getBlund (applyBlock dbUsed) wNextH loadCond mappendR mempty)
                         =<< resolveForwardLink wHeader
               | diff gstateH < diff wHeader -> do
                     -- Wallet sync tip is higher than current blockchain tip:
                     -- apparently, a rollback has occured which hasn't been processed
                     -- properly.
                     -- We start from @wHeader@ and rollback transactions in blocks until
                     -- we reach @gstateH@.
                     -- We can afford to load all rolled back blocks from DB together, because
                     -- there's not more than 'blkSecurityParam' blocks which can be rolled back.
                     blunds <- getNewestFirst <$>
                         GS.loadBlundsWhile (\b -> getBlockHeader b /= gstateH) (headerHash wHeader)
                     pure $ foldl' (\r b -> r <> rollbackBlock dbUsed b) mempty blunds
               | otherwise ->
                     -- Turns out that wallet is already synced, do nothing.
                     mempty <$ logInfoS (sformat ("Wallet "%build%" is already synced") wAddr)

    -- If the wallet was never synced with blockchain, we should check if
    -- genesis 'Utxo' contains some outputs belonging to wallet, and if
    -- that's the case, initialize wallet DB with genesis data.
    whenNothing_ wTipHeader $ do
        let wdc = eskToWalletDecrCredentials encSK
            ownGenesisData =
                selectOwnAddresses wdc (txOutAddress . toaOut . snd) $
                M.toList $ unGenesisUtxo genesisUtxo
            ownGenesisUtxo = M.fromList $ map fst ownGenesisData
            ownGenesisAddrs = map snd ownGenesisData
        mapM_ WS.addWAddress ownGenesisAddrs
        WS.updateWalletBalancesAndUtxo (utxoToModifier ownGenesisUtxo)

    startFromH <- maybe firstGenesisHeader pure wTipHeader

    -- Finally compute the resulting modifier and apply it to wallet DB.
    mapModifier@CAccModifier{..} <- computeAccModifier startFromH
    applyModifierToWallet wAddr gstateHHash mapModifier

    -- Wallet sync is done, so we mark the wallet as ready
    -- to make it available from api endpoints.
    WS.setWalletReady wAddr True
    logInfoS $
        sformat ("Wallet "%build%" has been synced with tip "
                %shortHashF%", "%build)
                wAddr (maybe genesisHash headerHash wTipHeader) mapModifier
  where
    firstGenesisHeader :: m BlockHeader
    firstGenesisHeader = resolveForwardLink (genesisHash @BlockHeaderStub) >>=
        maybe (error "Unexpected state: genesisHash doesn't have forward link")
            (maybe (error "No genesis block corresponding to header hash") pure <=< DB.getHeader)

-- | Apply current modifier of set of used addresses to
-- initial list of all used addresses. Returns set of resulting
-- used addresses as 'HashSet'.
constructAllUsed
    :: [(CId Addr, HeaderHash)]
    -> VoidModifier (CId Addr, HeaderHash)
    -> HashSet (CId Addr)
constructAllUsed dbUsed modif =
    HS.map fst $
    getKeys $
    MM.modifyHashMap modif $
    HM.fromList $
    zip dbUsed (repeat ()) -- not so good performance :(

-- | Given list of transactions with corresponding 'TxUndo's and
-- block headers and additional info, derive 'CAccModifier' which
-- accumulates all changes which these transactions cause in wallet DB
-- (such as addition\/deletion of addresses, updates of transaction
-- history and 'Utxo' caches, etc.)
trackingApplyTxs
    :: HasConfiguration
    => EncryptedSecretKey                     -- ^ Wallet's secret key
    -> [(CId Addr, HeaderHash)]               -- ^ All used addresses from db along with their HeaderHashes
    -> (BlockHeader -> Maybe ChainDifficulty) -- ^ Function to determine tx chain difficulty
    -> (BlockHeader -> Maybe Timestamp)       -- ^ Function to determine tx timestamp in history
    -> (BlockHeader -> Maybe PtxBlockInfo)    -- ^ Function to determine pending tx's block info
    -> [(TxAux, TxUndo, BlockHeader)]         -- ^ Txs of blocks and corresponding header hash
    -> CAccModifier
trackingApplyTxs (eskToWalletDecrCredentials -> wdc) dbUsed getDiff getTs getPtxBlkInfo txs =
    foldl' applyTx mempty txs
  where
    applyTx :: CAccModifier -> (TxAux, TxUndo, BlockHeader) -> CAccModifier
    applyTx CAccModifier{..} (tx, undo, blkHeader) = do
        let hh = headerHash blkHeader
            hhs = repeat hh
            wh@(WithHash _ txId) = withHash (taTx tx)
        let thee@THEntryExtra{..} =
                -- Performs detection of wallet's addresses and among transaction's
                -- inputs and outputs. See "Pos.Wallet.Web.Tracking.Decrypt".
                buildTHEntryExtra wdc (wh, undo) (getDiff blkHeader, getTs blkHeader)

            ownTxIns = map (fst . fst) theeInputs
            ownTxOuts = map fst theeOutputs

            addedHistory = maybe camAddedHistory (flip DL.cons camAddedHistory) (isTxEntryInteresting thee)

            usedAddrs = map (cwamId . snd) theeOutputs
            changeAddrs = evalChange
                              (constructAllUsed dbUsed camUsed)
                              (map snd theeInputs)
                              (map snd theeOutputs)
                              (length theeOutputs == NE.length (_txOutputs $ taTx tx))

            mPtxBlkInfo = getPtxBlkInfo blkHeader
            addedPtxCandidates =
                -- Here we only need to add wallet's /own outgoing transactions/
                -- in 'camAddedPtxCandidates', because it's only these transactions
                -- which may be created inside user's wallet.
                -- Adding all transactions from block to 'camAddedPtxCandidates' doesn't
                -- affect correctness of code, but may cause performance problems if
                -- there's a lot of transactions in blocks.
                if | Just ptxBlkInfo <- mPtxBlkInfo
                     -> if not (null theeInputs)
                        then DL.cons (txId, ptxBlkInfo) camAddedPtxCandidates
                        else camAddedPtxCandidates
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

-- | Given list of transactions with corresponding 'TxUndo's and
-- block headers and additional info, derive 'CAccModifier' which
-- accumulates all changes caused by /rollback/ of these transactions.
-- Like 'trackingApplyTxs', but vise versa.
trackingRollbackTxs
    :: HasConfiguration
    => EncryptedSecretKey                      -- ^ Wallet's secret key
    -> [(CId Addr, HeaderHash)]                -- ^ All used addresses from DB along with their 'HeaderHash'es
    -> (BlockHeader -> Maybe ChainDifficulty)  -- ^ Function to determine tx chain difficulty
    -> (BlockHeader -> Maybe Timestamp)        -- ^ Function to determine tx timestamp in history
    -> [(TxAux, TxUndo, BlockHeader)]          -- ^ Txs of blocks and corresponding header hash
    -> CAccModifier
trackingRollbackTxs (eskToWalletDecrCredentials -> wdc) dbUsed getDiff getTs txs =
    foldl' rollbackTx mempty txs
  where
    rollbackTx :: CAccModifier -> (TxAux, TxUndo, BlockHeader) -> CAccModifier
    rollbackTx CAccModifier{..} (tx, undo, blkHeader) = do
        let wh@(WithHash _ txId) = withHash (taTx tx)
            hh = headerHash blkHeader
            hhs = repeat hh
            thee@THEntryExtra{..} =
                -- Performs detection of wallet's addresses and among transaction's
                -- inputs and outputs. See "Pos.Wallet.Web.Tracking.Decrypt".
                buildTHEntryExtra wdc (wh, undo) (getDiff blkHeader, getTs blkHeader)

            ownTxOutIns = map (fst . fst) theeOutputs
            deletedHistory = maybe camDeletedHistory (DL.snoc camDeletedHistory) (isTxEntryInteresting thee)
            deletedPtxCandidates =
                -- See definition of @addedPtxCandidates@ in 'trackingApplyTxs'
                -- for explanation.
                if not (null theeInputs)
                then DL.cons (txId, theeTxEntry) camDeletedPtxCandidates
                else camDeletedPtxCandidates

        -- Rollback isn't needed, because we don't use @utxoGet@
        -- (undo contains all required information)
        let usedAddrs = map (cwamId . snd) theeOutputs
            changeAddrs =
                evalChange
                    (constructAllUsed dbUsed camUsed)
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

-- | After processing block application, apply changes contained
-- in resulting 'CAccModifier' to wallet DB.
-- TODO: Probably should be merged with 'rollbackModifierFromWallet'
-- and be performed as single acidic action.
applyModifierToWallet
    :: MonadWalletDB ctx m
    => CId Wal      -- ^ Wallet ID
    -> HeaderHash   -- ^ Header hash of block until which wallet has been synced (new wallet sync tip)
    -> CAccModifier -- ^ Modifier
    -> m ()
applyModifierToWallet wid newTip CAccModifier{..} = do
    mapM_ WS.addWAddress (sortedInsertions camAddresses)
    mapM_ (WS.addCustomAddress UsedAddr . fst) (MM.insertions camUsed)
    mapM_ (WS.addCustomAddress ChangeAddr . fst) (MM.insertions camChange)
    WS.updateWalletBalancesAndUtxo camUtxo
    let cMetas = M.fromList
               $ mapMaybe (\THEntry {..} -> (\mts -> (_thTxId, CTxMeta . timestampToPosix $ mts)) <$> _thTimestamp)
               $ DL.toList camAddedHistory
    WS.addOnlyNewTxMetas wid cMetas
    let addedHistory = txHistoryListToMap $ DL.toList camAddedHistory
    WS.insertIntoHistoryCache wid addedHistory
    -- Resubmitting worker can change Ptx in db nonatomically, but
    -- tracker has priority over the resubmiter, thus do not use CAS here
    forM_ camAddedPtxCandidates $ \(txid, ptxBlkInfo) ->
        WS.setPtxCondition wid txid (PtxInNewestBlocks ptxBlkInfo)
    WS.setWalletSyncTip wid newTip

-- | After processing rollback, apply changes contained in
-- resulting 'CAccModifier' to wallet DB.
-- TODO: Probably should be merged with 'applyModifierToWallet'
-- and be performed as single acidic action.
rollbackModifierFromWallet
    :: (MonadWalletDB ctx m, MonadSlots ctx m)
    => CId Wal      -- ^ Wallet ID
    -> HeaderHash   -- ^ Header hash of block until which wallet has been rolled back (new wallet sync tip)
    -> CAccModifier -- ^ Modifier
    -> m ()
rollbackModifierFromWallet wid newTip CAccModifier{..} = do
    -- TODO maybe do it as one acid-state transaction.
    mapM_ WS.removeWAddress (indexedDeletions camAddresses)
    mapM_ (WS.removeCustomAddress UsedAddr) (MM.deletions camUsed)
    mapM_ (WS.removeCustomAddress ChangeAddr) (MM.deletions camChange)
    WS.updateWalletBalancesAndUtxo camUtxo
    forM_ camDeletedPtxCandidates $ \(txid, poolInfo) -> do
        curSlot <- getCurrentSlotInaccurate
        WS.ptxUpdateMeta wid txid (WS.PtxResetSubmitTiming curSlot)
        WS.setPtxCondition wid txid (PtxApplying poolInfo)
    let deletedHistory = txHistoryListToMap (DL.toList camDeletedHistory)
    WS.removeFromHistoryCache wid deletedHistory
    WS.removeWalletTxMetas wid (map encodeCType $ M.keys deletedHistory)
    WS.setWalletSyncTip wid newTip


-- | Given subsets of inputs and outputs of transaction which
-- belong to a wallet, determine set of /change addresses/.
--
-- /Change address/ is an address to which money remainder is sent.
-- Output address is a /change address/ if:
-- 1. it belongs to source account (taken from one of source addresses)
-- 2. it's not mentioned in the blockchain (aka isn't "used" address)
-- 3. there is at least one non "change" address among all outputs ones
--
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
    :: HashSet (CId Addr)
    -> [CWAddressMeta] -- ^ Own input addresses of tx
    -> [CWAddressMeta] -- ^ Own outputs addresses of tx
    -> Bool            -- ^ Whether all tx's outputs are our own
    -> [CId Addr]
evalChange allUsed inputs outputs allOutputsOur
    | [] <- inputs = [] -- It means this transaction isn't our outgoing transaction.
    | inp : _ <- inputs =
        let srcAccount = addrMetaToAccount inp in
        -- Apply the first point.
        let addrFromSrcAccount = HS.fromList $ map cwamId $ filter ((== srcAccount) . addrMetaToAccount) outputs in
        -- Apply the second point.
        let potentialChange = addrFromSrcAccount `HS.difference` allUsed in
        -- Apply the third point.
        if allOutputsOur && potentialChange == HS.fromList (map cwamId outputs) then []
        else HS.toList potentialChange

setLogger :: HasLoggerName m => m a -> m a
setLogger = modifyLoggerName (<> "wallet" <> "sync")

----------------------------------------------------------------------------
-- Cached modifier
----------------------------------------------------------------------------

-- | Evaluates `txMempoolToModifier` and provides result as a parameter
-- to given function.
-- Used in order to optimize mempool accesses. All functions which
-- need data about changes in mempool related to particular wallet
-- accept this data as 'CachedCAccModifier'. If 'CachedCAccModifier'
-- is already available (e. g. is passed to higher-level function
-- as an argument), then it's passed directly. Otherwise we need
-- to derive it from mempool using 'txMempoolToModifier'.
-- 'fixingCachedAccModifier' is a helper for doing this.
fixingCachedAccModifier
    :: (WalletTrackingEnvRead ctx m, MonadKeySearch key m)
    => (CachedCAccModifier -> key -> m a)
    -> key -> m a
fixingCachedAccModifier action key =
    findKey key >>= txMempoolToModifier >>= flip action key

-- | A version of 'fixingCachedAccModifier' for
-- actions which don't accept wallet key.
fixCachedAccModifierFor
    :: (WalletTrackingEnvRead ctx m, MonadKeySearch key m)
    => key
    -> (CachedCAccModifier -> m a)
    -> m a
fixCachedAccModifierFor key action =
    fixingCachedAccModifier (const . action) key
