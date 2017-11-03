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
--   then wallet-db can fall behind from node-db (when interruption during rollback)
--   or vice versa (when interruption during application)
--   @syncWSetsWithGStateLock@ implements this functionality.
-- • When a user wants to import a secret key. Then we must rely on
--   Utxo (GStateDB), because blockchain can be large.

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
       ) where

import           Universum
import           Unsafe                           (unsafeLast)

import           Control.Lens                     (to)
import           Control.Monad.Catch              (handleAll)
import qualified Data.DList                       as DL
import qualified Data.HashMap.Strict              as HM
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as M
import           Ether.Internal                   (HasLens (..))
import           Formatting                       (build, sformat, (%))
import           System.Wlog                      (HasLoggerName, WithLogger, logError,
                                                   logInfo, logWarning, modifyLoggerName)

import           Pos.Block.Core                   (BlockHeader, getBlockHeader,
                                                   mainBlockTxPayload)
import           Pos.Block.Types                  (Blund, undoTx)
import           Pos.Client.Txp.History           (TxHistoryEntry (..),
                                                   txHistoryListToMap)
import           Pos.Core                         (BlockHeaderStub, ChainDifficulty,
                                                   HasConfiguration, HasDifficulty (..),
                                                   HeaderHash, Timestamp,
                                                   blkSecurityParam, genesisHash,
                                                   headerHash, headerSlotL,
                                                   timestampToPosix)
import           Pos.Crypto                       (EncryptedSecretKey, WithHash (..),
                                                   shortHashF, withHash)
import qualified Pos.DB.Block                     as DB
import qualified Pos.DB.DB                        as DB
import qualified Pos.GState                       as GS
import           Pos.GState.BlockExtra            (foldlUpWhileM, resolveForwardLink)
import           Pos.Slotting                     (MonadSlots (..), MonadSlotsData,
                                                   getSlotStartPure, getSystemStartM)
import           Pos.StateLock                    (Priority (..), StateLock,
                                                   withStateLockNoMetrics)
import           Pos.Txp                          (GenesisUtxo (..), TxAux (..),
                                                   TxOutAux (..), TxUndo,
                                                   flattenTxPayload, genesisUtxo, toaOut,
                                                   topsortTxs, txOutAddress,
                                                   utxoToModifier)
import           Pos.Txp.MemState.Class           (MonadTxpMem, getLocalTxsNUndo)
import           Pos.Util.Chrono                  (getNewestFirst)
import           Pos.Util.LogSafe                 (logInfoS, logWarningS)
import qualified Pos.Util.Modifier                as MM
import           Pos.Util.Servant                 (encodeCType)

import           Pos.Wallet.WalletMode            (WalletMempoolExt)
import           Pos.Wallet.Web.Account           (MonadKeySearch (..))
import           Pos.Wallet.Web.ClientTypes       (Addr, CId, CTxMeta (..),
                                                   CWAddressMeta (..), Wal, encToCId,
                                                   isTxLocalAddress)
import           Pos.Wallet.Web.Error.Types       (WalletError (..))
import           Pos.Wallet.Web.Pending.Types     (PtxBlockInfo, PtxCondition (PtxApplying, PtxInNewestBlocks))
import           Pos.Wallet.Web.State             (AddressLookupMode (..),
                                                   CustomAddressType (..), MonadWalletDB,
                                                   WalletTip (..))
import qualified Pos.Wallet.Web.State             as WS
import           Pos.Wallet.Web.Tracking.Decrypt  (THEntryExtra (..), buildTHEntryExtra,
                                                   eskToWalletDecrCredentials,
                                                   isTxEntryInteresting,
                                                   selectOwnAddresses)
import           Pos.Wallet.Web.Tracking.Modifier (CAccModifier (..), CachedCAccModifier,
                                                   deleteAndInsertIMM, deleteAndInsertMM,
                                                   deleteAndInsertVM, indexedDeletions,
                                                   sortedInsertions)
import           Pos.Wallet.Web.Util              (getWalletAddrMetas)

type BlockLockMode ctx m =
     ( WithLogger m
     , MonadReader ctx m
     , HasLens StateLock ctx StateLock
     , DB.MonadBlockDB m
     , MonadMask m
     )

type WalletTrackingEnvRead ctx m =
     ( BlockLockMode ctx m
     , MonadTxpMem WalletMempoolExt ctx m
     , WS.MonadWalletDBRead ctx m
     , MonadSlotsData ctx m
     , WithLogger m
     , HasConfiguration
     )

type WalletTrackingEnv ctx m =
     ( WalletTrackingEnvRead ctx m
     , MonadWalletDB ctx m
     )

syncWalletOnImport :: WalletTrackingEnv ctx m => EncryptedSecretKey -> m ()
syncWalletOnImport = syncWalletsWithGState . one

txMempoolToModifier :: WalletTrackingEnvRead ctx m => EncryptedSecretKey -> m CAccModifier
txMempoolToModifier encSK = do
    let wHash (i, TxAux {..}, _) = WithHash taTx i
        wId = encToCId encSK
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

    tipH <- DB.getTipHeader
    allAddresses <- getWalletAddrMetas Ever wId
    case topsortTxs wHash txsWUndo of
        Nothing      -> mempty <$ logWarning "txMempoolToModifier: couldn't topsort mempool txs"
        Just ordered -> pure $
            trackingApplyTxs encSK allAddresses getDiff getTs getPtxBlkInfo $
            map (\(_, tx, undo) -> (tx, undo, tipH)) ordered

----------------------------------------------------------------------------
-- Logic
----------------------------------------------------------------------------

-- Iterate over blocks (using forward links) and actualize our accounts.
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
        Just (SyncedWith wTip) -> DB.blkGetHeader wTip >>= \case
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
        -- If account's syncTip is before the current gstate's tip,
        -- then it loads accounts and addresses starting with @wHeader@.
        -- syncTip can be before gstate's the current tip
        -- when we call @syncWalletSetWithTip@ at the first time
        -- or if the application was interrupted during rollback.
        -- We don't load all blocks explicitly, because blockain can be long.
        wNewTip <-
            if (gstateTipH ^. difficultyL > fromIntegral blkSecurityParam + fromIntegral wdiff) then do
                -- Wallet tip is "far" from gState tip,
                -- rollback can't occur more then @blkSecurityParam@ blocks,
                -- so we can sync wallet and GState without the block lock
                -- to avoid blocking of blocks verification/application.
                bh <- unsafeLast . getNewestFirst <$> DB.loadHeadersByDepth (blkSecurityParam + 1) (headerHash gstateTipH)
                logInfo $
                    sformat ("Wallet's tip is far from GState tip. Syncing with "%build%" without the block lock")
                    (headerHash bh)
                syncWalletWithGStateUnsafe encSK wTipH bh
                pure $ Just bh
            else pure wTipH
        withStateLockNoMetrics HighPriority $ \tip -> do
            logInfo $ sformat ("Syncing wallet with "%build%" under the block lock") tip
            tipH <- maybe (error "No block header corresponding to tip") pure =<< DB.blkGetHeader tip
            syncWalletWithGStateUnsafe encSK wNewTip tipH

----------------------------------------------------------------------------
-- Unsafe operations. Core logic.
----------------------------------------------------------------------------
-- These operation aren't atomic and don't take the block lock.

-- BE CAREFUL! This function iterates over blockchain, the blockchain can be large.
syncWalletWithGStateUnsafe
    :: forall ctx m .
    ( MonadWalletDB ctx m
    , DB.MonadBlockDB m
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
        loadCond (b, _) _ = b ^. difficultyL <= gstateH ^. difficultyL
        wAddr = encToCId encSK
        mappendR r mm = pure (r <> mm)
        diff = (^. difficultyL)
        mDiff = Just . diff
        gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)

        mainBlkHeaderTs mBlkH =
          getSlotStartPure systemStart (mBlkH ^. headerSlotL) slottingData
        blkHeaderTs = either (const Nothing) mainBlkHeaderTs

        -- assuming that transactions are not created until syncing is complete
        ptxBlkInfo = const Nothing

        rollbackBlock :: [CWAddressMeta] -> Blund -> CAccModifier
        rollbackBlock allAddresses (b, u) =
            trackingRollbackTxs encSK allAddresses mDiff blkHeaderTs $
            zip3 (gbTxs b) (undoTx u) (repeat $ getBlockHeader b)

        applyBlock :: [CWAddressMeta] -> Blund -> m CAccModifier
        applyBlock allAddresses (b, u) = pure $
            trackingApplyTxs encSK allAddresses mDiff blkHeaderTs ptxBlkInfo $
            zip3 (gbTxs b) (undoTx u) (repeat $ getBlockHeader b)

        computeAccModifier :: BlockHeader -> m CAccModifier
        computeAccModifier wHeader = do
            allAddresses <- getWalletAddrMetas Ever wAddr
            logInfoS $
                sformat ("Wallet "%build%" header: "%build%", current tip header: "%build)
                wAddr wHeader gstateH
            if | diff gstateH > diff wHeader -> do
                     -- If wallet's syncTip is before than the current tip in the blockchain,
                     -- then it loads wallets starting with @wHeader@.
                     -- Sync tip can be before the current tip
                     -- when we call @syncWalletSetWithTip@ at the first time
                     -- or if the application was interrupted during rollback.
                     -- We don't load blocks explicitly, because blockain can be long.
                     maybe (pure mempty)
                         (\wNextH ->
                            foldlUpWhileM DB.blkGetBlund (applyBlock allAddresses) wNextH loadCond mappendR mempty)
                         =<< resolveForwardLink wHeader
               | diff gstateH < diff wHeader -> do
                     -- This rollback can occur
                     -- if the application was interrupted during blocks application.
                     blunds <- getNewestFirst <$>
                         DB.loadBlundsWhile (\b -> getBlockHeader b /= gstateH) (headerHash wHeader)
                     pure $ foldl' (\r b -> r <> rollbackBlock allAddresses b) mempty blunds
               | otherwise -> mempty <$ logInfoS (sformat ("Wallet "%build%" is already synced") wAddr)

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
    mapModifier@CAccModifier{..} <- computeAccModifier startFromH
    applyModifierToWallet wAddr gstateHHash mapModifier
    -- Mark the wallet as ready, so it will be available from api endpoints.
    WS.setWalletReady wAddr True
    logInfoS $
        sformat ("Wallet "%build%" has been synced with tip "
                %shortHashF%", "%build)
                wAddr (maybe genesisHash headerHash wTipHeader) mapModifier
  where
    firstGenesisHeader :: m BlockHeader
    firstGenesisHeader = resolveForwardLink (genesisHash @BlockHeaderStub) >>=
        maybe (error "Unexpected state: genesisHash doesn't have forward link")
            (maybe (error "No genesis block corresponding to header hash") pure <=< DB.blkGetHeader)

-- Process transactions on block application,
-- decrypt our addresses, and add/delete them to/from wallet-db.
-- Addresses are used in TxIn's will be deleted,
-- in TxOut's will be added.
trackingApplyTxs
    :: HasConfiguration
    => EncryptedSecretKey                          -- ^ Wallet's secret key
    -> [CWAddressMeta]                             -- ^ All addresses in wallet
    -> (BlockHeader -> Maybe ChainDifficulty)      -- ^ Function to determine tx chain difficulty
    -> (BlockHeader -> Maybe Timestamp)            -- ^ Function to determine tx timestamp in history
    -> (BlockHeader -> Maybe PtxBlockInfo)         -- ^ Function to determine pending tx's block info
    -> [(TxAux, TxUndo, BlockHeader)]              -- ^ Txs of blocks and corresponding header hash
    -> CAccModifier
trackingApplyTxs (eskToWalletDecrCredentials -> wdc) allAddresses getDiff getTs getPtxBlkInfo txs =
    foldl' applyTx mempty txs
  where
    applyTx :: CAccModifier -> (TxAux, TxUndo, BlockHeader) -> CAccModifier
    applyTx CAccModifier{..} (tx, undo, blkHeader) = do
        let hh = headerHash blkHeader
            hhs = repeat hh
            wh@(WithHash _ txId) = withHash (taTx tx)
        let thee@THEntryExtra{..} =
                buildTHEntryExtra wdc (wh, undo) (getDiff blkHeader, getTs blkHeader)

            ownTxIns = map (fst . fst) theeInputs
            ownTxOuts = map fst theeOutputs

            addedHistory = maybe camAddedHistory (flip DL.cons camAddedHistory) (isTxEntryInteresting thee)

            usedAddrs = map (cwamId . snd) theeOutputs
            changeAddrs = evalChange allAddresses (map (cwamId . snd) theeInputs) usedAddrs

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
    => EncryptedSecretKey -- ^ Wallet's secret key
    -> [CWAddressMeta]    -- ^ All addresses
    -> (BlockHeader -> Maybe ChainDifficulty)  -- ^ Function to determine tx chain difficulty
    -> (BlockHeader -> Maybe Timestamp)        -- ^ Function to determine tx timestamp in history
    -> [(TxAux, TxUndo, BlockHeader)] -- ^ Txs of blocks and corresponding header hash
    -> CAccModifier
trackingRollbackTxs (eskToWalletDecrCredentials -> wdc) allAddress getDiff getTs txs =
    foldl' rollbackTx mempty txs
  where
    rollbackTx :: CAccModifier -> (TxAux, TxUndo, BlockHeader) -> CAccModifier
    rollbackTx CAccModifier{..} (tx, undo, blkHeader) = do
        let wh@(WithHash _ txId) = withHash (taTx tx)
            hh = headerHash blkHeader
            hhs = repeat hh
            thee@THEntryExtra{..} =
                buildTHEntryExtra wdc (wh, undo) (getDiff blkHeader, getTs blkHeader)

            ownTxOutIns = map (fst . fst) theeOutputs
            deletedHistory = maybe camDeletedHistory (DL.snoc camDeletedHistory) (isTxEntryInteresting thee)
            deletedPtxCandidates = DL.cons (txId, theeTxEntry) camDeletedPtxCandidates

        -- Rollback isn't needed, because we don't use @utxoGet@
        -- (undo contains all required information)
        let usedAddrs = map (cwamId . snd) theeOutputs
            changeAddrs = evalChange allAddress (map (cwamId . snd) theeInputs) usedAddrs
        CAccModifier
            (deleteAndInsertIMM (map snd theeOutputs) [] camAddresses)
            (deleteAndInsertVM (zip usedAddrs hhs) [] camUsed)
            (deleteAndInsertVM (zip changeAddrs hhs) [] camChange)
            (deleteAndInsertMM ownTxOutIns (map fst theeInputs) camUtxo)
            camAddedHistory
            deletedHistory
            camAddedPtxCandidates
            deletedPtxCandidates

applyModifierToWallet
    :: MonadWalletDB ctx m
    => CId Wal
    -> HeaderHash
    -> CAccModifier
    -> m ()
applyModifierToWallet wid newTip CAccModifier{..} = do
    -- TODO maybe do it as one acid-state transaction.
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
    -- resubmitting worker can change ptx in db nonatomically, but
    -- tracker has priority over the resubmiter, thus do not use CAS here
    forM_ camAddedPtxCandidates $ \(txid, ptxBlkInfo) ->
        WS.setPtxCondition wid txid (PtxInNewestBlocks ptxBlkInfo)
    WS.setWalletSyncTip wid newTip

rollbackModifierFromWallet
    :: (MonadWalletDB ctx m, MonadSlots ctx m)
    => CId Wal
    -> HeaderHash
    -> CAccModifier
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

evalChange
    :: [CWAddressMeta] -- ^ All adresses
    -> [CId Addr]      -- ^ Own input addresses of tx
    -> [CId Addr]      -- ^ Own outputs addresses of tx
    -> [CId Addr]
evalChange allAddresses inputs outputs
    | null inputs || null outputs = []
    | otherwise = filter (isTxLocalAddress allAddresses (NE.fromList inputs)) outputs

setLogger :: HasLoggerName m => m a -> m a
setLogger = modifyLoggerName (<> "wallet" <> "sync")

----------------------------------------------------------------------------
-- Cached modifier
----------------------------------------------------------------------------

-- | Evaluates `txMempoolToModifier` and provides result as a parameter
-- to given function.
fixingCachedAccModifier
    :: (WalletTrackingEnvRead ctx m, MonadKeySearch key m)
    => (CachedCAccModifier -> key -> m a)
    -> key -> m a
fixingCachedAccModifier action key =
    findKey key >>= txMempoolToModifier >>= flip action key

fixCachedAccModifierFor
    :: (WalletTrackingEnvRead ctx m, MonadKeySearch key m)
    => key
    -> (CachedCAccModifier -> m a)
    -> m a
fixCachedAccModifierFor key action =
    fixingCachedAccModifier (const . action) key
