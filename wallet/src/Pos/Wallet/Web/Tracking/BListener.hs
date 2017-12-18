{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Instance Blockchain Listener for WalletWebDB.
-- Guaranteed that state of GStateDB and BlockDB isn't changed
-- during @onApplyBlocks@ and @onRollbackBlocks@ callbacks.

module Pos.Wallet.Web.Tracking.BListener
       ( MonadBListener(..)
       , onApplyBlocksWebWallet
       , onRollbackBlocksWebWallet
       ) where

import           Universum

import           Control.Lens (to)
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (convertUnit)
import           Formatting (build, sformat, (%))
import           System.Wlog (HasLoggerName (modifyLoggerName), WithLogger)

import           Pos.Block.BListener (MonadBListener (..))
import           Pos.Block.Types (Blund, undoTx)
import           Pos.Core (HasConfiguration, HeaderHash, Timestamp, difficultyL, headerHash,
                           headerSlotL, prevBlockL)
import           Pos.Core.Block (BlockHeader, blockHeader, getBlockHeader, mainBlockTxPayload)
import           Pos.Core.Txp (TxAux (..), TxUndo)
import           Pos.DB.BatchOp (SomeBatchOp)
import           Pos.DB.Class (MonadDBRead)
import qualified Pos.GState as GS
import           Pos.Reporting (MonadReporting, reportOrLogW)
import           Pos.Slotting (MonadSlots, MonadSlotsData, getCurrentEpochSlotDuration,
                               getSlotStartPure, getSystemStartM)
import           Pos.Txp.Base (flattenTxPayload)
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Util.LogSafe (logInfoS, logWarningS)
import           Pos.Util.TimeLimit (CanLogInParallel, logWarningWaitInf)

import           Pos.Wallet.Web.Account (AccountMode, getSKById)
import           Pos.Wallet.Web.ClientTypes (CId, Wal)
import qualified Pos.Wallet.Web.State as WS
import           Pos.Wallet.Web.Tracking.Modifier (CAccModifier (..))
import           Pos.Wallet.Web.Tracking.Sync (applyModifierToWallet, rollbackModifierFromWallet,
                                               trackingApplyTxs, trackingRollbackTxs)

-- | A function which performs provided action only if given wallet is
-- is synced with blockchain up until given hash.
walletGuard ::
    ( AccountMode ctx m
    )
    => HeaderHash -- ^ Block header hash to check wallet sync tip against.
    -> CId Wal    -- ^ Wallet ID which sync tip to check.
    -> m ()       -- ^ Action to perform
    -> m ()
walletGuard curTip wAddr action = WS.getWalletSyncTip wAddr >>= \case
    Nothing ->
        -- Happens if the wallet isn't present in the wallet DB or the DB
        -- is corrupted.
        logWarningS $ sformat ("There is no syncTip corresponding to wallet #"%build) wAddr
    Just WS.NotSynced ->
        -- Happens if initial synchronization hasn't been done to given wallet.
        -- Normally it's the case only for wallets for which initial synchronization
        -- process is currently in progress.
        logInfoS $ sformat ("Wallet #"%build%" hasn't been synced yet") wAddr
    Just (WS.SyncedWith wTip)
        -- Otherwise, just compare wallet's sync tip with provided hash.
        | wTip /= curTip ->
            logWarningS $
                sformat ("Skip wallet #"%build%", because of wallet's tip "%build
                         %" mismatched with current tip") wAddr wTip
        | otherwise -> action

-- | Action which is performed when a block (or set of several blocks) is applied.
-- Always performed under block semaphore.
onApplyBlocksWebWallet
    :: forall ctx m .
    ( AccountMode ctx m
    , WS.MonadWalletDB ctx m
    , MonadSlotsData ctx m мог ебнуться getTxFee
    , MonadDBRead m
    , MonadReporting ctx m
    , CanLogInParallel m
    , HasConfiguration
    )
    => OldestFirst NE Blund -> m SomeBatchOp
onApplyBlocksWebWallet blunds = setLogger . reportTimeouts "apply" $ do
    let oldestFirst = getOldestFirst blunds
        -- Get all transactions from provided blocks
        txsWUndo = concatMap gbTxsWUndo oldestFirst
        newTipH = NE.last oldestFirst ^. _1 . blockHeader
    currentTipHH <- GS.getTip
    -- Synchronize every wallet in database separately
    mapM_ (catchInSync "apply" $ syncWallet currentTipHH newTipH txsWUndo)
       =<< WS.getWalletAddresses

    -- It's silly, but if/when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where
    -- Given all transactions from provided blocks, apply them if wallet sync tip
    -- has lower difficulty than blockchain tip.
    -- See 'Pos.Wallet.Web.Tracking.Sync.trackingApplyTxs' and
    -- 'Pos.Wallet.Web.Tracking.Sync.applyModifierToWallet'
    -- for details of implementation.
    syncWallet
        :: HeaderHash
        -> BlockHeader
        -> [(TxAux, TxUndo, BlockHeader)]
        -> CId Wal
        -> m ()
    syncWallet curTip newTipH blkTxsWUndo wAddr = walletGuard curTip wAddr $ do
        blkHeaderTs <- blkHeaderTsGetter
        dbUsed <- WS.getCustomAddresses WS.UsedAddr
        encSK <- getSKById wAddr
        let mapModifier =
                trackingApplyTxs encSK dbUsed gbDiff blkHeaderTs ptxBlkInfo blkTxsWUndo
        applyModifierToWallet wAddr (headerHash newTipH) mapModifier
        logMsg "Applied" (getOldestFirst blunds) wAddr mapModifier

    gbDiff = Just . view difficultyL
    ptxBlkInfo = either (const Nothing) (Just . view difficultyL)

-- | Action which is performed when a block (or set of several blocks) is
-- rolled back. Always performed under block semaphore.
onRollbackBlocksWebWallet
    :: forall ctx m .
    ( AccountMode ctx m
    , WS.MonadWalletDB ctx m
    , MonadDBRead m
    , MonadSlots ctx m
    , MonadReporting ctx m
    , CanLogInParallel m
    , HasConfiguration
    )
    => NewestFirst NE Blund -> m SomeBatchOp
onRollbackBlocksWebWallet blunds = setLogger . reportTimeouts "rollback" $ do
    let newestFirst = getNewestFirst blunds
        -- Collect all transactions from txs payloads of given blocks.
        txs = concatMap (reverse . gbTxsWUndo) newestFirst
        newTip = (NE.last newestFirst) ^. prevBlockL
    currentTipHH <- GS.getTip
    -- Synchronize every wallet in database separately.
    mapM_ (catchInSync "rollback" $ syncWallet currentTipHH newTip txs)
        =<< WS.getWalletAddresses

    -- It's silly, but if/when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where
    -- Roll back transactions fetched from blocks if wallet sync tip has higher
    -- difficulty than current blockchain tip.
    -- See 'Pos.Wallet.Web.Tracking.Sync.trackingRollbackTxs' and
    -- 'Pos.Wallet.Web.Tracking.Sync.rollbackModifierFromWallet'
    -- for details of implementation.
    syncWallet
        :: HeaderHash
        -> HeaderHash
        -> [(TxAux, TxUndo, BlockHeader)]
        -> CId Wal
        -> m ()
    syncWallet curTip newTip txs wid = walletGuard curTip wid $ do
        encSK <- getSKById wid
        blkHeaderTs <- blkHeaderTsGetter
        dbUsed <- WS.getCustomAddresses WS.UsedAddr
        let mapModifier = trackingRollbackTxs encSK dbUsed gbDiff blkHeaderTs txs
        rollbackModifierFromWallet wid newTip mapModifier
        logMsg "Rolled back" (getNewestFirst blunds) wid mapModifier

    gbDiff = Just . view difficultyL

-- | Returns a function which determines correct slot start time
-- for a given block header.
blkHeaderTsGetter
    :: ( MonadSlotsData ctx m
       , MonadDBRead m
       , HasConfiguration
       )
    => m (BlockHeader -> Maybe Timestamp)
blkHeaderTsGetter = do
    systemStart <- getSystemStartM
    sd <- GS.getSlottingData
    let mainBlkHeaderTs mBlkH =
            getSlotStartPure systemStart (mBlkH ^. headerSlotL) sd
    return $ either (const Nothing) mainBlkHeaderTs

-- | Helper function to fetch list of transactions with corresponding
-- 'TxUndo's and headers of block containing the transaction from 'Blund'.
gbTxsWUndo :: Blund -> [(TxAux, TxUndo, BlockHeader)]
gbTxsWUndo (Left _, _) = []
gbTxsWUndo (blk@(Right mb), undo) =
    zip3 (mb ^. mainBlockTxPayload . to flattenTxPayload)
            (undoTx undo)
            (repeat $ getBlockHeader blk)

-- | Modify logger name to distinguish logs from inside 'BListener'
-- from other ones.
setLogger :: HasLoggerName m => m a -> m a
setLogger = modifyLoggerName (<> "wallet" <> "blistener")

-- | Log warning messages if 'BListener' application takes too much time
-- (more than half of slot duration, which is currently (december 2017)
-- 10 seconds).
-- See 'Pos.Util.TimeLimit' for details of internal implementaion.
reportTimeouts
    :: (MonadSlotsData ctx m, CanLogInParallel m)
    => Text -> m a -> m a
reportTimeouts desc action = do
    slotDuration <- getCurrentEpochSlotDuration
    let firstWarningTime = convertUnit slotDuration `div` 2
    logWarningWaitInf firstWarningTime tag action
  where
    tag = "Wallet blistener " <> desc

-- | Helper function for logging message about completing block application
-- or rollback procedures for particular wallet.
logMsg
    :: (MonadIO m, WithLogger m)
    => Text
    -> NonEmpty Blund
    -> CId Wal
    -> CAccModifier
    -> m ()
logMsg action (NE.length -> bNums) wid accModifier =
    logInfoS $
        sformat (build%" "%build%" block(s) to wallet "%build%", "%build)
             action bNums wid accModifier

-- | Catches errors which happen during synchronization of a single wallet
-- and log them or report to reporting server depending on type of error
-- (see 'Pos.Reporting.reportOrLogW' for details).
catchInSync
    :: (MonadReporting ctx m)
    => Text -> (CId Wal -> m ()) -> CId Wal -> m ()
catchInSync desc syncWallet wId =
    syncWallet wId `catchAny` reportOrLogW prefix
  where
    -- REPORT:ERROR 'reportOrLogW' in wallet sync.
    fmt = "Failed to sync wallet "%build%" in BListener ("%build%"): "
    prefix = sformat fmt wId desc
