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
import           Pos.Core (HeaderHash, Timestamp, difficultyL, headerSlotL, prevBlockL)
import           Pos.Core.Block (BlockHeader (..), blockHeader, getBlockHeader, mainBlockTxPayload)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Core.NetworkMagic (NetworkMagic)
import           Pos.Core.Txp (TxAux (..), TxUndo)
import           Pos.DB.BatchOp (SomeBatchOp)
import           Pos.DB.Class (MonadDBRead)
import qualified Pos.GState as GS
import           Pos.Infra.Reporting (MonadReporting, reportOrLogE)
import           Pos.Infra.Slotting (MonadSlots, MonadSlotsData, getCurrentEpochSlotDuration,
                                     getSlotStartPure, getSystemStartM)
import           Pos.Infra.Util.LogSafe (buildSafe, logInfoSP, logWarningSP, secretOnlyF, secure)
import           Pos.Infra.Util.TimeLimit (CanLogInParallel, logWarningWaitInf)
import           Pos.Txp.Base (flattenTxPayload)
import           Pos.Wallet.Web.Tracking.Decrypt (eskToWalletDecrCredentials)

import           Pos.Wallet.Web.Account (AccountMode, getSKById)
import           Pos.Wallet.Web.ClientTypes (CId, Wal)
import qualified Pos.Wallet.Web.State as WS
import           Pos.Wallet.Web.Tracking.Modifier (CAccModifier (..))
import           Pos.Wallet.Web.Tracking.Sync (applyModifierToWallet, rollbackModifierFromWallet,
                                               trackingApplyTxs, trackingRollbackTxs)
import           Pos.Wallet.Web.Tracking.Types (TrackingOperation (..))


walletGuard ::
       (WithLogger m, MonadIO m)
    => WS.WalletSnapshot
    -> HeaderHash
    -> CId Wal
    -> m ()
    -> m ()
walletGuard ws curTip wAddr action = case WS.getWalletSyncState ws wAddr of
    Nothing -> logWarningSP $ \sl -> sformat ("There is no syncTip corresponding to wallet #"%secretOnlyF sl build) wAddr
    Just WS.NotSynced    -> logInfoSP $ \sl -> sformat ("Wallet #"%secretOnlyF sl build%" hasn't been synced yet") wAddr
    Just (WS.SyncedWith wTip)      -> tipGuard wTip
    Just (WS.RestoringFrom _ _) -> do
        logWarningSP $ \sl ->
            sformat ( "Wallet #"%secretOnlyF sl build%" is restoring, not tracking it just yet...") wAddr
    where
        tipGuard wTip
            | wTip /= curTip =
                logWarningSP $ \sl ->
                    sformat ("Skip wallet #"%secretOnlyF sl build%", because of wallet's tip "%build
                             %" mismatched with current tip") wAddr wTip
            | otherwise = action

-- Perform this action under block lock.
onApplyBlocksWebWallet
    :: forall ctx m .
    ( AccountMode ctx m
    , WS.WalletDbReader ctx m
    , MonadSlotsData ctx m
    , MonadDBRead m
    , MonadReporting m
    , CanLogInParallel m
    )
    => NetworkMagic -> OldestFirst NE Blund -> m SomeBatchOp
onApplyBlocksWebWallet nm blunds = setLogger . reportTimeouts "apply" $ do
    db <- WS.askWalletDB
    ws <- WS.getWalletSnapshot db
    let oldestFirst = getOldestFirst blunds
        txsWUndo = concatMap gbTxsWUndo oldestFirst
        newTipH = NE.last oldestFirst ^. _1 . blockHeader
    currentTipHH <- GS.getTip
    mapM_ (catchInSync "apply" $ syncWallet db ws currentTipHH newTipH txsWUndo)
          (WS.getWalletAddresses ws)

    -- It's silly, but when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where
    syncWallet
        :: WS.WalletDB
        -> WS.WalletSnapshot
        -> HeaderHash
        -> BlockHeader
        -> [(TxAux, TxUndo, BlockHeader)]
        -> CId Wal
        -> m ()
    syncWallet db ws curTip newTipH blkTxsWUndo wAddr = walletGuard ws curTip wAddr $ do
        blkHeaderTs <- blkHeaderTsGetter
        encSK <- getSKById nm wAddr

        let credentials = eskToWalletDecrCredentials nm encSK
        let dbUsed = WS.getCustomAddresses ws WS.UsedAddr
        let applyBlockWith trackingOp = do
              let mapModifier = trackingApplyTxs credentials dbUsed gbDiff blkHeaderTs ptxBlkInfo blkTxsWUndo
              applyModifierToWallet db trackingOp wAddr newTipH mapModifier
              logMsg "Applied" (getOldestFirst blunds) wAddr mapModifier

        applyBlockWith SyncWallet

    gbDiff = Just . view difficultyL
    ptxBlkInfo = \case
        BlockHeaderGenesis _ -> Nothing
        BlockHeaderMain h -> Just $ h ^. difficultyL

-- Perform this action under block lock.
onRollbackBlocksWebWallet
    :: forall ctx m .
    ( AccountMode ctx m
    , WS.WalletDbReader ctx m
    , MonadDBRead m
    , MonadSlots ctx m
    , MonadReporting m
    , CanLogInParallel m
    )
    => NetworkMagic
    -> NewestFirst NE Blund
    -> m SomeBatchOp
onRollbackBlocksWebWallet nm blunds = setLogger . reportTimeouts "rollback" $ do
    db <- WS.askWalletDB
    ws <- WS.getWalletSnapshot db
    let newestFirst = getNewestFirst blunds
        txs = concatMap (reverse . gbTxsWUndo) newestFirst
        newTip = (NE.last newestFirst) ^. prevBlockL
    currentTipHH <- GS.getTip
    mapM_ (catchInSync "rollback" $ syncWallet db ws currentTipHH newTip txs)
          (WS.getWalletAddresses ws)

    -- It's silly, but when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where
    syncWallet
        :: WS.WalletDB
        -> WS.WalletSnapshot
        -> HeaderHash
        -> HeaderHash
        -> [(TxAux, TxUndo, BlockHeader)]
        -> CId Wal
        -> m ()
    syncWallet db ws curTip newTip txs wid = walletGuard ws curTip wid $ do
        encSK <- getSKById nm wid
        blkHeaderTs <- blkHeaderTsGetter

        let rollbackBlockWith trackingOperation = do
              let dbUsed = WS.getCustomAddresses ws WS.UsedAddr
                  mapModifier = trackingRollbackTxs (eskToWalletDecrCredentials nm encSK) dbUsed gbDiff blkHeaderTs txs
              rollbackModifierFromWallet db trackingOperation wid newTip mapModifier
              logMsg "Rolled back" (getNewestFirst blunds) wid mapModifier

        rollbackBlockWith SyncWallet

    gbDiff = Just . view difficultyL

blkHeaderTsGetter
    :: ( MonadSlotsData ctx m
       , MonadDBRead m
       )
    => m (BlockHeader -> Maybe Timestamp)
blkHeaderTsGetter = do
    systemStart <- getSystemStartM
    sd <- GS.getSlottingData
    let mainBlkHeaderTs mBlkH =
            getSlotStartPure systemStart (mBlkH ^. headerSlotL) sd
    return $ \case
        BlockHeaderGenesis _ -> Nothing
        BlockHeaderMain h -> mainBlkHeaderTs h

gbTxsWUndo :: Blund -> [(TxAux, TxUndo, BlockHeader)]
gbTxsWUndo (Left _, _) = []
gbTxsWUndo (blk@(Right mb), undo) =
    zip3 (mb ^. mainBlockTxPayload . to flattenTxPayload)
            (undoTx undo)
            (repeat $ getBlockHeader blk)

setLogger :: HasLoggerName m => m a -> m a
setLogger = modifyLoggerName (const "syncWalletBListener")

reportTimeouts
    :: (MonadSlotsData ctx m, CanLogInParallel m)
    => Text -> m a -> m a
reportTimeouts desc action = do
    slotDuration <- getCurrentEpochSlotDuration
    let firstWarningTime = convertUnit slotDuration `div` 2
    logWarningWaitInf firstWarningTime tag action
  where
    tag = "Wallet blistener " <> desc

logMsg
    :: (MonadIO m, WithLogger m)
    => Text
    -> NonEmpty Blund
    -> CId Wal
    -> CAccModifier
    -> m ()
logMsg action (NE.length -> bNums) wid accModifier =
    logInfoSP $ \sl ->
        sformat (build%" "%build%" block(s) to wallet "%secretOnlyF sl build%", "%buildSafe sl)
             action bNums wid accModifier

catchInSync
    :: (MonadReporting m, MonadIO m, WithLogger m, MonadCatch m)
    => Text -> (CId Wal -> m ()) -> CId Wal -> m ()
catchInSync desc syncWallet wId =
    syncWallet wId `catchAny` \e -> do
        reportOrLogE (prefix secure) e
        logWarningSP $ \sl -> prefix sl <> show e
  where
    -- REPORT:ERROR 'reportOrLogW' in wallet sync.
    fmt sl = "Failed to sync wallet "%secretOnlyF sl build%" in BListener ("%build%"): "
    prefix sl = sformat (fmt sl) wId desc
