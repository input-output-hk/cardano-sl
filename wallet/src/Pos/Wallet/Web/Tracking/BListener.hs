{-# LANGUAGE ScopedTypeVariables #-}

-- | Instance Blockchain Listener for WalletWebDB.
-- Guaranteed that state of GStateDB and BlockDB isn't changed
-- during @onApplyBlocks@ and @onRollbackBlocks@ callbacks.

module Pos.Wallet.Web.Tracking.BListener
       ( MonadBListener(..)
       , onApplyTracking
       , onRollbackTracking
       ) where

import           Universum

import           Control.Lens                     (to)
import qualified Data.List.NonEmpty               as NE
import           Data.Time.Units                  (convertUnit)
import           Formatting                       (build, sformat, (%))
import           System.Wlog                      (HasLoggerName (modifyLoggerName),
                                                   WithLogger)
import           Mockable                         (Async, Mockable, Delay)


import           Pos.Block.BListener              (MonadBListener (..))
import           Pos.Block.Core                   (BlockHeader, blockHeader,
                                                   getBlockHeader, mainBlockTxPayload)
import           Pos.Block.Types                  (Blund, undoTx)
import           Pos.Core                         (HasConfiguration, HeaderHash,
                                                   Timestamp, difficultyL, headerHash,
                                                   headerSlotL, prevBlockL)
import           Pos.DB.BatchOp                   (SomeBatchOp)
import           Pos.DB.Class                     (MonadDBRead)
import qualified Pos.GState                       as GS
import           Pos.Reporting                    (MonadReporting, reportOrLogW)
import           Pos.Slotting                     (MonadSlots, MonadSlotsData,
                                                   getCurrentEpochSlotDuration,
                                                   getSlotStartPure, getSystemStartM)
import           Pos.Ssc.Class.Helpers            (SscHelpersClass)
import           Pos.Txp.Core                     (TxAux (..), TxUndo, flattenTxPayload)
import           Pos.Util.Chrono                  (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Util.LogSafe                 (logInfoS, logWarningS)
import           Pos.Util.TimeLimit               (CanLogInParallel, logWarningWaitInf)

import           Pos.Wallet.Web.Account           (AccountMode, getSKById)
import           Pos.Wallet.Web.ClientTypes       (CId, Wal)
import qualified Pos.Wallet.Web.State             as WS
import           Pos.Wallet.Web.State             (WalletDbReader, WalletSnapshot)
import           Pos.Wallet.Web.Tracking.Modifier (CAccModifier (..))
import           Pos.Wallet.Web.Tracking.Sync     (applyModifierToWallet,
                                                   rollbackModifierFromWallet,
                                                   trackingApplyTxs, trackingRollbackTxs)
import           Pos.Wallet.Web.Util              (getWalletAddrMetas)

walletGuard ::
       (WithLogger m, MonadIO m)
    => WalletSnapshot
    -> HeaderHash
    -> CId Wal
    -> m ()
    -> m ()
walletGuard ws curTip wAddr action = case WS.getWalletSyncTip ws wAddr of
    Nothing -> logWarningS $ sformat ("There is no syncTip corresponding to wallet #"%build) wAddr
    Just WS.NotSynced    -> logInfoS $ sformat ("Wallet #"%build%" hasn't been synced yet") wAddr
    Just (WS.SyncedWith wTip)
        | wTip /= curTip ->
            logWarningS $
                sformat ("Skip wallet #"%build%", because of wallet's tip "%build
                         %" mismatched with current tip") wAddr wTip
        | otherwise -> action

-- Perform this action under block lock.
onApplyTracking
    :: forall ssc ctx m .
    ( SscHelpersClass ssc
    , WalletDbReader ctx m
    , Mockable Delay m
    , Mockable Async m
    , AccountMode ctx m
    , MonadSlotsData ctx m
    , MonadDBRead m
    , MonadReporting ctx m
    , HasConfiguration
    )
    => OldestFirst NE (Blund ssc) -> m SomeBatchOp
onApplyTracking blunds = setLogger . reportTimeouts "apply" $ do
    ws <- WS.getWalletSnapshot
    let oldestFirst = getOldestFirst blunds
        txsWUndo = concatMap gbTxsWUndo oldestFirst
        newTipH = NE.last oldestFirst ^. _1 . blockHeader
    currentTipHH <- GS.getTip
    mapM_ (catchInSync "apply" $ syncWallet ws currentTipHH newTipH txsWUndo)
          (WS.getWalletAddresses ws)

    -- It's silly, but when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where

    syncWallet
        :: WalletSnapshot
        -> HeaderHash
        -> BlockHeader ssc
        -> [(TxAux, TxUndo, BlockHeader ssc)]
        -> CId Wal
        -> m ()
    syncWallet ws curTip newTipH blkTxsWUndo wAddr = walletGuard ws curTip wAddr $ do
        blkHeaderTs <- blkHeaderTsGetter
        let allAddresses = getWalletAddrMetas ws WS.Ever wAddr
        encSK <- getSKById wAddr
        let mapModifier =
                trackingApplyTxs encSK allAddresses gbDiff blkHeaderTs ptxBlkInfo blkTxsWUndo
        applyModifierToWallet wAddr (headerHash newTipH) mapModifier
        logMsg "Applied" (getOldestFirst blunds) wAddr mapModifier

    gbDiff = Just . view difficultyL
    ptxBlkInfo = either (const Nothing) (Just . view difficultyL)

-- Perform this action under block lock.
onRollbackTracking
    :: forall ssc ctx m .
    ( AccountMode ctx m
    , WalletDbReader ctx m
    , Mockable Delay m
    , Mockable Async m
    , MonadDBRead m
    , MonadSlots ctx m
    , SscHelpersClass ssc
    , MonadReporting ctx m
    , HasConfiguration
    )
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
onRollbackTracking blunds = setLogger . reportTimeouts "rollback" $ do
    ws <- WS.getWalletSnapshot
    let newestFirst = getNewestFirst blunds
        txs = concatMap (reverse . gbTxsWUndo) newestFirst
        newTip = (NE.last newestFirst) ^. prevBlockL
    currentTipHH <- GS.getTip
    mapM_ (catchInSync "rollback" $ syncWallet ws currentTipHH newTip txs)
          (WS.getWalletAddresses ws)

    -- It's silly, but when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where
    syncWallet
        :: WalletSnapshot
        -> HeaderHash
        -> HeaderHash
        -> [(TxAux, TxUndo, BlockHeader ssc)]
        -> CId Wal
        -> m ()
    syncWallet ws curTip newTip txs wid = walletGuard ws curTip wid $ do
        let allAddresses = getWalletAddrMetas ws WS.Ever wid
        encSK <- getSKById wid
        blkHeaderTs <- blkHeaderTsGetter

        let mapModifier = trackingRollbackTxs encSK allAddresses gbDiff blkHeaderTs txs
        rollbackModifierFromWallet wid newTip mapModifier
        logMsg "Rolled back" (getNewestFirst blunds) wid mapModifier

    gbDiff = Just . view difficultyL

blkHeaderTsGetter
    :: ( MonadSlotsData ctx m
       , MonadDBRead m
       , SscHelpersClass ssc
       , HasConfiguration
       )
    => m (BlockHeader ssc -> Maybe Timestamp)
blkHeaderTsGetter = do
    systemStart <- getSystemStartM
    sd <- GS.getSlottingData
    let mainBlkHeaderTs mBlkH =
            getSlotStartPure systemStart (mBlkH ^. headerSlotL) sd
    return $ either (const Nothing) mainBlkHeaderTs

gbTxsWUndo :: Blund ssc -> [(TxAux, TxUndo, BlockHeader ssc)]
gbTxsWUndo (Left _, _) = []
gbTxsWUndo (blk@(Right mb), undo) =
    zip3 (mb ^. mainBlockTxPayload . to flattenTxPayload)
            (undoTx undo)
            (repeat $ getBlockHeader blk)

setLogger :: HasLoggerName m => m a -> m a
setLogger = modifyLoggerName (<> "wallet" <> "blistener")

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
    -> NonEmpty (Blund ssc)
    -> CId Wal
    -> CAccModifier
    -> m ()
logMsg action (NE.length -> bNums) wid accModifier =
    logInfoS $
        sformat (build%" "%build%" block(s) to wallet "%build%", "%build)
             action bNums wid accModifier

catchInSync
    :: (MonadReporting ctx m)
    => Text -> (CId Wal -> m ()) -> CId Wal -> m ()
catchInSync desc syncWallet wId =
    syncWallet wId `catchAny` reportOrLogW prefix
  where
    -- REPORT:ERROR 'reportOrLogW' in wallet sync.
    fmt = "Failed to sync wallet "%build%" in BListener ("%build%"): "
    prefix = sformat fmt wId desc
