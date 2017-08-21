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
import           Formatting                       (build, sformat, (%))
import           System.Wlog                      (HasLoggerName (modifyLoggerName),
                                                   WithLogger, logInfo, logWarning)

import           Pos.Block.BListener              (MonadBListener (..))
import           Pos.Block.Core                   (BlockHeader, blockHeader,
                                                   getBlockHeader, mainBlockTxPayload)
import           Pos.Block.Types                  (Blund, undoTx)
import           Pos.Core                         (HasCoreConstants, HeaderHash,
                                                   difficultyL, headerHash, headerSlotL,
                                                   prevBlockL)
import           Pos.DB.BatchOp                   (SomeBatchOp)
import           Pos.DB.Class                     (MonadDBRead)
import qualified Pos.GState                       as GS
import           Pos.Slotting                     (MonadSlotsData, getSlotStartPure,
                                                   getSystemStartM)
import           Pos.Ssc.Class.Helpers            (SscHelpersClass)
import           Pos.Txp.Core                     (TxAux (..), TxUndo, flattenTxPayload)
import           Pos.Util.Chrono                  (NE, NewestFirst (..), OldestFirst (..))

import           Pos.Wallet.Web.Account           (AccountMode, getSKById)
import           Pos.Wallet.Web.ClientTypes       (CId, Wal)
import qualified Pos.Wallet.Web.State             as WS
import           Pos.Wallet.Web.Tracking.Modifier (CAccModifier (..))
import           Pos.Wallet.Web.Tracking.Sync     (applyModifierToWallet,
                                                   rollbackModifierFromWallet,
                                                   trackingApplyTxs, trackingRollbackTxs)
import           Pos.Wallet.Web.Util              (getWalletAddrMetas)

walletGuard ::
    ( AccountMode ctx m
    )
    => HeaderHash
    -> CId Wal
    -> m ()
    -> m ()
walletGuard curTip wAddr action = WS.getWalletSyncTip wAddr >>= \case
    Nothing -> logWarning $ sformat ("There is no syncTip corresponding to wallet #"%build) wAddr
    Just WS.NotSynced    -> logInfo $ sformat ("Wallet #"%build%" hasn't been synced yet") wAddr
    Just (WS.SyncedWith wTip)
        | wTip /= curTip ->
            logWarning $
                sformat ("Skip wallet #"%build%", because of wallet's tip "%build
                         %" mismatched with current tip") wAddr wTip
        | otherwise -> action

-- Perform this action under block lock.
onApplyTracking
    :: forall ssc ctx m .
    ( SscHelpersClass ssc
    , AccountMode ctx m
    , MonadSlotsData ctx m
    , MonadDBRead m
    , HasCoreConstants
    )
    => OldestFirst NE (Blund ssc) -> m SomeBatchOp
onApplyTracking blunds = setLogger $ do
    let oldestFirst = getOldestFirst blunds
        txsWUndo = concatMap gbTxsWUndo oldestFirst
        newTipH = NE.last oldestFirst ^. _1 . blockHeader
    currentTipHH <- GS.getTip
    mapM_ (catchInSync "apply" $ syncWallet currentTipHH newTipH txsWUndo)
       =<< WS.getWalletAddresses

    -- It's silly, but when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where

    syncWallet
        :: HeaderHash
        -> BlockHeader ssc
        -> [(TxAux, TxUndo, BlockHeader ssc)]
        -> CId Wal
        -> m ()
    syncWallet curTip newTipH blkTxsWUndo wAddr = walletGuard curTip wAddr $ do

        systemStart <- getSystemStartM
        sd          <- GS.getSlottingData

        let mainBlkHeaderTs mBlkH =
              getSlotStartPure systemStart (mBlkH ^. headerSlotL) sd
            blkHeaderTs = either (const Nothing) mainBlkHeaderTs

        allAddresses <- getWalletAddrMetas WS.Ever wAddr
        encSK <- getSKById wAddr
        let mapModifier =
                trackingApplyTxs encSK allAddresses gbDiff blkHeaderTs blkTxsWUndo
        applyModifierToWallet wAddr (headerHash newTipH) mapModifier
        logMsg "Applied" (getOldestFirst blunds) wAddr mapModifier

    gbTxsWUndo :: Blund ssc -> [(TxAux, TxUndo, BlockHeader ssc)]
    gbTxsWUndo (Left _, _) = []
    gbTxsWUndo (blk@(Right mb), undo) =
        zip3 (mb ^. mainBlockTxPayload . to flattenTxPayload)
             (undoTx undo)
             (repeat $ getBlockHeader blk)
    gbDiff = Just . view difficultyL

-- Perform this action under block lock.
onRollbackTracking
    :: forall ssc ctx m .
    ( AccountMode ctx m
    , MonadDBRead m
    )
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
onRollbackTracking blunds = setLogger $ do
    let newestFirst = getNewestFirst blunds
        txs = concatMap (reverse . blundTxUn) newestFirst
        newTip = (NE.last newestFirst) ^. prevBlockL
    currentTipHH <- GS.getTip
    mapM_ (catchInSync "rollback" $ syncWallet currentTipHH newTip txs)
        =<< WS.getWalletAddresses

    -- It's silly, but when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where
    syncWallet :: HeaderHash -> HeaderHash -> [(TxAux, TxUndo)] -> CId Wal -> m ()
    syncWallet curTip newTip txs wid = walletGuard curTip wid $ do
        allAddresses <- getWalletAddrMetas WS.Ever wid
        encSK <- getSKById wid
        let mapModifier = trackingRollbackTxs encSK allAddresses $
                          map (\(aux, undo) -> (aux, undo, newTip)) txs
        rollbackModifierFromWallet wid newTip mapModifier
        logMsg "Rolled back" (getNewestFirst blunds) wid mapModifier
    gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)
    blundTxUn (b, u) = zip (gbTxs b) (undoTx u)

setLogger :: HasLoggerName m => m a -> m a
setLogger = modifyLoggerName (<> "wallet" <> "blistener")

logMsg
    :: WithLogger m
    => Text
    -> NonEmpty (Blund ssc)
    -> CId Wal
    -> CAccModifier
    -> m ()
logMsg action (NE.length -> bNums) wid accModifier =
    logInfo $
        sformat (build%" "%build%" block(s) to wallet "%build%", "%build)
             action bNums wid accModifier

catchInSync
    :: (MonadCatch m, WithLogger m)
    => Text -> (CId Wal -> m ()) -> CId Wal -> m ()
catchInSync desc syncWallet wId =
    syncWallet wId `catchAll` (logWarning . sformat fmt wId desc)
  where
    fmt = "Failed to sync wallet "%build%" in BListener ("%build%"): "%build
