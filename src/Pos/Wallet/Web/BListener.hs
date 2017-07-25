{-# LANGUAGE ScopedTypeVariables #-}

-- | Instance Blockchain Listener for WalletWebDB.
-- Guaranteed that state of GStateDB and BlockDB isn't changed
-- during @onApplyBlocks@ and @onRollbackBlocks@ callbacks.

module Pos.Wallet.Web.BListener
       ( MonadBListener(..)
       , onApplyTracking
       , onRollbackTracking
       ) where

import           Universum

import           Control.Lens               (to)
import qualified Data.List.NonEmpty         as NE
import           Formatting                 (build, sformat, (%))
import           System.Wlog                (WithLogger, logDebug, logWarning)

import           Pos.Block.BListener        (MonadBListener (..))
import           Pos.Block.Core             (BlockHeader, blockHeader, mainBlockTxPayload)
import           Pos.Block.Types            (Blund, undoTx)
import           Pos.Core                   (HeaderHash, difficultyL, headerHash,
                                             headerSlotL, prevBlockL)
import           Pos.DB.BatchOp             (SomeBatchOp)
import           Pos.DB.Class               (MonadDBRead)
import qualified Pos.GState                 as GS
import           Pos.Slotting               (MonadSlotsData (..), getSlotStartPure)
import           Pos.Ssc.Class.Helpers      (SscHelpersClass)
import           Pos.Txp.Core               (TxAux (..), TxUndo, flattenTxPayload)
import           Pos.Txp.Toil               (evalToilTEmpty, runDBToil)
import           Pos.Util.Chrono            (NE, NewestFirst (..), OldestFirst (..))

import           Pos.Wallet.Web.Account     (AccountMode, getSKByAddr)
import           Pos.Wallet.Web.ClientTypes (CId, Wal)
import qualified Pos.Wallet.Web.State       as WS
import           Pos.Wallet.Web.Tracking    (CAccModifier (..), applyModifierToWallet,
                                             getWalletAddrMetasDB,
                                             rollbackModifierFromWallet, trackingApplyTxs,
                                             trackingRollbackTxs)

-- Perform this action under block lock.
onApplyTracking
    :: forall ssc ctx m .
    ( SscHelpersClass ssc
    , AccountMode ctx m
    , MonadSlotsData m
    , MonadDBRead m
    )
    => OldestFirst NE (Blund ssc) -> m SomeBatchOp
onApplyTracking blunds = do

    let oldestFirst = getOldestFirst blunds
        txs = concatMap (gbTxs . fst) oldestFirst
        newTipH = NE.last oldestFirst ^. _1 . blockHeader
    mapM_ (catchInSync "apply" $ syncWallet newTipH txs)
        =<< WS.getWalletAddresses

    -- It's silly, but when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where
    syncWallet :: BlockHeader ssc -> [TxAux] -> CId Wal -> m ()
    syncWallet newTipH txs wid = do
        sd <- GS.getSlottingData
        let mainBlkHeaderTs mBlkH =
                getSlotStartPure (mBlkH ^. headerSlotL) sd
            blkHeaderTs = either (const Nothing) mainBlkHeaderTs

        allAddresses <- getWalletAddrMetasDB WS.Ever wid
        encSK <- getSKByAddr wid
        mapModifier <- runDBToil $
                       evalToilTEmpty $
                       trackingApplyTxs encSK allAddresses gbDiff blkHeaderTs $
                       zip txs $ repeat newTipH
        applyModifierToWallet wid (headerHash newTipH) mapModifier
        logMsg "applied" (getOldestFirst blunds) wid mapModifier

    gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)
    gbDiff = Just . view difficultyL

-- Perform this action under block lock.
onRollbackTracking
    :: forall ssc ctx m .
    ( SscHelpersClass ssc
    , AccountMode ctx m
    )
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
onRollbackTracking blunds = do
    let newestFirst = getNewestFirst blunds
        txs = concatMap (reverse . blundTxUn) newestFirst
        newTip = (NE.last newestFirst) ^. prevBlockL
    mapM_ (catchInSync "rollback" $ syncWallet newTip txs)
        =<< WS.getWalletAddresses

    -- It's silly, but when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where
    syncWallet :: HeaderHash -> [(TxAux, TxUndo)] -> CId Wal -> m ()
    syncWallet newTip txs wid = do
        allAddresses <- getWalletAddrMetasDB WS.Ever wid
        encSK <- getSKByAddr wid
        let mapModifier = trackingRollbackTxs encSK allAddresses $
                          map (\(aux, undo) -> (aux, undo, newTip)) txs
        rollbackModifierFromWallet wid newTip mapModifier
        logMsg "rolled back" (getNewestFirst blunds) wid mapModifier
    gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)
    blundTxUn (b, u) = zip (gbTxs b) (undoTx u)

logMsg
    :: WithLogger m
    => Text
    -> NonEmpty (Blund ssc)
    -> CId Wal
    -> CAccModifier
    -> m ()
logMsg action (NE.length -> bNums) wid accModifier =
    logDebug $
        sformat ("Wallet Tracking: "%build%" "%build%" block(s) to wallet "
                %build%", "%build)
        action bNums wid accModifier

catchInSync
    :: (MonadCatch m, WithLogger m)
    => Text -> (CId Wal -> m ()) -> CId Wal -> m ()
catchInSync desc syncWallet wId =
    syncWallet wId `catchAll` (logWarning . sformat fmt wId desc)
  where
    fmt = "Failed to sync wallet "%build%" in BListener ("%build%"): "%build
