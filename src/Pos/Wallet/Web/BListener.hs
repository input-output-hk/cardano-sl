{-# LANGUAGE ScopedTypeVariables #-}

-- | Instance Blockchain Listener for WalletWebDB.
-- Guaranteed that state of GStateDB and BlockDB isn't changed
-- during @onApplyBlocks@ and @onRollbackBlocks@ callbacks.

module Pos.Wallet.Web.BListener
       ( -- BListener instance.
       ) where

import           Universum

import           Control.Lens               (to)
import qualified Data.List.NonEmpty         as NE
import           Formatting                 (build, sformat, (%))
import           Mockable                   (MonadMockable)
import           System.Wlog                (WithLogger, logDebug, logWarning)

import           Pos.Block.BListener        (MonadBListener (..))
import           Pos.Block.Core             (BlockHeader, blockHeader, mainBlockTxPayload)
import           Pos.Block.Types            (Blund, undoTx)
import           Pos.Core                   (HeaderHash, difficultyL, headerHash,
                                             headerSlotL, prevBlockL)
import           Pos.DB.BatchOp             (SomeBatchOp)
import           Pos.DB.Class               (MonadDBRead, MonadRealDB)
import qualified Pos.DB.GState              as GS
import           Pos.Slotting               (SlottingData, getSlotStartPure)
import           Pos.Ssc.Class.Helpers      (SscHelpersClass)
import           Pos.Txp.Core               (TxAux (..), TxUndo, flattenTxPayload)
import           Pos.Txp.Toil               (evalToilTEmpty, runDBTxp)
import           Pos.Util.Chrono            (NE, NewestFirst (..), OldestFirst (..))

import           Pos.Wallet.KeyStorage      (MonadKeys)
import           Pos.Wallet.Web.Account     (AccountMode, getSKByAddr)
import           Pos.Wallet.Web.ClientTypes (CId, Wal)
import           Pos.Wallet.Web.State       (AddressLookupMode (..), WalletWebDB)
import qualified Pos.Wallet.Web.State       as WS
import           Pos.Wallet.Web.Tracking    (CAccModifier (..), applyModifierToWallet,
                                             getWalletAddrMetasDB,
                                             rollbackModifierFromWallet, trackingApplyTxs,
                                             trackingRollbackTxs)

instance ( MonadRealDB m
         , MonadDBRead m
         , MonadMockable m
         , MonadKeys m
         , WithLogger m
         )
         => MonadBListener (WalletWebDB m) where
    onApplyBlocks = onApplyTracking
    onRollbackBlocks = onRollbackTracking


walletGuard ::
    ( AccountMode m
    , WithLogger m
    )
    => HeaderHash
    -> CId Wal
    -> m ()
    -> m ()
walletGuard curTip wAddr action = WS.getWalletSyncTip wAddr >>= \case
    Nothing -> logWarning $ sformat ("Wallet Tracking: there is no syncTip corresponding to wallet #"%build) wAddr
    Just WS.NotSynced    -> logDebug $ sformat ("Wallet Tracking: Wallet #"%build%" hasn't been synced yet") wAddr
    Just (WS.SyncedWith wTip)
        | wTip /= curTip ->
            logDebug $
                sformat ("Wallet Tracking: skip wallet #"%build
                        %", because of wallet's tip "%build%" mismatched with current tip") wAddr wTip
        | otherwise -> action

-- Perform this action under block lock.
onApplyTracking
    :: forall ssc m .
    ( SscHelpersClass ssc
    , AccountMode m
    , WithLogger m
    , MonadRealDB m
    , MonadDBRead m
    )
    => OldestFirst NE (Blund ssc) -> m SomeBatchOp
onApplyTracking blunds = do
    let oldestFirst = getOldestFirst blunds
        txs = concatMap (gbTxs . fst) oldestFirst
        newTipH = NE.last oldestFirst ^. _1 . blockHeader
    currentTipHH <- GS.getTip
    sd <- GS.getSlottingData
    mapM_ (syncWallet sd currentTipHH newTipH txs) =<< WS.getWalletAddresses

    -- It's silly, but when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where
    syncWallet :: SlottingData -> HeaderHash -> BlockHeader ssc -> [TxAux] -> CId Wal -> m ()
    syncWallet sd curTip newTipH txs wAddr = walletGuard curTip wAddr $ do
        let mainBlkHeaderTs mBlkH =
                getSlotStartPure True (mBlkH ^. headerSlotL) sd
            blkHeaderTs = either (const Nothing) mainBlkHeaderTs

        allAddresses <- getWalletAddrMetasDB WS.Ever wAddr
        encSK <- getSKByAddr wAddr
        mapModifier <- runDBTxp $
                    evalToilTEmpty $
                    trackingApplyTxs encSK allAddresses gbDiff blkHeaderTs $
                    zip txs $ repeat newTipH
        applyModifierToWallet wAddr (headerHash newTipH) mapModifier
        logMsg "applied" (getOldestFirst blunds) wAddr mapModifier

    gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)
    gbDiff = Just . view difficultyL

-- Perform this action under block lock.
onRollbackTracking
    :: forall ssc m .
    ( SscHelpersClass ssc
    , AccountMode m
    , WithLogger m
    , MonadDBRead m
    )
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
onRollbackTracking blunds = do
    let newestFirst = getNewestFirst blunds
        txs = concatMap (reverse . blundTxUn) newestFirst
        newTip = (NE.last newestFirst) ^. prevBlockL
    currentTipHH <- GS.getTip
    mapM_ (syncWallet currentTipHH newTip txs) =<< WS.getWalletAddresses

    -- It's silly, but when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where
    syncWallet :: HeaderHash -> HeaderHash -> [(TxAux, TxUndo)] -> CId Wal -> m ()
    syncWallet curTip newTip txs wAddr = walletGuard curTip wAddr $ do
        allAddresses <- getWalletAddrMetasDB Ever wAddr
        encSK <- getSKByAddr wAddr
        let mapModifier = trackingRollbackTxs encSK allAddresses $
                          map (\(aux, undo) -> (aux, undo, newTip)) txs
        rollbackModifierFromWallet wAddr newTip mapModifier
        logMsg "rolled back" (getNewestFirst blunds) wAddr mapModifier
    gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)
    blundTxUn (b, u) = zip (gbTxs b) (undoTx u)

logMsg
    :: WithLogger m
    => Text
    -> NonEmpty (Blund ssc)
    -> CId Wal
    -> CAccModifier
    -> m ()
logMsg action (NE.length -> bNums) wAddr accModifier =
    logDebug $
        sformat ("Wallet Tracking: "%build%" "%build%" block(s) to walletset "%build
                %", "%build)
        action bNums wAddr accModifier
