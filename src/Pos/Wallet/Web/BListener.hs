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
import           Serokell.Util              (listJson)
import           System.Wlog                (WithLogger, logDebug)

import           Pos.Block.BListener        (MonadBListener (..))
import           Pos.Block.Core             (mainBlockTxPayload)
import           Pos.Block.Types            (Blund, undoTx)
import           Pos.Core                   (HeaderHash, headerHash, prevBlockL)
import           Pos.DB.Class               (MonadRealDB, MonadDBRead)
import           Pos.DB.BatchOp             (SomeBatchOp)
import           Pos.Ssc.Class.Helpers      (SscHelpersClass)
import           Pos.Txp.Core               (TxAux, TxUndo, flattenTxPayload)
import           Pos.Txp.Toil               (evalToilTEmpty, runDBToil)
import           Pos.Util.Chrono            (NE, NewestFirst (..), OldestFirst (..))
import qualified Pos.Util.Modifier          as MM

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
    , WithLogger m
    , MonadRealDB ctx m
    , MonadDBRead m
    )
    => OldestFirst NE (Blund ssc) -> m SomeBatchOp
onApplyTracking blunds = do
    let txs = concatMap (gbTxs . fst) $ getOldestFirst blunds
    let newTip = headerHash $ NE.last $ getOldestFirst blunds
    mapM_ (syncWalletSet newTip txs) =<< WS.getWalletAddresses
    
    -- It's silly, but when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where
    syncWalletSet :: HeaderHash -> [TxAux] -> CId Wal -> m ()
    syncWalletSet newTip txs wAddr = do
        allAddresses <- getWalletAddrMetasDB WS.Ever wAddr
        encSK <- getSKByAddr wAddr
        mapModifier <- runDBToil $
                       evalToilTEmpty $
                       trackingApplyTxs encSK allAddresses $
                       zip txs (repeat newTip)
        applyModifierToWallet wAddr newTip mapModifier
        logMsg "applied" (getOldestFirst blunds) wAddr mapModifier
    gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)

-- Perform this action under block lock.
onRollbackTracking
    :: forall ssc ctx m .
    ( SscHelpersClass ssc
    , AccountMode ctx m
    , WithLogger m
    )
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
onRollbackTracking blunds = do
    let txs = concatMap (reverse . blundTxUn) $ getNewestFirst blunds
    let newTip = (NE.last $ getNewestFirst blunds) ^. prevBlockL
    mapM_ (syncWalletSet newTip txs) =<< WS.getWalletAddresses

    -- It's silly, but when the wallet is migrated to RocksDB, we can write
    -- something a bit more reasonable.
    pure mempty
  where
    syncWalletSet :: HeaderHash -> [(TxAux, TxUndo)] -> CId Wal -> m ()
    syncWalletSet newTip txs wAddr = do
        allAddresses <- getWalletAddrMetasDB WS.Ever wAddr
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
logMsg action (NE.length -> bNums) wAddr CAccModifier{..} =
    logDebug $
        sformat ("Wallet Tracking: "%build%" "%build%" block(s) to walletset "%build
                %", added accounts: "%listJson
                %", deleted accounts: "%listJson
                %", used address: "%listJson
                %", change address: "%listJson)
        action bNums wAddr
        (map fst $ MM.insertions camAddresses)
        (MM.deletions camAddresses)
        (map (fst . fst) $ MM.insertions camUsed)
        (map (fst . fst) $ MM.insertions camChange)
