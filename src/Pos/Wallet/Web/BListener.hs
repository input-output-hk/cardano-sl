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
import           Serokell.Util              (listJson)
import           System.Wlog                (WithLogger, logDebug)

import           Pos.Block.BListener        (MonadBListener (..))
import           Pos.Block.Core             (mainBlockTxPayload)
import           Pos.Block.Types            (Blund, undoTx)
import           Pos.Core                   (HeaderHash, headerHash, prevBlockL)
import           Pos.DB.Class               (MonadDBRead)
import           Pos.DB.Rocks               (MonadRealDB)
import           Pos.Ssc.Class.Helpers      (SscHelpersClass)
import           Pos.Txp.Core               (TxAux, TxUndo, flattenTxPayload)
import           Pos.Txp.Toil               (evalToilTEmpty, runDBToil)
import           Pos.Util.Chrono            (NE, NewestFirst (..), OldestFirst (..))
import qualified Pos.Util.Modifier          as MM

import           Pos.Wallet.KeyStorage      (MonadKeys)
import           Pos.Wallet.Web.Account     (AccountMode, getSKByAddr)
import           Pos.Wallet.Web.ClientTypes (CId, Wal)
import           Pos.Wallet.Web.State       (WalletWebDB)
import qualified Pos.Wallet.Web.State       as WS
import           Pos.Wallet.Web.Tracking    (CAccModifier, applyModifierToWSet,
                                             trackingApplyTxs, trackingRollbackTxs)

instance ( MonadRealDB m
         , MonadDBRead m
         , MonadMockable m
         , MonadKeys m
         , WithLogger m
         )
         => MonadBListener (WalletWebDB m) where
    onApplyBlocks = onApplyTracking
    onRollbackBlocks = onRollbackTracking

-- Perform this action under block lock.
onApplyTracking
    :: forall ssc m .
    ( SscHelpersClass ssc
    , AccountMode m
    , WithLogger m
    , MonadRealDB m
    , MonadDBRead m
    )
    => OldestFirst NE (Blund ssc) -> m ()
onApplyTracking blunds = do
    let txs = concatMap (gbTxs . fst) $ getOldestFirst blunds
    let newTip = headerHash $ NE.last $ getOldestFirst blunds
    mapM_ (syncWalletSet newTip txs) =<< WS.getWalletAddresses
  where
    syncWalletSet :: HeaderHash -> [TxAux] -> CId Wal -> m ()
    syncWalletSet newTip txs wAddr = do
        encSK <- getSKByAddr wAddr
        mapModifier <- runDBToil $ evalToilTEmpty $ trackingApplyTxs encSK txs
        applyModifierToWSet wAddr newTip mapModifier
        logMsg "applied" (getOldestFirst blunds) wAddr mapModifier
    gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)

-- Perform this action under block lock.
onRollbackTracking
    :: forall ssc m .
    ( SscHelpersClass ssc
    , AccountMode m
    , WithLogger m
    )
    => NewestFirst NE (Blund ssc) -> m ()
onRollbackTracking blunds = do
    let txs = concatMap (reverse . blundTxUn) $ getNewestFirst blunds
    let newTip = (NE.last $ getNewestFirst blunds) ^. prevBlockL
    mapM_ (syncWalletSet newTip txs) =<< WS.getWalletAddresses
  where
    syncWalletSet :: HeaderHash -> [(TxAux, TxUndo)] -> CId Wal -> m ()
    syncWalletSet newTip txs wAddr = do
        encSK <- getSKByAddr wAddr
        let mapModifier = trackingRollbackTxs encSK txs
        applyModifierToWSet wAddr newTip mapModifier
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
logMsg action (NE.length -> bNums) wAddr mm =
    logDebug $
        sformat ("Wallet Tracking: "%build%" "%build%" block(s) to walletset "%build
                %", added accounts: "%listJson
                %", deleted accounts: "%listJson)
        action bNums wAddr (map fst $ MM.insertions mm) (MM.deletions mm)
