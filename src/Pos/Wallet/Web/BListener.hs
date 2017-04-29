{-# LANGUAGE ScopedTypeVariables #-}

-- | Instance Blockchain Listener for WalletWebDB.

module Pos.Wallet.Web.BListener
       ( -- BListener instance.
       ) where

import           Mockable                   (MonadMockable)
import           Universum

import qualified Data.List.NonEmpty         as NE
import           Pos.Block.BListener        (MonadBListener (..))
import           Pos.Block.Types            (Blund)
import           Pos.Block.Types            (undoTx)
import           Pos.DB.Class               (MonadDB)
import           Pos.Ssc.Class.Helpers      (SscHelpersClass)
import           Pos.Txp.Core               (TxAux, TxUndo)
import           Pos.Txp.Toil               (evalToilTEmpty, runDBTxp)
import           Pos.Types                  (HeaderHash, blockTxas, headerHash,
                                             prevBlockL)
import           Pos.Util.Chrono            (NE, NewestFirst (..), OldestFirst (..))

import           Pos.Wallet.Web.Account     (AccountMode, getSKByAddr)
import           Pos.Wallet.Web.ClientTypes (CAddress, WS)
import           Pos.Wallet.Web.State       (WalletWebDB)
import qualified Pos.Wallet.Web.State       as WS
import           Pos.Wallet.Web.Tracking    (applyModifierToWSet, trackingApplyTxs,
                                             trackingRollbackTxs)

instance ( MonadDB m
         , MonadMockable m
         , AccountMode m
         )
         => MonadBListener (WalletWebDB m) where
    onApplyBlocks = onApplyTracking
    onRollbackBlocks = onRollbackTracking

onApplyTracking
    :: forall ssc m .
    ( SscHelpersClass ssc
    , AccountMode m
    , MonadDB m)
    => OldestFirst NE (Blund ssc) -> m ()
onApplyTracking blunds = do
    let txs = concatMap (gbTxs . fst) $ getOldestFirst blunds
    let newTip = headerHash $ NE.last $ getOldestFirst blunds
    mapM_ (syncWalletSet newTip txs) =<< WS.getWSetAddresses
  where
    syncWalletSet :: HeaderHash -> [TxAux] -> CAddress WS -> m ()
    syncWalletSet newTip txs wsAddr = do
        encSK <- getSKByAddr wsAddr
        mapModifier <- runDBTxp $ evalToilTEmpty $ trackingApplyTxs encSK txs
        applyModifierToWSet wsAddr newTip mapModifier
    gbTxs = either (const []) (^. blockTxas)

onRollbackTracking
    :: forall ssc m .
    ( SscHelpersClass ssc
    , AccountMode m
    )
    => NewestFirst NE (Blund ssc) -> m ()
onRollbackTracking blunds = do
    let txs = concatMap (reverse . blundTxUn) $ getNewestFirst blunds
    let newTip = (NE.last $ getNewestFirst blunds) ^. prevBlockL
    mapM_ (syncWalletSet newTip txs) =<< WS.getWSetAddresses
  where
    syncWalletSet :: HeaderHash -> [(TxAux, TxUndo)] -> CAddress WS -> m ()
    syncWalletSet newTip txs wsAddr = do
        encSK <- getSKByAddr wsAddr
        let mapModifier = trackingRollbackTxs encSK txs
        applyModifierToWSet wsAddr newTip mapModifier
    gbTxs = either (const []) (^. blockTxas)
    blundTxUn (b, u) = zip (gbTxs b) (undoTx u)
