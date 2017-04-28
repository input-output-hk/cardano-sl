-- | Instance Blockchain Listener for WalletWebDB.

module Pos.Wallet.Web.BListener () where

import           Mockable              (MonadMockable)
import           Universum

import           Pos.Block.BListener   (MonadBListener (..))
import           Pos.Block.Types       (Blund)
import           Pos.Ssc.Class.Helpers (SscHelpersClass)
import           Pos.Util.Chrono       (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Wallet.Web.State  (WalletWebDB, WebWalletModeDB)

instance (MonadIO m, MonadMockable m) => MonadBListener (WalletWebDB m) where
    onApplyBlocks = onApplyTracking
    onRollbackBlocks = onRollbackTracking

onApplyTracking
    :: (SscHelpersClass ssc, WebWalletModeDB m)
    => OldestFirst NE (Blund ssc) -> m ()
onApplyTracking blunds = do
    !() <- traceM "ON APPLY TRACKING"
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
    :: (SscHelpersClass ssc, WebWalletModeDB m)
    => NewestFirst NE (Blund ssc) -> m ()
onRollbackTracking _ = pass
