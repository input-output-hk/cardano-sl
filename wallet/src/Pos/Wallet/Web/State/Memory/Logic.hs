-- | Logic of updating in-memory state.

module Pos.Wallet.Web.State.Memory.Logic
       ( updateStorageModifierOnTx
       , buildStorageModifier
       , TxpMempoolToModifierEnv
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as HM
import           Ether.Internal (HasLens (..))
import           System.Wlog (WithLogger, logError, logWarning)

import           Formatting (build, sformat, (%))
import           Pos.Core (HeaderHash)
import           Pos.Crypto (EncryptedSecretKey, WithHash (..))
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead)
import           Pos.Txp (MonadTxpMem, TxAux (..), TxId, TxUndo, askTxpMem, getLocalTxsNUndo,
                          topsortTxs, txpTip)
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Account (AccountMode, getSKById)
import           Pos.Wallet.Web.ClientTypes (Addr, CId)
import           Pos.Wallet.Web.Error (WalletError (..))
import qualified Pos.Wallet.Web.State as WS
import           Pos.Wallet.Web.State.Memory.Types (ExtStorageModifier (..), ExtStorageModifierVar,
                                                    HasExtStorageModifier)
import qualified Pos.Wallet.Web.State.Memory.Types as SM
import           Pos.Wallet.Web.Tracking (WalletModifier, trackingApplyTxToModifierM,
                                          trackingApplyTxs)

updateStorageModifierOnTx
    :: ( HasExtStorageModifier ctx
       , AccountMode m
       , MonadDBRead m
       , MonadReader ctx m
       , WS.MonadWalletDBRead m
       )
    => (TxId, TxAux, TxUndo) -> m ()
updateStorageModifierOnTx (_, txAux, txUndo) = do
    walIds <- WS.getWalletIds
    walMods <- forM walIds $ \wal -> do
        dbUsed <- WS.getCustomAddressesDB WS.UsedAddr
        newMod <- trackingApplyTxToModifierM wal dbUsed mempty (txAux, txUndo)
        pure (wal, newMod)
    walletsVar <- view (lensOf @ExtStorageModifierVar)
    atomically $ do
        ExtStorageModifier{..} <- STM.readTMVar walletsVar
        void $ STM.swapTMVar walletsVar $
            ExtStorageModifier esmTip (foldr SM.applyWalModifier esmMemStorageModifier walMods)

buildStorageModifier
    :: ( TxpMempoolToModifierEnv ctx m
       , AccountMode m
       )
    => m ExtStorageModifier
buildStorageModifier = do
    memTip <- atomically . readTVar =<< (txpTip <$> askTxpMem)
    walIds <- WS.getWalletIds
    walMods <- forM walIds $ \wal -> do
        encSK <- getSKById wal
        dbUsed <- WS.getCustomAddressesDB WS.UsedAddr
        newMod <- txpMempoolToWalModifier encSK dbUsed
        pure (wal, newMod)
    pure $ ExtStorageModifier memTip (foldr SM.applyWalModifier mempty walMods)

type TxpMempoolToModifierEnv ctx m =
     ( MonadTxpMem WalletMempoolExt ctx m
     , WithLogger m
     , MonadIO m
     , MonadDBRead m
     )

txpMempoolToWalModifier
    :: TxpMempoolToModifierEnv ctx m
    => EncryptedSecretKey
    -> [(CId Addr, HeaderHash)]
    -> m WalletModifier
txpMempoolToWalModifier encSK dbUsed = do
    let wHash (i, TxAux {..}, _) = WithHash taTx i
        fInfo = \_ -> (Nothing, Nothing, Nothing) -- no difficulty, no timestamp, no slot for mempool
    (txs, undoMap) <- getLocalTxsNUndo

    txsWUndo <- forM txs $ \(id, tx) -> case HM.lookup id undoMap of
        Just undo -> pure (id, tx, undo)
        Nothing -> do
            let errMsg = sformat ("There is no undo corresponding to TxId #"%build%" from txp mempool") id
            logError errMsg
            throwM $ InternalError errMsg

    tipH <- DB.getTipHeader
    case topsortTxs wHash txsWUndo of
        Nothing      -> mempty <$ logWarning "txMempoolToModifier: couldn't topsort mempool txs"
        Just ordered ->
            pure $
                trackingApplyTxs encSK dbUsed fInfo
                    (map (\(_, tx, undo) -> (tx, undo, tipH)) ordered)
