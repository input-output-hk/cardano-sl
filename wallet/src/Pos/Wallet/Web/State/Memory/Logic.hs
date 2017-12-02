-- | Logic of updating in-memory state.

module Pos.Wallet.Web.State.Memory.Logic
       ( updateStorageModifierOnTx
       , buildStorageModifier
       ) where

import           Universum

import qualified Control.Concurrent.STM            as STM
import           Ether.Internal                    (HasLens (..))

import           Pos.DB.Block                      (MonadBlockDB)
import           Pos.Txp                           (TxAux (..), TxId, TxUndo, askTxpMem,
                                                    txpTip)
import           Pos.Wallet.Web.Account            (AccountMode, getSKById)
import qualified Pos.Wallet.Web.State              as WS
import           Pos.Wallet.Web.State.Memory.Types (ExtStorageModifier (..),
                                                    ExtStorageModifierVar,
                                                    HasExtStorageModifier)
import qualified Pos.Wallet.Web.State.Memory.Types as SM
import           Pos.Wallet.Web.Tracking           (WalletTrackingMempoolEnv,
                                                    trackingApplyTxToModifierM,
                                                    txMempoolToModifier)
import           Pos.Wallet.Web.Util               (getWalletAddrMetas)

updateStorageModifierOnTx
    :: ( HasExtStorageModifier ctx
       , AccountMode ctx m
       , MonadBlockDB m
       )
    => (TxId, TxAux, TxUndo) -> m ()
updateStorageModifierOnTx (_, txAux, txUndo) = do
    walIds <- WS.getWalletIds
    walMods <- forM walIds $ \wal -> do
        allAddresses <- getWalletWAddresses WS.Ever wal
        newMod <- trackingApplyTxToModifierM wal allAddresses mempty (txAux, txUndo)
        pure (wal, newMod)
    walletsVar <- view (lensOf @ExtStorageModifierVar)
    atomically $ STM.modifyTVar walletsVar $
        \e -> e {esmMemStorageModifier = foldr SM.applyWalModifier (esmMemStorageModifier e) walMods}

buildStorageModifier
    :: ( TxpMempoolToModifierEnv ctx m
       , AccountMode ctx m
       )
    => m ExtStorageModifier
buildStorageModifier = do
    memTip <- atomically . readTVar =<< (txpTip <$> askTxpMem)
    walIds <- WS.getWalletAddresses
    walMods <- forM walIds $ \wal -> do
        encSK <- getSKById wal
        newMod <- txpMempoolToWalModifier encSK
        pure (wal, newMod)
    pure $ ExtStorageModifier memTip (foldr SM.applyWalModifier mempty walMods)

type TxpMempoolToModifierEnv ctx m =
     ( BlockLockMode ctx m
     , MonadTxpMem WalletMempoolExt ctx m
     , WS.MonadWalletDBRead ctx m
     , MonadSlotsData ctx m
     , WithLogger m
     , HasConfiguration
     )

txpMempoolToWalModifier :: TxpMempoolToModifierEnv ctx m => EncryptedSecretKey -> m WalletModifier
txpMempoolToWalModifier encSK = do
    let wHash (i, TxAux {..}, _) = WithHash taTx i
        wId = encToCId encSK
        fInfo = \_ -> (Nothing, Nothing, Nothing) -- no difficulty, no timestamp, no slot for mempool
    (txs, undoMap) <- getLocalTxsNUndo

    txsWUndo <- forM txs $ \(id, tx) -> case HM.lookup id undoMap of
        Just undo -> pure (id, tx, undo)
        Nothing -> do
            let errMsg = sformat ("There is no undo corresponding to TxId #"%build%" from txp mempool") id
            logError errMsg
            throwM $ InternalError errMsg

    tipH <- DB.getTipHeader
    allAddressesDB <- getWalletWAddressesDB WS.Ever wId
    case topsortTxs wHash txsWUndo of
        Nothing      -> mempty <$ logWarning "txMempoolToModifier: couldn't topsort mempool txs"
        Just ordered ->
            pure $
                trackingApplyTxs encSK allAddressesDB fInfo
                    (map (\(_, tx, undo) -> (tx, undo, tipH)) ordered)
