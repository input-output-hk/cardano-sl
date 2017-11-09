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
    walIds <- WS.getWalletAddresses
    walMods <- forM walIds $ \wal -> do
        -- TODO pass not only db addresses
        allAddresses <- getWalletAddrMetas WS.Ever wal
        newMod <- trackingApplyTxToModifierM wal allAddresses mempty (txAux, txUndo)
        pure (wal, newMod)
    walletsVar <- view (lensOf @ExtStorageModifierVar)
    atomically $ STM.modifyTVar walletsVar $
        \e -> e {esmMemStorageModifier = foldr SM.applyWalModifier (esmMemStorageModifier e) walMods}

buildStorageModifier
    :: ( WalletTrackingMempoolEnv ctx m
       , AccountMode ctx m
       )
    => m ExtStorageModifier
buildStorageModifier = do
    memTip <- atomically . readTVar =<< (txpTip <$> askTxpMem)
    walIds <- WS.getWalletAddresses
    walMods <- forM walIds $ \wal -> do
        encSK <- getSKById wal
        newMod <- txMempoolToModifier encSK
        pure (wal, newMod)
    pure $ ExtStorageModifier memTip (foldr SM.applyWalModifier mempty walMods)
