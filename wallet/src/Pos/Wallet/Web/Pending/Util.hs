{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions utils.

module Pos.Wallet.Web.Pending.Util
    ( isPtxInBlocks
    , mkPendingTx
    ) where

import           Universum

import           Formatting                   (build, sformat, (%))
import           Pos.Slotting.Class           (getCurrentSlotInaccurate)
import           Pos.Txp                      (TxAux, TxId)
import           Pos.Util.Util                (maybeThrow)
import           Pos.Wallet.Web.Assurance     (AssuranceLevel (HighAssurance),
                                               assuredBlockDepth)
import           Pos.Wallet.Web.ClientTypes   (CId, CWalletMeta (..), Wal, cwAssurance)
import           Pos.Wallet.Web.Error         (WalletError (RequestError))
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..))
import           Pos.Wallet.Web.State         (getWalletMeta)

isPtxInBlocks :: PtxCondition -> Bool
isPtxInBlocks = \case
    PtxApplying{}      -> False
    PtxInUpperBlocks{} -> True
    PtxPersisted{}     -> True
    PtxWontApply{}    -> False

mkPendingTx :: MonadWalletWebMode m => CId Wal -> TxId -> TxAux -> m PendingTx
mkPendingTx wid ptxTxId ptxTxAux = do
    ptxCreationSlot <- getCurrentSlotInaccurate
    CWalletMeta{..} <- maybeThrow noWallet =<< getWalletMeta wid
    return PendingTx
        { ptxCond = PtxApplying
        , ptxAssuredDepth = assuredBlockDepth cwAssurance HighAssurance
        , ..
        }
  where
    noWallet =
        RequestError $ sformat ("Failed to get meta of wallet "%build) wid

