{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions utils.

module Pos.Wallet.Web.Pending.Util
    ( ptxPoolInfo
    , isPtxInBlocks
    , mkPendingTx
    , isReclaimableFailure
    ) where

import           Universum

import           Formatting                   (build, sformat, (%))

import           Pos.Client.Txp.History       (TxHistoryEntry)
import           Pos.Slotting.Class           (getCurrentSlotInaccurate)
import           Pos.Txp                      (ToilVerFailure (..), TxAux (..), TxId)
import           Pos.Util.Util                (maybeThrow)
import           Pos.Wallet.Web.ClientTypes   (CId, CWalletMeta (..), Wal, cwAssurance)
import           Pos.Wallet.Web.Error         (WalletError (RequestError))
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..),
                                               PtxPoolInfo)
import           Pos.Wallet.Web.State         (getWalletMeta)

ptxPoolInfo :: PtxCondition -> Maybe PtxPoolInfo
ptxPoolInfo (PtxApplying i)    = Just i
ptxPoolInfo (PtxWontApply _ i) = Just i
ptxPoolInfo _                  = Nothing

isPtxInBlocks :: PtxCondition -> Bool
isPtxInBlocks = isNothing . ptxPoolInfo

mkPendingTx
    :: MonadWalletWebMode m
    => CId Wal -> TxId -> TxAux -> TxHistoryEntry -> m PendingTx
mkPendingTx wid _ptxTxId _ptxTxAux th = do
    _ptxCreationSlot <- getCurrentSlotInaccurate
    CWalletMeta{..} <- maybeThrow noWallet =<< getWalletMeta wid
    return PendingTx
        { _ptxCond = PtxApplying th
        , _ptxWallet = wid
        , ..
        }
  where
    noWallet =
        RequestError $ sformat ("Failed to get meta of wallet "%build) wid

isReclaimableFailure :: ToilVerFailure -> Bool
isReclaimableFailure = \case
    -- If number of 'ToilVerFailure' constructors will ever change, compiler
    -- will complain - for this purpose we consider all cases explicitly here.
    ToilKnown                -> True
    ToilTipsMismatch{}       -> True
    ToilSlotUnknown          -> True
    ToilOverwhelmed{}        -> True
    ToilNotUnspent{}         -> False
    ToilOutGTIn{}            -> False
    ToilInconsistentTxAux{}  -> False
    ToilInvalidOutputs{}     -> False
    ToilUnknownInput{}       -> False
    ToilWitnessDoesntMatch{} -> False
    ToilInvalidWitness{}     -> False
    ToilTooLargeTx{}         -> False
    ToilInvalidMinFee{}      -> False
    ToilInsufficientFee{}    -> False
    ToilUnknownAttributes{}  -> False
    ToilBootInappropriate{}  -> False
    ToilRepeatedInput{}      -> False

