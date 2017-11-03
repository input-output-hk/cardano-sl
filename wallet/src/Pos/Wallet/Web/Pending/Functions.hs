{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions utils.

module Pos.Wallet.Web.Pending.Functions
    ( ptxPoolInfo
    , isPtxInBlocks
    , mkPendingTx
    , isReclaimableFailure
    , usingPtxCoords
    ) where

import           Universum

import           Formatting                   (build, sformat, (%))

import           Pos.Client.Txp.History       (TxHistoryEntry)
import           Pos.Slotting.Class           (MonadSlots (..))
import           Pos.Txp                      (ToilVerFailure (..), TxAux (..), TxId)
import           Pos.Util.Util                (maybeThrow)
import           Pos.Wallet.Web.ClientTypes   (CId, Wal)
import           Pos.Wallet.Web.Error         (WalletError (RequestError))
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..),
                                               PtxPoolInfo)
import           Pos.Wallet.Web.Pending.Util  (mkPtxSubmitTiming)
import           Pos.Wallet.Web.State         (MonadWalletDBRead, getWalletMeta)

ptxPoolInfo :: PtxCondition -> Maybe PtxPoolInfo
ptxPoolInfo (PtxApplying i)    = Just i
ptxPoolInfo (PtxWontApply _ i) = Just i
ptxPoolInfo _                  = Nothing

isPtxInBlocks :: PtxCondition -> Bool
isPtxInBlocks = isNothing . ptxPoolInfo

mkPendingTx
    :: (MonadThrow m, MonadIO m, MonadWalletDBRead ctx m, MonadSlots ctx m)
    => CId Wal -> TxId -> TxAux -> TxHistoryEntry -> m PendingTx
mkPendingTx wid _ptxTxId _ptxTxAux th = do
    void $ maybeThrow noWallet =<< getWalletMeta wid

    _ptxCreationSlot <- getCurrentSlotInaccurate
    return PendingTx
        { _ptxCond = PtxApplying th
        , _ptxWallet = wid
        , _ptxPeerAck = False
        , _ptxSubmitTiming = mkPtxSubmitTiming _ptxCreationSlot
        , ..
        }
  where
    noWallet =
        RequestError $ sformat ("Failed to get meta of wallet "%build) wid

-- | Whether formed transaction ('TxAux') has reasonable chances to be applied
-- later after specified error.
isReclaimableFailure :: ToilVerFailure -> Bool
isReclaimableFailure = \case
    -- We consider all cases explicitly here to prevent changing
    -- constructors set blindly
    ToilKnown                -> True
    ToilTipsMismatch{}       -> True
    ToilSlotUnknown          -> True
    ToilOverwhelmed{}        -> True
    ToilNotUnspent{}         -> False
    ToilOutGreaterThanIn{}   -> False
    ToilInconsistentTxAux{}  -> False
    ToilInvalidOutputs{}     -> False
    ToilUnknownInput{}       -> False
    ToilWitnessDoesntMatch{} -> False
    ToilInvalidWitness{}     -> False
    ToilTooLargeTx{}         -> False
    ToilInvalidMinFee{}      -> False
    ToilInsufficientFee{}    -> False
    ToilUnknownAttributes{}  -> False
    ToilNonBootstrapDistr{}  -> False
    ToilRepeatedInput{}      -> False

usingPtxCoords :: (CId Wal -> TxId -> a) -> PendingTx -> a
usingPtxCoords f PendingTx{..} = f _ptxWallet _ptxTxId
