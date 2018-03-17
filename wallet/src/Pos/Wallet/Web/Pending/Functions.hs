{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions utils.

module Pos.Wallet.Web.Pending.Functions
    ( ptxPoolInfo
    , isPtxActive
    , isPtxInBlocks
    , mkPendingTx
    , isReclaimableFailure
    , usingPtxCoords
    ) where

import           Universum

import           Formatting (build, sformat, (%))

import           Pos.Core (HasConfiguration)
import           Pos.Client.Txp.History (SaveTxException (..), TxHistoryEntry)
import           Pos.Core.Txp (TxAux (..), TxId)
import           Pos.Slotting.Class (MonadSlots (..))
import           Pos.Txp (ToilVerFailure (..))
import           Pos.Util.Util (maybeThrow)
import           Pos.Wallet.Web.ClientTypes (CId, Wal)
import           Pos.Wallet.Web.Error (WalletError (RequestError))
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..), PtxPoolInfo)
import           Pos.Wallet.Web.Pending.Util (mkPtxSubmitTiming)
import           Pos.Wallet.Web.State (WalletSnapshot, getWalletMeta)

ptxPoolInfo :: PtxCondition -> Maybe PtxPoolInfo
ptxPoolInfo (PtxCreating i)     = Just i
ptxPoolInfo (PtxApplying i)     = Just i
ptxPoolInfo (PtxWontApply _ i)  = Just i
ptxPoolInfo PtxInNewestBlocks{} = Nothing
ptxPoolInfo PtxPersisted{}      = Nothing

-- | Whether transaction is claimed to be once created.
isPtxActive :: PtxCondition -> Bool
isPtxActive PtxCreating{} = False
isPtxActive _             = True

isPtxInBlocks :: PtxCondition -> Bool
isPtxInBlocks = isNothing . ptxPoolInfo

mkPendingTx
    :: (HasConfiguration, MonadThrow m, MonadIO m, MonadSlots ctx m)
    => WalletSnapshot
    -> CId Wal -> TxId -> TxAux -> TxHistoryEntry -> m PendingTx
mkPendingTx ws wid _ptxTxId _ptxTxAux th = do
    void $ maybeThrow noWallet $ getWalletMeta ws wid

    _ptxCreationSlot <- getCurrentSlotInaccurate
    return PendingTx
        { _ptxCond = PtxCreating th
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
isReclaimableFailure :: SaveTxException -> Bool
isReclaimableFailure (SaveTxToilFailure tvf) = case tvf of
    -- We consider all cases explicitly here to prevent changing
    -- constructors set blindly
    ToilKnown                -> True
    ToilTipsMismatch{}       -> True
    ToilSlotUnknown          -> True
    ToilOverwhelmed{}        -> True
    ToilNotUnspent{}         -> False
    ToilOutGreaterThanIn{}   -> False
    ToilInconsistentTxAux{}  -> False
    ToilInvalidOutput{}     -> False
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
