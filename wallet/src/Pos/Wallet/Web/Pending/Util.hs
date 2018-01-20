{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions utils.

module Pos.Wallet.Web.Pending.Util
    ( ptxPoolInfo
    , isPtxActive
    , isPtxInBlocks
    , sortPtxsChrono
    , mkPendingTx
    , isReclaimableFailure
    , usingPtxCoords
    ) where

import           Universum

import           Formatting                     (build, sformat, (%))

import           Pos.Client.Txp.History         (TxHistoryEntry)
import           Pos.Crypto                     (WithHash (..))
import           Pos.Slotting.Class             (getCurrentSlotInaccurate)
import           Pos.Txp                        (ToilVerFailure (..), TxAux (..), TxId,
                                                 topsortTxs)
import           Pos.Util.Chrono                (OldestFirst (..))
import           Pos.Util.Util                  (maybeThrow)
import           Pos.Wallet.Web.ClientTypes     (CId, CWalletMeta (..), Wal, cwAssurance)
import           Pos.Wallet.Web.Error           (WalletError (RequestError))
import           Pos.Wallet.Web.Mode            (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Types   (PendingTx (..), PtxCondition (..),
                                                 PtxPoolInfo)
import           Pos.Wallet.Web.Pending.Updates (mkPtxSubmitTiming)
import           Pos.Wallet.Web.State           (getWalletMeta)

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

-- | Sort pending transactions as close as possible to chronological order.
sortPtxsChrono :: [PendingTx] -> OldestFirst [] PendingTx
sortPtxsChrono = OldestFirst . sortWith _ptxCreationSlot . tryTopsort
  where
    tryTopsort txs = fromMaybe txs $ topsortTxs wHash txs
    wHash PendingTx{..} = WithHash (taTx _ptxTxAux) _ptxTxId

mkPendingTx
    :: MonadWalletWebMode m
    => CId Wal -> TxId -> TxAux -> TxHistoryEntry -> m PendingTx
mkPendingTx wid _ptxTxId _ptxTxAux th = do
    _ptxCreationSlot <- getCurrentSlotInaccurate
    CWalletMeta{..} <- maybeThrow noWallet =<< getWalletMeta wid
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

-- | Whether formed transaction ('TxAux') has a chance to be applied later
-- after specified error.
isReclaimableFailure :: ToilVerFailure -> Bool
isReclaimableFailure = \case
    -- We consider all cases explicitly here to prevent changing
    -- constructors set blindly
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
    ToilNonBootstrapDistr{}  -> False
    ToilRepeatedInput{}      -> False

usingPtxCoords :: (CId Wal -> TxId -> a) -> PendingTx -> a
usingPtxCoords f PendingTx{..} = f _ptxWallet _ptxTxId
