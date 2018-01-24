{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions utils.

module Pos.Wallet.Web.Pending.Util
    ( ptxPoolInfo
    , isPtxInBlocks
    , sortPtxsChrono
    , mkPendingTx
    , isReclaimableFailure
    , usingPtxCoords
    , allPendingAddresses
    , nonConfirmedTransactions
    ) where

import           Universum

import qualified Data.Set                       as Set
import           Formatting                     (build, sformat, (%))

import           Pos.Client.Txp.History         (TxHistoryEntry)
import           Pos.Client.Txp.Util            (PendingAddresses (..))
import           Pos.Core.Types                 (Address)
import           Pos.Crypto                     (WithHash (..))
import           Pos.Slotting.Class             (getCurrentSlotInaccurate)
import           Pos.Txp                        (ToilVerFailure (..), Tx (..), TxAux (..),
                                                 TxId, TxOut(..), topsortTxs)
import           Pos.Util.Chrono                (OldestFirst (..))
import           Pos.Util.Util                  (maybeThrow)

import           Pos.Wallet.Web.ClientTypes     (CId, CWalletMeta (..), Wal, cwAssurance)
import           Pos.Wallet.Web.Error           (WalletError (RequestError))
import           Pos.Wallet.Web.Mode            (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Types   (PendingTx (..), PtxCondition (..),
                                                 PtxPoolInfo)
import           Pos.Wallet.Web.Pending.Updates (mkPtxSubmitTiming)
import           Pos.Wallet.Web.State           (WalletSnapshot, getWalletMeta)

ptxPoolInfo :: PtxCondition -> Maybe PtxPoolInfo
ptxPoolInfo (PtxApplying i)    = Just i
ptxPoolInfo (PtxWontApply _ i) = Just i
ptxPoolInfo _                  = Nothing

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
    => WalletSnapshot
    -> CId Wal -> TxId -> TxAux -> TxHistoryEntry -> m PendingTx
mkPendingTx ws wid _ptxTxId _ptxTxAux th = do
    _ptxCreationSlot <- getCurrentSlotInaccurate
    CWalletMeta{..} <- maybeThrow noWallet (getWalletMeta ws wid)
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

-- | Returns the full list of "pending addresses", which are @output@ addresses
-- associated to transactions not yet persisted in the blockchain.
allPendingAddresses :: [PendingTx] -> PendingAddresses
allPendingAddresses =
    PendingAddresses . Set.unions . map grabTxOutputs . nonConfirmedTransactions
  where
    grabTxOutputs :: PendingTx -> Set.Set Address
    grabTxOutputs PendingTx{..} =
        let (TxAux tx _) = _ptxTxAux
            (UnsafeTx _ outputs _) = tx
            in Set.fromList $ map (\(TxOut a _) -> a) (toList outputs)

-- | Filters the input '[PendingTx]' to choose only the ones which are not
-- yet persisted in the blockchain.
nonConfirmedTransactions :: [PendingTx] -> [PendingTx]
nonConfirmedTransactions = filter isPending
  where
    -- | Is this 'PendingTx' really pending?
    isPending :: PendingTx -> Bool
    isPending PendingTx{..} = case _ptxCond of
        PtxInNewestBlocks _ -> False
        PtxPersisted        -> False
        _                   -> True
