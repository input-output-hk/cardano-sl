{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions utils.

module Pos.Wallet.Web.Pending.Util
    ( isPtxInBlocks
    , mkPendingTx
    , isReclaimableFailure
    ) where

import           Universum

import           Formatting                   (build, sformat, (%))

import qualified Pos.Constants                as C
import           Pos.Slotting.Class           (getCurrentSlotInaccurate)
import           Pos.Txp                      (TxAux, TxId)
import           Pos.Txp                      (ToilVerFailure (..))
import           Pos.Util.Util                (maybeThrow)
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
    PtxWontApply{}     -> False

mkPendingTx :: MonadWalletWebMode m => CId Wal -> TxId -> TxAux -> m PendingTx
mkPendingTx wid _ptxTxId _ptxTxAux = do
    _ptxCreationSlot <- getCurrentSlotInaccurate
    CWalletMeta{..} <- maybeThrow noWallet =<< getWalletMeta wid
    return PendingTx
        { _ptxCond = PtxApplying
        , _ptxWallet = wid
        , _ptxAttemptsRem = C.pendingTxAttemptsLimit
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
    ToilInvalidInput{}       -> False
    ToilWitnessDoesntMatch{} -> False
    ToilTooLargeTx{}         -> False
    ToilInvalidMinFee{}      -> False
    ToilInsufficientFee{}    -> False
    ToilUnknownAttributes{}  -> False
    ToilBootInappropriate{}  -> False
    ToilRepeatedInput{}      -> False

