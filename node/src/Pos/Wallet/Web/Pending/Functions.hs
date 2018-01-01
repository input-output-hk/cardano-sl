module Pos.Wallet.Web.Pending.Functions
    ( reevaluateApplyingPtxs
    ) where

import           Universum

import qualified Data.HashMap.Strict          as HM
import           Control.Lens                 (has, each)

import           Pos.Crypto                   (withHash, hash)
import           Pos.Core.Configuration       (HasConfiguration)
import           Pos.Core.Slotting            (flatSlotId)
import           Pos.Core.Types               (ChainDifficulty, FlatSlotId, SlotId)
import           Pos.Txp.Core                 (TxId, topsortTxs, taTx)
import           Pos.Txp.Toil.Class           (MonadUtxoRead)
import           Pos.Txp.Toil.Trans           (evalToilTEmpty)
import           Pos.Txp.Toil.Failure         (ToilVerFailure)
import           Pos.Txp.Toil.Utxo.Functions  (VTxContext (..), verifyTxUtxo, applyTxToUtxo)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..),
                                               PtxSubmitTiming (..), pstNextDelay,
                                               pstNextSlot, ptxPeerAck, ptxSubmitTiming,
                                               ptxCond, _PtxApplying, ptxTxAux, ptxTxId)


-- | Reevaluate all pending txs (in 'PtxApplying' state):
--
--   * 'PtxWontApply' for ones that can't be applied
--   * 'PtxInNewestBlocks' for ones that are already
--     in the blockchain (at any depth)
--   * 'PtxApplying' to ones that can be applied either directly or after
--     applying some other transactions
--
-- TODO: do we need to do anything special to ensure that the UTXO we're
-- shown doesn't include outputs from failed txs or mempool txs? I'm not
-- sure
reevaluateApplyingPtxs
    :: (HasConfiguration, MonadUtxoRead m)
    => HashMap TxId PendingTx
    -> ChainDifficulty
    -> m (HashMap TxId PendingTx)
reevaluateApplyingPtxs ptxs curDifficulty = do
    -- Get all 'PtxApplying' transactions
    let applying, notApplying :: HashMap TxId PendingTx
        applying    = HM.filter (has (ptxCond . _PtxApplying)) ptxs
        notApplying = HM.filter (not . has (ptxCond . _PtxApplying)) ptxs
    -- Find those that are already in blocks; they'll be marked as
    -- 'PtxInNewestBlocks'
    let (present, missing) = undefined  -- HM.partition ....... applying
    -- Topsort the rest ('missing'), go one by one and either add to UTXO or
    -- mark as 'PtxWontApply'
    --
    -- TODO: is evalToilTEmpty okay?
    (missingInvalid, missingValid) <- evalToilTEmpty $ do
        let sorted = fromMaybe missing $ topsortTxs undefined missing
        -- TODO: is it okay that we don't do 'verifyGState' here?
        fmap partitionEithers $ forM sorted $ \ptx -> do
            let vtc = VTxContext True     -- TODO: is it necessarily 'True'?
            let txAux  = ptx ^. ptxTxAux
                txPure = taTx txAux
            runExceptT (verifyTxUtxo vtc txAux) >>= \case
                Left err -> pure (Left (ptx, err))
                Right _  -> applyTxToUtxo (withHash txPure) >>
                            pure (Right ptx)

    pure $ mconcat
        -- tx wasn't 'PtxApplying' -> do nothing
        [ notApplying
        -- tx isn't present but valid -> do nothing (it already is PtxApplying)
        , mkHashMap (view ptxTxId) $
            missingValid
        -- tx is present in blocks -> set 'PtxInNewestBlocks'
        , mkHashMap (view ptxTxId) $
          map (setInNewestBlocks curDifficulty) $
            present
        -- tx isn't present and invalid -> set 'PtxWontApply'
        , mkHashMap (view ptxTxId) $
          map setWontApply $
            missingInvalid
        ]

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | Create a 'HashMap' by deriving a key from each value.
mkHashMap
    :: (Eq key, Hashable key)
    => (val -> key) -> [val] -> HashMap key val
mkHashMap f = HM.fromList . map (f &&& identity)

-- | Cancel an 'PtxApplying' transaction by setting its status to
-- 'PtxWontApply' with the given cancellation reason.
setWontApply :: (PendingTx, ToilVerFailure) -> PendingTx
setWontApply (ptx, err) =
    case ptx ^. ptxCond of
        PtxApplying poolInfo ->
            ptx & ptxCond .~ PtxWontApply (pretty err) poolInfo
        _otherwise -> ptx

-- | Mark an 'PtxApplying' transaction as a transaction that has appeared in
-- the blockchain.
--
-- TODO: @curDifficulty@ is a lie here, and in case of a rollback it might
-- hurt us because all transactions will lose their 'PtxInNewestBlocks'
-- status. Perhaps we should be setting some other status.
setInNewestBlocks :: ChainDifficulty -> PendingTx -> PendingTx
setInNewestBlocks curDifficulty ptx =
    case ptx ^. ptxCond of
        PtxApplying _ ->
            ptx & ptxCond .~ PtxInNewestBlocks curDifficulty
        _otherwise -> ptx
