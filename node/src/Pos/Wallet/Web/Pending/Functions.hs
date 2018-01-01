module Pos.Wallet.Web.Pending.Functions
    ( reevaluateApplyingPtxs
    ) where

import           Universum

import qualified Data.HashMap.Strict          as HM
import           Control.Lens                 (has, each)

import           Pos.Core.Configuration       (HasConfiguration)
import           Pos.Core.Slotting            (flatSlotId)
import           Pos.Core.Types               (ChainDifficulty, FlatSlotId, SlotId)
import           Pos.Txp.Core                 (TxId, topsortTxs)
import           Pos.Txp.Toil.Class           (MonadUtxoRead)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..),
                                               PtxSubmitTiming (..), pstNextDelay,
                                               pstNextSlot, ptxPeerAck, ptxSubmitTiming, ptxCond, _PtxApplying)


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
    (missingInvalid, missingValid) <- runToilT $ do
        let sorted = topsortTxs undefined missing
        -- TODO: is it okay that we don't do 'verifyGState' here?
        fmap partitionEithers $ forM sorted $ \tx -> do
            let vtc = VTxContext True     -- TODO: is it necessarily 'True'?
            runExceptT (verifyTxUtxo vtc tx) >>= \case
                Left err -> pure (Left (tx, err))
                Right () -> applyTxToUtxo tx >> pure (Right tx)

    -- Put it all together:
    --
    --   * tx wasn't 'PtxApplying' -> do nothing
    --   * tx isn't present but valid -> do nothing (it already is PtxApplying)
    --   * tx is present in blocks -> set 'PtxInNewestBlocks'
    --   * tx isn't present and invalid -> set 'PtxWontApply'
    pure $ mconcat
        [ notApplying
        , missingValid
        , present & each . ptxCond %~ \case
              PtxApplying _ -> PtxInNewestBlocks curDifficulty
              other         -> other
              -- TODO: @curDifficulty@ is a lie here, and in case of a
              -- rollback it might hurt us because all transactions will
              -- lose their 'PtxInNewestBlocks' status.
        , missingInvalid & each . ptxCond %~ \case
              (PtxApplying poolInfo, err) -> PtxWontApply err poolInfo
              other                       -> other
        ]
