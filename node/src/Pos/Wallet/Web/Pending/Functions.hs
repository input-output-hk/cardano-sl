{-# LANGUAGE AllowAmbiguousTypes #-}

module Pos.Wallet.Web.Pending.Functions
    ( reevaluateApplyingPtxs
    , mkHashMap
    ) where

import           Universum

import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import           Control.Lens                 (has, to, folded, _Right)

import           Pos.Block.Core               (Block, mainBlockTxPayload)
import           Pos.Crypto                   (WithHash (..), hash)
import           Pos.Core.Configuration       (HasConfiguration, genesisHash)
import           Pos.Core.Types               (ChainDifficulty)
import           Pos.Core.Class               (difficultyL, headerHash, prevBlockL)
import qualified Pos.DB.DB                    as DB
import qualified Pos.DB.Block                 as DB
import           Pos.Txp.Core                 (Tx, TxId, topsortTxs, taTx, txpTxs)
import           Pos.Txp.Toil.Class           (MonadUtxoRead)
import           Pos.Txp.Toil.Trans           (evalToilTEmpty)
import           Pos.Txp.Toil.Failure         (ToilVerFailure)
import           Pos.Txp.Toil.Utxo.Functions  (VTxContext (..), verifyTxUtxo,
                                               applyTxToUtxo)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..),
                                               ptxCond, _PtxApplying, ptxTxAux, ptxTxId)
import           Pos.Util.Util                (getKeys)


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
    :: forall ssc m.
       (DB.MonadBlockDB ssc m, HasConfiguration, MonadUtxoRead m)
    => HashMap TxId PendingTx -> m (HashMap TxId PendingTx)
reevaluateApplyingPtxs ptxs = do
    -- Get all 'PtxApplying' transactions
    let (applying, notApplying) = partitionHashMap isApplying ptxs
    -- Find those that are already in blocks; they'll be marked as
    -- 'PtxInNewestBlocks'
    presentIds <- getTxsBlockStatus @ssc (getKeys applying)
    let present :: HashMap TxId (PendingTx, ChainDifficulty)
        present = HM.intersectionWith (,) applying presentIds
    let missing :: HashMap TxId PendingTx
        missing = HM.difference applying presentIds
    -- Topsort the rest ('missing'), go one by one and either add to UTXO or
    -- mark as 'PtxWontApply'
    --
    -- TODO: is evalToilTEmpty okay?
    (missingInvalid, missingValid) <- evalToilTEmpty $ do
        let sorted = fromMaybe (toList missing) $
                     topsortTxs ptxWithHash (toList missing)
        fmap partitionEithers $ forM sorted $ \ptx -> do
            let vtc = VTxContext True     -- TODO: is it necessarily 'True'?
            runExceptT (verifyTxUtxo vtc (ptx ^. ptxTxAux)) >>= \case
                Left err -> pure (Left (ptx, err))
                Right _  -> applyTxToUtxo (ptxWithHash ptx) >>
                            pure (Right ptx)

    pure $ mconcat
        -- tx wasn't 'PtxApplying' -> do nothing
        [ notApplying
        -- tx isn't present but valid -> do nothing (it already is PtxApplying)
        , mkHashMap (view ptxTxId) $
            missingValid
        -- tx is present in blocks -> set 'PtxInNewestBlocks'
        , fmap setInNewestBlocks $
            present
        -- tx isn't present and invalid -> set 'PtxWontApply'
        , mkHashMap (view ptxTxId) $
          map setWontApply $
            missingInvalid
        ]

----------------------------------------------------------------------------
-- General utilities
----------------------------------------------------------------------------

-- | Create a 'HashMap' by deriving a key from each value.
mkHashMap
    :: (Eq k, Hashable k)
    => (v -> k) -> [v] -> HashMap k v
mkHashMap f = HM.fromList . map (f &&& identity)

-- | Partition a 'HashMap'.
partitionHashMap
    :: (v -> Bool) -> HashMap k v -> (HashMap k v, HashMap k v)
partitionHashMap p hm = (HM.filter p hm, HM.filter (not . p) hm)

----------------------------------------------------------------------------
-- Utilities for pending transactions
----------------------------------------------------------------------------

-- | Check if the transaction is in 'PtxApplying' state.
isApplying :: PendingTx -> Bool
isApplying = has (ptxCond . _PtxApplying)

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
setInNewestBlocks :: (PendingTx, ChainDifficulty) -> PendingTx
setInNewestBlocks (ptx, difficulty) =
    case ptx ^. ptxCond of
        PtxApplying _ ->
            ptx & ptxCond .~ PtxInNewestBlocks difficulty
        _otherwise -> ptx

-- | Get tx and hash out of a pending transaction.
ptxWithHash :: PendingTx -> WithHash Tx
ptxWithHash ptx =
    WithHash { whData = ptx ^. ptxTxAux . to taTx
             , whHash = ptx ^. ptxTxId
             }

----------------------------------------------------------------------------
-- Utilities for traversing the blockchain
----------------------------------------------------------------------------

-- | Get status of several transactions – for ones that are in the
-- blockchain, say in what block they are.
getTxsBlockStatus
    :: forall ssc m. DB.MonadBlockDB ssc m
    => HashSet TxId
    -> m (HashMap TxId ChainDifficulty)
getTxsBlockStatus txs = foldMapBlocks @ssc $ \block -> do
    let blockTxs   = block ^.. _Right . mainBlockTxPayload . txpTxs . folded
        blockTxIds = HS.fromList $ map hash blockTxs
        blockDiff  = block ^. difficultyL
    pure $ fmap (const blockDiff) $ HS.toMap $ HS.intersection txs blockTxIds

-- | Traverse all blocks in the blockchain (starting from the newest one)
-- and concatenate the results.
foldMapBlocks
    :: forall ssc m a. (DB.MonadBlockDB ssc m, Monoid a)
    => (Block ssc -> m a) -> m a
foldMapBlocks f = do
    tip <- headerHash <$> DB.getTipHeader @ssc
    go tip mempty
  where
    go h !acc
        | h == genesisHash = pure acc
        | otherwise = do
              block <- DB.getBlockThrow @ssc h
              val <- f block
              go (block ^. prevBlockL) (acc <> val)
