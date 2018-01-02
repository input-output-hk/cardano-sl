{-# LANGUAGE AllowAmbiguousTypes #-}

module Pos.Wallet.Web.Pending.Functions
    ( reevaluateUncertainPtxs
    , mkHashMap
    ) where

import           Universum

import           Control.Lens                 (has, to, folded, _Right)
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import           Formatting                   (sformat, (%))
import           Serokell.Util                (listJson)
import           System.Wlog                  (logInfo, WithLogger)

import           Pos.Block.Core               (Block, mainBlockTxPayload)
import           Pos.Crypto                   (WithHash (..), hash)
import           Pos.Core.Configuration       (HasConfiguration, genesisHash)
import           Pos.Core.Types               (ChainDifficulty)
import           Pos.Core.Class               (difficultyL, headerHash, prevBlockL)
import qualified Pos.DB.DB                    as DB
import qualified Pos.DB.Block                 as DB
import           Pos.StateLock                (withStateLock, Priority (..), MonadStateLock)
import           Pos.Txp.Core                 (Tx, TxId, topsortTxs, taTx, txpTxs)
import           Pos.Txp.Toil.Class           (MonadUtxoRead)
import           Pos.Txp.Toil.Trans           (evalToilTEmpty)
import           Pos.Txp.Toil.Failure         (ToilVerFailure)
import           Pos.Txp.Toil.Utxo.Functions  (VTxContext (..), verifyTxUtxo,
                                               applyTxToUtxo)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..),
                                               ptxCond, _PtxApplying, _PtxWontApply,
                                               ptxTxAux, ptxTxId)
import           Pos.Util.Util                (getKeys)


-- | Reevaluate all pending txs (in 'PtxApplying' and 'PtxWontApply' states):
--
--   * 'PtxInNewestBlocks' for ones that are already in the blockchain
--     (at any depth)
--   * 'PtxWontApply' for ones that can't be applied
--   * 'PtxApplying' to ones that can be applied either directly or after
--     applying some other transactions
--
reevaluateUncertainPtxs
    :: forall ssc ctx m.
       ( DB.MonadBlockDB ssc m
       , MonadStateLock ctx m
       , HasConfiguration
       , MonadUtxoRead m
       , WithLogger m
       , MonadIO m
       )
    => HashMap TxId PendingTx -> m (HashMap TxId PendingTx)
reevaluateUncertainPtxs ptxs =  withStateLock LowPriority "reevaluateUncertainPtxs" $ \__tip -> do
    -- Get all transactions we're uncertain about
    let isUncertain ptx = has (ptxCond . _PtxApplying)  ptx ||
                          has (ptxCond . _PtxWontApply) ptx
    let (uncertain, certain) = partitionHashMap isUncertain ptxs
    -- Find those that are already in blocks; they'll be marked as
    -- 'PtxInNewestBlocks'
    presentIds <- getTxsBlockStatus @ssc (getKeys uncertain)
    let present :: HashMap TxId (PendingTx, ChainDifficulty)
        present = HM.intersectionWith (,) uncertain presentIds
    let missing :: HashMap TxId PendingTx
        missing = HM.difference uncertain presentIds
    -- Topsort the rest ('missing'), go one by one and either add to UTXO or
    -- mark as 'PtxWontApply'
    (missingInvalid, missingValid) <- evalToilTEmpty $ do
        let sorted = fromMaybe (error "Pending transactions couldn't be topsorted") $
                     topsortTxs ptxWithHash (toList missing)
        fmap partitionEithers $ forM sorted $ \ptx -> do
            let vtc = VTxContext True     -- TODO: is it necessarily 'True'?
            runExceptT (verifyTxUtxo vtc (ptx ^. ptxTxAux)) >>= \case
                Left err -> pure (Left (ptx, err))
                Right _  -> applyTxToUtxo (ptxWithHash ptx) >>
                            pure (Right ptx)

    logInfo $ sformat ("These transactions are certain and won't be affected: "
                       %listJson) (HM.keys certain)
    logInfo $ sformat ("These transactions are present in blocks: "
                       %listJson) (HM.keys present)
    logInfo $ sformat ("These transactions are not in blocks and invalid: "
                       %listJson) (map (view ptxTxId . fst) missingInvalid)
    logInfo $ sformat ("These transactions are not in blocks and valid: "
                       %listJson) (map (view ptxTxId) missingValid)


    pure $ mconcat
        [ certain                          -- certain = do nothing
        , map setInNewestBlocks present    -- present = PtxInNewestBlocks
        , mkHashMap (view ptxTxId) $       -- invalid = PtxWontApply
            map setWontApply missingInvalid
        , mkHashMap (view ptxTxId) $       -- valid = PtxApplying
            map setApplying missingValid
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

-- | Mark a transaction with 'PtxInNewestBlocks'.
setInNewestBlocks :: (PendingTx, ChainDifficulty) -> PendingTx
setInNewestBlocks (ptx, difficulty) = ptx & ptxCond %~ \case
    PtxApplying _    -> PtxInNewestBlocks difficulty
    PtxWontApply _ _ -> PtxInNewestBlocks difficulty
    other            -> other

-- | Cancel an __uncertain__ transaction by setting its status to
-- 'PtxWontApply' with the given cancellation reason.
setWontApply :: (PendingTx, ToilVerFailure) -> PendingTx
setWontApply (ptx, err) = ptx & ptxCond %~ \case
    PtxApplying    poolInfo -> PtxWontApply (pretty err) poolInfo
    PtxWontApply _ poolInfo -> PtxWontApply (pretty err) poolInfo
    other                   -> other

-- | Schedule an __uncertain__ transaction for resubmission by setting its
-- status to 'PtxApplying'.
setApplying :: PendingTx -> PendingTx
setApplying ptx = ptx & ptxCond %~ \case
    PtxWontApply _ poolInfo -> PtxApplying poolInfo
    other                   -> other

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
