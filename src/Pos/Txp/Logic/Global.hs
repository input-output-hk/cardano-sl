{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Transaction processing logic.

module Pos.Txp.Logic.Global
       ( txVerifyBlocks
       , txApplyBlocks
       , txRollbackBlocks
       ) where

import           Control.Lens        (each, _Wrapped)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.List.NonEmpty  as NE
import           Formatting          (build, sformat, (%))
import           Serokell.Util       (VerificationRes (VerFailure), formatAllErrors,
                                      verResFullF)
import           System.Wlog         (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Block.Types     (Blund, Undo (undoTx))
import           Pos.Constants       (maxLocalTxs)
import           Pos.Crypto          (WithHash (..), hash, withHash)
import           Pos.DB              (MonadDB, SomePrettyBatchOp (SomePrettyBatchOp),
                                      getUtxoDB)
import qualified Pos.DB.GState       as GS
import           Pos.Exception       (assertionFailed)
import           Pos.Ssc.Class.Types (Ssc)
import           Pos.Txp.Error       (TxpError (..))
import           Pos.Txp.MemState    (MonadTxpMem (..))
import           Pos.Txp.Txp.Types   (BalancesView (..), MemPool (..), UtxoView (..))
import           Pos.Types           (Block, Coin, SlotId, StakeholderId, Tx (..), TxAux,
                                      TxDistribution (..), TxId, TxIn (..), TxOutAux,
                                      TxUndo, TxWitness, VTxGlobalContext (..),
                                      VTxLocalContext (..), blockSlot, blockTxas,
                                      coinToInteger, headerHash, mkCoin, prevBlockL,
                                      slotIdF, sumCoins, topsortTxs, txOutStake,
                                      verifyTxPure)
import           Pos.Types.Coin      (unsafeAddCoin, unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Util            (NE, NewestFirst (..), OldestFirst (..),
                                      inAssertMode, _neHead)

type TxpWorkMode ssc m = ( Ssc ssc
                         , WithLogger m
                         , MonadDB ssc m
                         , MonadTxpMem m
                         , MonadThrow m)

txVerifyBlocks = notImplemented

-- | Apply chain of /definitely/ valid blocks to state on transactions
-- processing.
txApplyBlocks
    :: TxpWorkMode ssc m
    => OldestFirst NE (Blund ssc)
    -> m (OldestFirst NE SomePrettyBatchOp)
txApplyBlocks blunds = notImplemented
    -- let blocks = map fst blunds
    -- tip <- GS.getTip
    -- when (tip /= blocks ^. _Wrapped . _neHead . prevBlockL) $ throwM $
    --     TxpCantApplyBlocks "oldest block in 'blunds' is not based on tip"
    -- inAssertMode $
    --     do verdict <- txVerifyBlocks blocks
    --        case verdict of
    --            Right _     -> pass
    --            Left errors ->
    --                assertionFailed $
    --                "txVerifyBlocks failed in txApplyBlocks call: " <> errors

    -- -- We apply all blocks and filter mempool for every block
    -- total <- GS.getTotalFtsStake
    -- let balView = BalancesView mempty total
    -- (verdict, txpModifier) <- runExceptT . runDBTxp . runTxpT notImplemented $ mapM applyTxp blunds
    -- case verdict of
    --     Left er -> throwM er
    --     Right _ -> notImplemented --map txpModifierToBatch txpModifier

-- -- | Remove from mem pool transactions from block
-- filterMemPool :: MonadTxpLD ssc m => [(TxId, (Tx, TxDistribution))] -> m ()
-- filterMemPool txs = modifyTxpLD_ (\(uv, mp, undos, tip) ->
--     let blkTxs = HM.fromList txs
--         newMPTxs = (localTxs mp) `HM.difference` blkTxs
--         newUndos = undos `HM.difference` blkTxs in
--     (uv, MemPool newMPTxs (HM.size newMPTxs), newUndos, tip))

txRollbackBlocks
    :: (WithLogger m, MonadDB ssc m)
    => NewestFirst NE (Blund ssc) -> m (NonEmpty SomePrettyBatchOp)
txRollbackBlocks blunds = notImplemented
    -- total <- getTotalFtsStake
    -- db <- getUtxoDB
    -- getNewestFirst <$>
    --     evalStateT (mapM txRollbackBlock blunds) (BalancesView mempty total db)

-- | Rollback block
txRollbackBlock :: (WithLogger m)
                => (Block ssc, Undo) -> m SomePrettyBatchOp
txRollbackBlock (block, undo) = notImplemented
    --TODO more detailed message must be here
  --   unless (length (undoTx undo) == length txs)
  --       $ panic "Number of txs must be equal length of undo"
  --   let batchOrError = foldl' prependToBatch (Right []) $ zip txs $ undoTx undo
  --   -- If we store block cache in UtxoView we must invalidate it
  --   -- Stakes/balances part
  --   let (txOutMinus, txInPlus) = concatStakes (txas, undoTx undo)
  --   let putTip = SomePrettyBatchOp $ PutTip (block ^. prevBlockL)
  --   stakesBatch <- recomputeStakes txInPlus txOutMinus
  --   either panic (pure . SomePrettyBatchOp . (\x->SomePrettyBatchOp x : [stakesBatch, putTip]))
  --          batchOrError
  -- where
  --   txas = either (const []) (toList . view blockTxas) block
  --   txs = either (const []) (toList . map (^. _1) . view blockTxas) block

  --   prependToBatch :: Either Text [SomePrettyBatchOp] ->
  --                     (Tx, [TxOutAux]) ->
  --                     Either Text [SomePrettyBatchOp]
  --   prependToBatch batchOrError (tx@Tx{..}, undoTx) = do
  --       batch <- batchOrError
  --       --TODO more detailed message must be here
  --       unless (length undoTx == length txInputs) $
  --           Left "Number of txInputs must be equal length of undo"
  --       let txId = hash tx
  --           keys = zipWith TxIn (repeat txId) [0..]
  --           putIn = zipWith (\i o -> SomePrettyBatchOp $ AddTxOut i o) txInputs undoTx
  --           delOut = map (SomePrettyBatchOp . DelTxIn) $ take (length txOutputs) keys
  --       return $ foldr' (:) (foldr' (:) batch putIn) delOut --how we could simplify it?
