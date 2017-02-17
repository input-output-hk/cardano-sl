{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

module Pos.Txp.Txp.Logic
    (
    ) where

import           Control.Monad.Except (MonadError (..))
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Formatting           (build, int, sformat, stext, (%))
import           System.Wlog          (WithLogger, logInfo)
import           Universum

import           Pos.Txp.Txp.Class    (MonadTxp (..), MonadTxpRead (..))
import           Pos.Txp.Txp.Failure  (TxpVerFailure (..))

import           Pos.Types.Coin       (coinToInteger, sumCoins, unsafeAddCoin,
                                       unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Types.Core
import           Pos.Types.Types

type ApplyMode m = (MonadTxp m, MonadError TxpVerFailure m, WithLogger m)

verifyAndApplyTxp
    :: (MonadTxp m, MonadError TxpVerFailure m)
    => [TxAux] -> m TxsUndo
verifyAndApplyTxp txas = notImplemented

applyTxp
    :: (MonadTxp m, MonadError TxpVerFailure m)
    => [TxAux] -> m ()
applyTxp = notImplemented
  --   undos <- mapM verifyOneTx txas
  --   let txsAndIds = map (\tx -> (hash (tx ^. _1), (tx ^. _1, tx ^. _3))) txas --TODO improve
  --   let batch = foldr' prependToBatch [] txsAndIds
  --   --let putTip = SomePrettyBatchOp $ PutTip (headerHash blk)
  --   --filterMemPool txsAndIds
  --   let (txOutPlus, txInMinus) = concatStakes (txas, undo)
  --   stakesBatch <- recomputeStakes txOutPlus txInMinus
  --   pure $ SomePrettyBatchOp $ [stakesBatch, SomePrettyBatchOp batch, putTip]
  -- where
  --   prependToBatch :: (TxId, (Tx, TxDistribution)) -> [SomePrettyBatchOp] -> [SomePrettyBatchOp]
  --   prependToBatch (txId, (Tx{..}, distr)) batch =
  --       let keys = zipWith TxIn (repeat txId) [0 ..]
  --           delIn = map (SomePrettyBatchOp . DelTxIn) txInputs
  --           putOut = zipWith (\i o -> SomePrettyBatchOp $ AddTxOut i o)
  --                        keys
  --                        (zip txOutputs (getTxDistribution distr))
  --       in foldr' (:) (foldr' (:) batch putOut) delIn --how we could simplify it?

recomputeStakes
    :: ApplyMode m
    => [(StakeholderId, Coin)]
    -> [(StakeholderId, Coin)]
    -> m ()
recomputeStakes plusDistr minusDistr = do
    let needResolve =
            HS.toList $
            HS.fromList (map fst plusDistr) `HS.union`
            HS.fromList (map fst minusDistr)
    resolvedStakes <- mapM resolve needResolve
    totalStake <- getTotalStake
    let positiveDelta = sumCoins (map snd plusDistr)
    let negativeDelta = sumCoins (map snd minusDistr)
    let newTotalStake = unsafeIntegerToCoin $
                        coinToInteger totalStake + positiveDelta - negativeDelta

    let newStakes
          = HM.toList $
              calcNegStakes minusDistr
                  (calcPosStakes $ zip needResolve resolvedStakes ++ plusDistr)
    setTotalStake newTotalStake
    mapM_ (uncurry setStake) newStakes
  where
    createInfo = sformat ("Stake for " %build%" will be created in UtxoDB")
    resolve ad = maybe (mkCoin 0 <$ logInfo (createInfo ad)) pure =<< getStake ad
    calcPosStakes distr = foldl' plusAt HM.empty distr
    calcNegStakes distr hm = foldl' minusAt hm distr
    -- @pva701 says it's not possible to get negative coin here. We *can* in
    -- theory get overflow because we're adding and only then subtracting,
    -- but in practice it won't happen unless someone has 2^63 coins or
    -- something.
    plusAt hm (key, val) = HM.insertWith unsafeAddCoin key val hm
    minusAt hm (key, val) =
        HM.alter (maybe err (\v -> Just (unsafeSubCoin v val))) key hm
      where
        err = panic ("recomputeStakes: no stake for " <> show key)

    concatStakes
        :: ([TxAux], TxsUndo)
        -> ([(StakeholderId, Coin)], [(StakeholderId, Coin)])
    concatStakes (txas, undo) = (txasTxOutDistr, undoTxInDistr)
      where
        txasTxOutDistr = concatMap concatDistr txas
        undoTxInDistr = concatMap txOutStake (concat undo)
        concatDistr (Tx{..}, _, distr)
            = concatMap txOutStake (zip txOutputs (getTxDistribution distr))


-- -- | Remove from mem pool transactions from block
-- filterMemPool :: MonadTxpLD ssc m => [(TxId, (Tx, TxDistribution))] -> m ()
-- filterMemPool txs = modifyTxpLD_ (\(uv, mp, undos, tip) ->
--     let blkTxs = HM.fromList txs
--         newMPTxs = (localTxs mp) `HM.difference` blkTxs
--         newUndos = undos `HM.difference` blkTxs in
--     (uv, MemPool newMPTxs (HM.size newMPTxs), newUndos, tip))

-- rollbackTxp
--     :: ( WithLogger m,
--          MonadBalances ssc m)
--     => (Block ssc, Undo) -> m SomePrettyBatchOp
-- rollbackTxp (block, undo) = notImplemented

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
-- normalizeTxp
--     :: (MonadDB ssc m, MonadTxpLD ssc m)
--     => [TxAux] -> m ()
-- normalizeTxp = notImplemented

-- CHECK: @processTxDo
-- #verifyTxPure
processTx
    :: (MonadTxp m, MonadError TxpVerFailure m)
    => (TxId, TxAux) -> m TxUndo
processTx tx@(id, aux) = notImplemented
  --   whenM (hasTx id) $ throwError TxpKnown
  --   undo <- verifyOneTx aux
  --   addTxWithUndo tx undo
  --   --either (throwError . TxpInvalid . formatAllErrors) (addTx tx) verifyRes
  -- where
  --   verifyRes =
  --       verifyTx True True VTxGlobalContext (fmap (fmap VTxLocalContext) . utxoGet) aux

    -- prependToUndo undo inp =
    --     fromMaybe (panic "Input isn't resolved")
    --               (utxoGet inp) : undo

    -- newState nAddUtxo nDelUtxo oldTxs oldSize oldUndos =
    --     let keys = zipWith TxIn (repeat id) [0 ..]
    --         zipKeys = zip keys (txOutputs tx `zip` getTxDistribution txd)
    --         addUtxoMembers = zip (txInputs tx) $ map (`HM.member` nAddUtxo) (txInputs tx)
    --         squashedDels = map fst $ filter snd addUtxoMembers
    --         notSquashedDels = map fst $ filter (not . snd) addUtxoMembers
    --         newAddUtxo' = foldl' (flip $ uncurry HM.insert)
    --                              (foldl' (flip HM.delete) nAddUtxo squashedDels)
    --                              zipKeys
    --         newDelUtxo' = foldl' (flip HS.insert) nDelUtxo notSquashedDels
    --         newUndos = HM.insert id (reverse $ foldl' prependToUndo [] (txInputs tx)) oldUndos
    --     in ( PTRadded
    --        , ( UtxoView newAddUtxo' newDelUtxo' utxoDB
    --          , MemPool (HM.insert id (tx, txw, txd) oldTxs) (oldSize + 1)
    --          , newUndos
    --          , tip))
