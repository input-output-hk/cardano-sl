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

import           Pos.Crypto           (WithHash (..), hash)
import           Pos.Types            (Coin, StakeholderId, Tx (..), TxAux, TxId, TxUndo,
                                       TxsUndo, getTxDistribution, topsortTxs, txOutStake, mkCoin)
import           Pos.Types.Coin       (coinToInteger, sumCoins, unsafeAddCoin,
                                       unsafeIntegerToCoin, unsafeSubCoin)

import           Pos.Txp.Txp.Class    (MonadTxp (..), MonadTxpRead (..))
import           Pos.Txp.Txp.Failure  (TxpVerFailure (..))
import qualified Pos.Txp.Txp.Utxo     as Utxo

type TxpMode m = (MonadTxp m, MonadError TxpVerFailure m, WithLogger m)

-- CHECK: @verifyTxp
-- | Verify transactions correctness with respect to Utxo applying
-- them one-by-one.
-- Note: transactions must be topsorted to pass check.
-- Warning: this function may apply some transactions and fail
-- eventually. Use it only on temporary data.
verifyTxp :: TxpMode m => [TxAux] -> m TxsUndo
verifyTxp = mapM (processTx . withTxId)

applyTxp :: TxpMode m => [(TxAux, TxUndo)] -> m ()
applyTxp txun = do
    let (txOutPlus, txInMinus) = concatStakes txun
    recomputeStakes txOutPlus txInMinus
    mapM_ (Utxo.applyTxToUtxo' . withTxId . fst) txun

rollbackTxp :: TxpMode m => [(TxAux, TxUndo)] -> m ()
rollbackTxp txun = do
    let (txOutMinus, txInPlus) = concatStakes txun
    recomputeStakes txInPlus txOutMinus
    mapM_ Utxo.rollbackTxUtxo $ reverse txun

-- | 1. Recompute UtxoView by current list of transactions.
-- | 2. Returns indices of valid transactions.
normalizeTxp :: TxpMode m => [(TxId, TxAux)] -> m [(TxId, TxAux, TxUndo)]
normalizeTxp txs = do
    topsorted <- note TxpCantTopsort (topsortTxs wHash txs)
    verdicts <- mapM (runExceptT . processTxWithPureChecks False) topsorted
    pure $ foldr' selectRight [] $ zip txs verdicts
  where
    selectRight ((id, tx), Left _) xs  = xs
    selectRight ((id, tx), Right u) xs = (id, tx, u) : xs
    wHash (i, (t, _, _)) = WithHash t i

-- CHECK: @processTx
-- #verifyTxUtxo
processTx
    :: TxpMode m => (TxId, TxAux) -> m TxUndo
processTx = processTxWithPureChecks True

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

recomputeStakes
    :: TxpMode m
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

-- Concatenate stakes of the all passed transactions and undos.
concatStakes
    :: [(TxAux, TxUndo)]
    -> ([(StakeholderId, Coin)], [(StakeholderId, Coin)])
concatStakes (unzip -> (txas, undo)) = (txasTxOutDistr, undoTxInDistr)
  where
    txasTxOutDistr = concatMap concatDistr txas
    undoTxInDistr = concatMap txOutStake (concat undo)
    concatDistr (Tx{..}, _, distr)
        = concatMap txOutStake (zip txOutputs (getTxDistribution distr))

withTxId :: TxAux -> (TxId, TxAux)
withTxId aux@(tx, _, _) = (hash tx, aux)

processTxWithPureChecks
    :: TxpMode m => Bool -> (TxId, TxAux) -> m TxUndo
processTxWithPureChecks pureChecks tx@(id, aux) = do
    -- TODO check it
    --whenM (hasTx id) $ throwError TxpKnown
    undo <- Utxo.verifyTxUtxo pureChecks pureChecks aux
    Utxo.applyTxToUtxo' tx
    pure undo
