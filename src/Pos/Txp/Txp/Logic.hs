{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

module Pos.Txp.Txp.Logic
    ( verifyTxp
    , applyTxp
    , rollbackTxp
    , normalizeTxp
    , processTx
    ) where

import           Control.Monad.Except (MonadError (..))
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Formatting           (build, sformat, (%))
import           System.Wlog          (WithLogger, logInfo)
import           Universum

import           Pos.Constants        (maxLocalTxs)
import           Pos.Crypto           (WithHash (..), hash)
import           Pos.Types            (Coin, StakeholderId, Tx (..), TxAux, TxId, TxUndo,
                                       TxsUndo, getTxDistribution, mkCoin, topsortTxs,
                                       txOutStake)
import           Pos.Types.Coin       (coinToInteger, sumCoins, unsafeAddCoin,
                                       unsafeIntegerToCoin, unsafeSubCoin)

import           Pos.Txp.Txp.Class    (MonadBalances (..), MonadBalancesRead (..),
                                       MonadTxPool (..), MonadUtxo (..))
import           Pos.Txp.Txp.Failure  (TxpVerFailure (..))
import qualified Pos.Txp.Txp.Utxo     as Utxo

type GlobalTxpMode m = ( MonadUtxo m
                       , MonadBalances m
                       , MonadError TxpVerFailure m
                       , WithLogger m)

type LocalTxpMode m = ( MonadUtxo m
                      , MonadTxPool m
                      , MonadError TxpVerFailure m)

-- CHECK: @verifyTxp
-- | Verify transactions correctness with respect to Utxo applying
-- them one-by-one.
-- Note: transactions must be topsorted to pass check.
-- Warning: this function may apply some transactions and fail
-- eventually. Use it only on temporary data.
verifyTxp :: GlobalTxpMode m => [TxAux] -> m TxsUndo
verifyTxp = mapM (processTxWithPureChecks True . withTxId)

applyTxp :: GlobalTxpMode m => [(TxAux, TxUndo)] -> m ()
applyTxp txun = do
    let (txOutPlus, txInMinus) = concatStakes txun
    recomputeStakes txOutPlus txInMinus
    mapM_ (Utxo.applyTxToUtxo' . withTxId . fst) txun

rollbackTxp :: GlobalTxpMode m => [(TxAux, TxUndo)] -> m ()
rollbackTxp txun = do
    let (txOutMinus, txInPlus) = concatStakes txun
    recomputeStakes txInPlus txOutMinus
    mapM_ Utxo.rollbackTxUtxo $ reverse txun

-- | 1. Recompute UtxoView by current list of transactions.
-- | 2. Apply to MemPool (and to Utxo) only valid transactions.
normalizeTxp :: LocalTxpMode m => [(TxId, TxAux)] -> m ()
normalizeTxp txs = do
    topsorted <- note TxpCantTopsort (topsortTxs wHash txs)
    mapM_ (runExceptT . processTx) topsorted
  where
    wHash (i, (t, _, _)) = WithHash t i

-- CHECK: @processTx
-- #verifyTxUtxo
processTx
    :: LocalTxpMode m => (TxId, TxAux) -> m ()
processTx tx@(id, aux) = do
    whenM (hasTx id) $ throwError TxpKnown
    whenM ((>= maxLocalTxs) <$> poolSize) $ throwError TxpOverwhelmed
    undo <- processTxWithPureChecks True tx
    putTxWithUndo id aux undo

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

recomputeStakes
    :: (MonadBalances m, WithLogger m)
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
    :: (MonadUtxo m, MonadError TxpVerFailure m)
    => Bool -> (TxId, TxAux) -> m TxUndo
processTxWithPureChecks pureChecks tx@(_, aux) = do
    undo <- Utxo.verifyTxUtxo pureChecks pureChecks aux
    Utxo.applyTxToUtxo' tx
    pure undo
