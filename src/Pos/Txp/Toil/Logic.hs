{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

-- | All logic of Txp,
-- it operates in terms of MonadUtxo, MonadBalances and MonadTxPool.

module Pos.Txp.Toil.Logic
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
import           Pos.Types            (Coin, StakeholderId, mkCoin)
import           Pos.Types.Coin       (coinToInteger, sumCoins, unsafeAddCoin,
                                       unsafeIntegerToCoin, unsafeSubCoin)

import           Pos.Txp.Core         (topsortTxs)
import           Pos.Txp.Core.Types   (Tx (..), TxAux, TxId, TxUndo, TxsUndo,
                                       getTxDistribution, txOutStake)
import           Pos.Txp.Toil.Class   (MonadBalances (..), MonadBalancesRead (..),
                                       MonadTxPool (..), MonadUtxo (..))
import           Pos.Txp.Toil.Failure (TxpVerFailure (..))
import qualified Pos.Txp.Toil.Utxo    as Utxo

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

-- | Apply transactions from one block.
applyTxp :: GlobalTxpMode m => [(TxAux, TxUndo)] -> m ()
applyTxp txun = do
    let (txOutPlus, txInMinus) = concatStakes txun
    recomputeStakes txOutPlus txInMinus
    mapM_ (Utxo.applyTxToUtxo' . withTxId . fst) txun

-- | Rollback transactions from one block.
rollbackTxp :: GlobalTxpMode m => [(TxAux, TxUndo)] -> m ()
rollbackTxp txun = do
    let (txOutMinus, txInPlus) = concatStakes txun
    recomputeStakes txInPlus txOutMinus
    mapM_ Utxo.rollbackTxUtxo $ reverse txun

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
normalizeTxp :: LocalTxpMode m => [(TxId, TxAux)] -> m ()
normalizeTxp txs = do
    topsorted <- note TxpCantTopsort (topsortTxs wHash txs)
    mapM_ (runExceptT . processTx) topsorted
  where
    wHash (i, (t, _, _)) = WithHash t i

-- CHECK: @processTx
-- #processWithPureChecks
-- Validate one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
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

-- Compute new stakeholder's stakes by lists of spent and received coins.
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
        = concatMap txOutStake (zip _txOutputs (getTxDistribution distr))

withTxId :: TxAux -> (TxId, TxAux)
withTxId aux@(tx, _, _) = (hash tx, aux)

processTxWithPureChecks
    :: (MonadUtxo m, MonadError TxpVerFailure m)
    => Bool -> (TxId, TxAux) -> m TxUndo
processTxWithPureChecks pureChecks tx@(_, aux) = do
    undo <- Utxo.verifyTxUtxo pureChecks pureChecks aux
    Utxo.applyTxToUtxo' tx
    pure undo
