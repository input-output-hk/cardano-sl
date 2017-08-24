{-# LANGUAGE TypeFamilies #-}

-- | Functions which work in 'MonadBalances' and are part of Toil logic.

module Pos.Txp.Toil.Balances.Functions
       ( applyTxsToBalances
       , rollbackTxsBalances
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.List.NonEmpty  as NE
import           Formatting          (sformat, (%))
import           Serokell.Util.Text  (listJson)
import           System.Wlog         (WithLogger, logDebug)

import           Pos.Core            (mkCoin)
import           Pos.Core.Coin       (coinToInteger, sumCoins, unsafeIntegerToCoin)
import           Pos.Txp.Core        (Tx (..), TxAux (..), TxOutAux (..),
                                      TxOutDistribution, TxUndo, getTxDistribution,
                                      txOutStake)
import           Pos.Txp.Toil.Class  (MonadBalances (..), MonadBalancesRead (..))

type BalancesMode m = (MonadBalances m, WithLogger m)

-- | Apply transactions to balances.
applyTxsToBalances :: BalancesMode m => [(TxAux, TxUndo)] -> m ()
applyTxsToBalances txun = do
    let (txOutPlus, txInMinus) = concatStakes txun
    recomputeStakes txOutPlus txInMinus

-- | Rollback application of transactions to balances.
rollbackTxsBalances :: BalancesMode m => [(TxAux, TxUndo)] -> m ()
rollbackTxsBalances txun = do
    let (txOutMinus, txInPlus) = concatStakes txun
    recomputeStakes txInPlus txOutMinus

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- Compute new stakeholder's stakes by lists of spent and received coins.
recomputeStakes
    :: BalancesMode m
    => TxOutDistribution
    -> TxOutDistribution
    -> m ()
recomputeStakes plusDistr minusDistr = do
    let (plusStakeHolders, plusCoins) = unzip plusDistr
        (minusStakeHolders, minusCoins) = unzip minusDistr
        needResolve =
            HS.toList $
            HS.fromList plusStakeHolders `HS.union`
            HS.fromList minusStakeHolders
    resolvedStakesRaw <- mapM resolve needResolve
    let resolvedStakes = map fst resolvedStakesRaw
    let createdStakes = concatMap snd resolvedStakesRaw
    unless (null createdStakes) $
        logDebug $ sformat ("Stakes for "%listJson%" will be created in BalancesDB") createdStakes
    totalStake <- getTotalStake
    let (positiveDelta, negativeDelta) = (sumCoins plusCoins, sumCoins minusCoins)
        newTotalStake = unsafeIntegerToCoin $
                        coinToInteger totalStake + positiveDelta - negativeDelta
    let newStakes
          = HM.toList $
            -- It's safe befause user's stake can't be more than a
            -- limit. Also we first add then subtract, so we return to
            -- the word64 range.
            map unsafeIntegerToCoin $
            calcNegStakes minusDistr
                (calcPosStakes $ zip needResolve resolvedStakes ++ plusDistr)
    setTotalStake newTotalStake
    mapM_ (uncurry setStake) newStakes
  where
    resolve ad = getStake ad >>= \case
        Just x -> pure (x, [])
        Nothing -> pure (mkCoin 0, [ad])
    calcPosStakes = foldl' plusAt HM.empty
    -- This implementation does all the computation using
    -- Integer. Maybe it's possible to do it in word64. (@volhovm)
    calcNegStakes distr hm = foldl' minusAt hm distr
    plusAt hm (key, c) = HM.insertWith (+) key (coinToInteger c) hm
    minusAt hm (key, c) =
        HM.alter (maybe err (\v -> Just (v - coinToInteger c))) key hm
      where
        err = error ("recomputeStakes: no stake for " <> show key)

-- Concatenate stakes of the all passed transactions and undos.
concatStakes
    :: [(TxAux, TxUndo)]
    -> (TxOutDistribution, TxOutDistribution)
concatStakes (unzip -> (txas, undo)) = (txasTxOutDistr, undoTxInDistr)
  where
    onlyKnownUndos = catMaybes . toList
    txasTxOutDistr = concatMap concatDistr txas
    undoTxInDistr = concatMap txOutStake (foldMap onlyKnownUndos undo)
    concatDistr (TxAux UnsafeTx {..} _ distr) =
        concatMap txOutStake $
        toList (NE.zipWith TxOutAux _txOutputs (getTxDistribution distr))
