{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Functions which work in 'MonadBalances' and are part of Toil logic.

module Pos.Txp.Toil.Balances.Functions
       ( applyTxsToBalances
       , rollbackTxsBalances
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.List.NonEmpty  as NE
import           Formatting          (build, sformat, (%))
import           System.Wlog         (WithLogger, logInfo)

import           Pos.Core.Coin       (coinToInteger, sumCoins, unsafeAddCoin,
                                      unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Types           (Coin, StakeholderId, mkCoin)

import           Pos.Txp.Core        (Tx (..), TxAux, TxOutAux (..), TxUndo,
                                      getTxDistribution, txOutStake)
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
    => [(StakeholderId, Coin)]
    -> [(StakeholderId, Coin)]
    -> m ()
recomputeStakes plusDistr minusDistr = do
    let (plusStakeHolders, plusCoins) = unzip plusDistr
        (minusStakeHolders, minusCoins) = unzip minusDistr
        needResolve =
            HS.toList $
            HS.fromList plusStakeHolders `HS.union`
            HS.fromList minusStakeHolders
    resolvedStakes <- mapM resolve needResolve
    totalStake <- getTotalStake
    let (positiveDelta, negativeDelta) = (sumCoins plusCoins, sumCoins minusCoins)
        newTotalStake = unsafeIntegerToCoin $
                        coinToInteger totalStake + positiveDelta - negativeDelta

    let newStakes
          = HM.toList $
              calcNegStakes minusDistr
                  (calcPosStakes $ zip needResolve resolvedStakes ++ plusDistr)
    setTotalStake newTotalStake
    mapM_ (uncurry setStake) newStakes
  where
    createInfo = sformat ("Stake for " %build%" will be created in UtxoDB")
    resolve ad = whenNothingM (getStake ad) (mkCoin 0 <$ logInfo (createInfo ad))
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
        err = error ("recomputeStakes: no stake for " <> show key)

-- Concatenate stakes of the all passed transactions and undos.
concatStakes
    :: [(TxAux, TxUndo)]
    -> ([(StakeholderId, Coin)], [(StakeholderId, Coin)])
concatStakes (unzip -> (txas, undo)) = (txasTxOutDistr, undoTxInDistr)
  where
    txasTxOutDistr = concatMap concatDistr txas
    undoTxInDistr = concatMap txOutStake (foldMap toList undo)
    concatDistr (UnsafeTx {..}, _, distr) =
        concatMap txOutStake $
        toList (NE.zipWith TxOutAux _txOutputs (getTxDistribution distr))
