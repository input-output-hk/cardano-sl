{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Higher-level functions working with GState DB.

module Pos.DB.GState.GState
       ( prepareGStateDB
       , sanityCheckGStateDB
       ) where

import           Control.Monad.Catch    (MonadMask)
import           System.Wlog            (WithLogger)
import           Universum

import           Pos.Context.Class      (WithNodeContext)
import           Pos.Context.Functions  (genesisUtxoM)
import           Pos.DB.Class           (MonadDB)
import           Pos.DB.GState.Balances (getTotalFtsStake, prepareGStateBalances,
                                         sanityCheckBalances)
import           Pos.DB.GState.Common   (prepareGStateCommon)
import           Pos.DB.GState.Update   (prepareGStateUS)
import           Pos.DB.GState.Utxo     (prepareGStateUtxo, sanityCheckUtxo)
import           Pos.Types              (HeaderHash)

-- | Put missing initial data into GState DB.
prepareGStateDB
    :: forall ssc m.
       (WithNodeContext ssc m, MonadDB ssc m)
    => HeaderHash -> m ()
prepareGStateDB initialTip = do
    prepareGStateCommon initialTip
    genesisUtxo <- genesisUtxoM
    prepareGStateUtxo genesisUtxo
    prepareGStateBalances genesisUtxo
    prepareGStateUS

-- | Check that GState DB is consistent.
sanityCheckGStateDB
    :: forall ssc m.
       (MonadDB ssc m, MonadMask m, WithLogger m)
    => m ()
sanityCheckGStateDB = do
    sanityCheckBalances
    sanityCheckUtxo =<< getTotalFtsStake
