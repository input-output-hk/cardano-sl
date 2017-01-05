{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Higher-level functions working with GState DB.

module Pos.DB.GState.GState
       ( prepareGStateDB
       ) where

-- import           Universum

import           Pos.DB.Class           (MonadDB)
import           Pos.DB.GState.Balances (prepareGStateBalances)
import           Pos.DB.GState.Common   (prepareGStateCommon)
import           Pos.DB.GState.Utxo     (prepareGStateUtxo)
import           Pos.Types              (HeaderHash, Utxo)

-- | Put missing initial data into GState DB.
prepareGStateDB
    :: forall ssc m.
       MonadDB ssc m
    => Utxo -> HeaderHash ssc -> m ()
prepareGStateDB genesisUtxo initialTip = do
    prepareGStateCommon initialTip
    prepareGStateUtxo genesisUtxo
    prepareGStateBalances genesisUtxo
