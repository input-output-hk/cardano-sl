{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Higher-level functions working with GState DB.

module Pos.DB.GState.GState
       ( prepareGStateDB
       ) where

import           Pos.Context.Class      (WithNodeContext)
import           Pos.Context.Functions  (genesisUtxoM)
import           Pos.DB.Class           (MonadDB)
import           Pos.DB.GState.Balances (prepareGStateBalances)
import           Pos.DB.GState.Common   (prepareGStateCommon)
import           Pos.DB.GState.Utxo     (prepareGStateUtxo)
import           Pos.Types              (HeaderHash)

-- | Put missing initial data into GState DB.
prepareGStateDB
    :: forall ssc m.
       (WithNodeContext ssc m, MonadDB ssc m)
    => HeaderHash ssc -> m ()
prepareGStateDB initialTip = do
    prepareGStateCommon initialTip
    genesisUtxo <- genesisUtxoM
    prepareGStateUtxo genesisUtxo
    prepareGStateBalances genesisUtxo
