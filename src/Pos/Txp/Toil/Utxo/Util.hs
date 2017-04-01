-- | Utility functions working on Utxo.

module Pos.Txp.Toil.Utxo.Util
       ( filterUtxoByAddr
       , utxoToStakes
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as M
import           Universum

import           Pos.Binary.Core     ()
import           Pos.Core            (Address, Coin, StakeholderId, unsafeAddCoin)
import           Pos.Txp.Core        (addrBelongsTo, txOutStake)
import           Pos.Txp.Toil.Types  (Utxo)

-- | Select only TxOuts for given addresses
filterUtxoByAddr :: Address -> Utxo -> Utxo
filterUtxoByAddr addr = M.filter (`addrBelongsTo` addr)

-- | Convert 'Utxo' to map from 'StakeholderId' to stake.
utxoToStakes :: Utxo -> HashMap StakeholderId Coin
utxoToStakes = foldl' putDistr mempty . M.toList
  where
    plusAt hm (key, val) = HM.insertWith unsafeAddCoin key val hm
    putDistr hm (_, toaux) = foldl' plusAt hm (txOutStake toaux)
