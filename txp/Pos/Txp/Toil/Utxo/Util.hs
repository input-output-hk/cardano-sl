-- | Utility functions working on Utxo.

module Pos.Txp.Toil.Utxo.Util
       ( filterUtxoByAddr
       , filterUtxoByAddrs
       , utxoToStakes
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Map.Strict     as M
import           Universum

import           Pos.Binary.Core     ()
import           Pos.Core            (Address, Coin, StakeholderId, unsafeAddCoin)
import           Pos.Core.Address    (AddressIgnoringAttributes (..))
import           Pos.Txp.Core        (addrBelongsTo, addrBelongsToSet, txOutStake)
import           Pos.Txp.Toil.Types  (Utxo)

-- | Select only TxOuts for given address
filterUtxoByAddr :: Address -> Utxo -> Utxo
filterUtxoByAddr addr = M.filter (`addrBelongsTo` addr)

-- | Select only TxOuts for given addresses
filterUtxoByAddrs :: [Address] -> Utxo -> Utxo
filterUtxoByAddrs addrs =
    let addrSet = HS.fromList $ map AddressIA addrs
    in  M.filter (`addrBelongsToSet` addrSet)

-- | Convert 'Utxo' to map from 'StakeholderId' to stake.
utxoToStakes :: Utxo -> HashMap StakeholderId Coin
utxoToStakes = foldl' putDistr mempty . M.toList
  where
    plusAt hm (key, val) = HM.insertWith unsafeAddCoin key val hm
    putDistr hm (_, toaux) = foldl' plusAt hm (txOutStake toaux)
