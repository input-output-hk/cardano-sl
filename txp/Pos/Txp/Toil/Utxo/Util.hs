-- | Utility functions working on Utxo.

module Pos.Txp.Toil.Utxo.Util
       ( filterUtxoByAddr
       , filterUtxoByAddrs
       , getTotalCoinsInUtxo
       , utxoToStakes
       , utxoToAddressCoinPairs
       , utxoToAddressCoinMap
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M

import           Pos.Binary.Core ()
import           Pos.Core (Address, Coin, HasGenesisData, StakesMap, sumCoins, unsafeAddCoin,
                           unsafeIntegerToCoin)
import           Pos.Core.Txp (TxOut (txOutValue), TxOutAux (..), _TxOut)
import           Pos.Txp.Base (addrBelongsTo, addrBelongsToSet, txOutStake)
import           Pos.Txp.Toil.Types (Utxo)

-- | Select only TxOuts for given address
filterUtxoByAddr :: Address -> Utxo -> Utxo
filterUtxoByAddr addr = M.filter (`addrBelongsTo` addr)

-- | Select only TxOuts for given addresses
filterUtxoByAddrs :: [Address] -> Utxo -> Utxo
filterUtxoByAddrs addrs =
    let addrSet = HS.fromList addrs
    in  M.filter (`addrBelongsToSet` addrSet)

-- | Get total amount of coins in given Utxo
getTotalCoinsInUtxo :: Utxo -> Coin
getTotalCoinsInUtxo =
    unsafeIntegerToCoin . sumCoins .
    map (txOutValue . toaOut) . toList

-- | Convert 'Utxo' to 'StakesMap'.
utxoToStakes :: HasGenesisData => Utxo -> StakesMap
utxoToStakes = foldl' putDistr mempty . M.toList
  where
    plusAt hm (key, val) = HM.insertWith unsafeAddCoin key val hm
    putDistr hm (_, TxOutAux txOut) = foldl' plusAt hm (txOutStake txOut)

utxoToAddressCoinPairs :: Utxo -> [(Address, Coin)]
utxoToAddressCoinPairs utxo = combineWith unsafeAddCoin txOuts
  where
    combineWith :: (Eq a, Hashable a) => (b -> b -> b) -> [(a, b)] -> [(a, b)]
    combineWith func = HM.toList . HM.fromListWith func

    txOuts :: [(Address, Coin)]
    txOuts = map processTxOutAux utxoElems

    utxoElems :: [TxOutAux]
    utxoElems = M.elems utxo

    processTxOutAux :: TxOutAux -> (Address, Coin)
    processTxOutAux = view _TxOut . toaOut

utxoToAddressCoinMap :: Utxo -> HashMap Address Coin
utxoToAddressCoinMap = HM.fromList . utxoToAddressCoinPairs
