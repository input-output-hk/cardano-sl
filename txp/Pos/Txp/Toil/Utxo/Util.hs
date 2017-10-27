-- | Utility functions working on Utxo.

module Pos.Txp.Toil.Utxo.Util
       ( filterUtxoByAddr
       , filterUtxoByAddrs
       , getTotalCoinsInUtxo
       , utxoToStakes
       , utxoToAddressCoinPairs
       , utxoToAddressCoinMap
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Map.Strict     as M
import           Universum

import           Pos.Binary.Core     ()
import           Pos.Core            (Address, Coin, sumCoins, unsafeAddCoin,
                                      unsafeIntegerToCoin)
import           Pos.Txp.Core        (TxOut (txOutValue), TxOutAux (toaOut),
                                      addrBelongsTo, addrBelongsToSet, _TxOut)
import           Pos.Txp.Toil.Types  (Utxo, utxoToStakes)

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
