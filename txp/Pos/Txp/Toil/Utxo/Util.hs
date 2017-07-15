-- | Utility functions working on Utxo.

module Pos.Txp.Toil.Utxo.Util
       ( filterUtxoByAddr
       , filterUtxoByAddrs
       , utxoToStakes
       , utxoToAddressCoinPairs
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Map.Strict     as M
import           Universum

import           Pos.Binary.Core     ()
import           Pos.Core            (Address, Coin, StakeholderId, unsafeAddCoin)
import           Pos.Core.Address    (AddressIgnoringAttributes (..))
import           Pos.Txp.Core        (TxOutAux (toaOut), addrBelongsTo, addrBelongsToSet,
                                      txOutStake, _TxOut)
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

utxoToAddressCoinPairs :: Utxo -> [(Address, Coin)]
utxoToAddressCoinPairs utxo =
    let txOuts = map (view _TxOut . toaOut) . M.elems $ utxo
    in combineWith unsafeAddCoin txOuts
  where
    combineWith :: (Eq a, Hashable a) => (b -> b -> b) -> [(a, b)] -> [(a, b)]
    combineWith func = HM.toList . HM.fromListWith func
