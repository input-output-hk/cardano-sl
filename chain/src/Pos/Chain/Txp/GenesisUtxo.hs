-- | Runtime propagation of genesis data (stakes & utxo).

module Pos.Chain.Txp.GenesisUtxo
       ( genesisUtxo
       , genesisStakes
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map

import           Pos.Chain.Txp.Toil (Utxo, utxoToStakes)
import           Pos.Core (Address, Coin, StakesMap, makeRedeemAddress)
import           Pos.Core.Genesis (GenesisData (..), getGenesisAvvmBalances,
                     getGenesisNonAvvmBalances)
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (unsafeHash)


genesisStakes :: GenesisData -> StakesMap
genesisStakes genesisData =
    utxoToStakes (gdBootStakeholders genesisData) $ genesisUtxo genesisData

genesisUtxo :: GenesisData -> Utxo
genesisUtxo genesisData =
    let
        preUtxo :: [(Address, Coin)]
        preUtxo =
            (first makeRedeemAddress <$> HM.toList
                    (getGenesisAvvmBalances $ gdAvvmDistr genesisData)
                )
                <> (HM.toList $ getGenesisNonAvvmBalances $ gdNonAvvmBalances
                       genesisData
                   )

        utxoEntry :: (Address, Coin) -> (TxIn, TxOutAux)
        utxoEntry (addr, coin) =
            (TxInUtxo (unsafeHash addr) 0, TxOutAux (TxOut addr coin))
    in
        Map.fromList $ utxoEntry <$> preUtxo
