{-# LANGUAGE NamedFieldPuns #-}

-- | Runtime propagation of genesis data (stakes & utxo).

module Pos.Chain.Txp.GenesisUtxo
       ( genesisUtxo
       , genesisStakes
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map

import           Pos.Chain.Txp.Toil (GenesisUtxo (..), utxoToStakes)
import           Pos.Chain.Txp.Tx (TxIn (..), TxOut (..))
import           Pos.Chain.Txp.TxOutAux (TxOutAux (..))
import           Pos.Core (Address, Coin, HasGenesisData, StakesMap,
                     genesisData, makeRedeemAddress)
import           Pos.Core.Genesis (GenesisData (..), getGenesisAvvmBalances,
                     getGenesisNonAvvmBalances)
import           Pos.Crypto (unsafeHash)


genesisStakes :: HasGenesisData => StakesMap
genesisStakes = utxoToStakes . unGenesisUtxo $ genesisUtxo

genesisUtxo :: HasGenesisData => GenesisUtxo
genesisUtxo =
    let GenesisData{ gdNonAvvmBalances
                   , gdAvvmDistr
                   } = genesisData

        preUtxo :: [(Address, Coin)]
        preUtxo = (first makeRedeemAddress <$> HM.toList (getGenesisAvvmBalances gdAvvmDistr))
                                  <> (HM.toList $ getGenesisNonAvvmBalances gdNonAvvmBalances)

        utxoEntry :: (Address, Coin) -> (TxIn, TxOutAux)
        utxoEntry (addr, coin) =
                 ( TxInUtxo (unsafeHash addr) 0
                 , TxOutAux (TxOut addr coin)
                 )

     in GenesisUtxo . Map.fromList $ utxoEntry <$> preUtxo
