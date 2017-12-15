{-# LANGUAGE NamedFieldPuns #-}

module Pos.Txp.GenesisUtxo
       ( genesisUtxo
       , genesisStakes
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map

import           Pos.Core (Address, Coin, GenesisData (..), HasConfiguration, StakesMap,
                           genesisData, getGenesisAvvmBalances, getGenesisNonAvvmBalances,
                           makeRedeemAddress)
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (unsafeHash)
import           Pos.Txp.Toil (GenesisUtxo (..), utxoToStakes)


genesisStakes :: HasConfiguration => StakesMap
genesisStakes = utxoToStakes . unGenesisUtxo $ genesisUtxo

genesisUtxo :: HasConfiguration => GenesisUtxo
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
