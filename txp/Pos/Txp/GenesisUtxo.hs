{-# LANGUAGE NamedFieldPuns #-}

module Pos.Txp.GenesisUtxo
       ( genesisUtxo
       , genesisStakes
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map

import           Pos.Core            (GenesisData (..), HasConfiguration, StakesMap,
                                      genesisData, getGenesisAvvmBalances,
                                      getGenesisNonAvvmBalances, makeRedeemAddress)
import           Pos.Crypto          (unsafeHash)
import           Pos.Txp.Core        (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil        (GenesisUtxo (..), utxoToStakes)


genesisStakes :: HasConfiguration => StakesMap
genesisStakes = utxoToStakes (gdBootStakeholders genesisData) . unGenesisUtxo $ genesisUtxo

genesisUtxo :: HasConfiguration => GenesisUtxo
genesisUtxo =
    let GenesisData{ gdNonAvvmBalances
                   , gdAvvmDistr
                   } = genesisData
        preUtxo = (first makeRedeemAddress <$> HM.toList (getGenesisAvvmBalances gdAvvmDistr))
                                  <> (HM.toList $ getGenesisNonAvvmBalances gdNonAvvmBalances)
        utxoEntry (addr, coin) =
                 ( TxInUtxo (unsafeHash addr) 0
                 , TxOutAux (TxOut addr coin)
                 )
     in GenesisUtxo . Map.fromList $ utxoEntry <$> preUtxo
