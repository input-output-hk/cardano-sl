{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.GenesisAvvmBalances
       ( HasGenesisAvvmBalances
       , withGenesisAvvmBalances
       , genesisAvvmBalances
       ) where

import           Data.Reflection        (Given (..), give)
import           Pos.Core.Genesis.Types (GenesisAvvmBalances)

type HasGenesisAvvmBalances = Given GenesisAvvmBalances

withGenesisAvvmBalances :: GenesisAvvmBalances -> (HasGenesisAvvmBalances => r) -> r
withGenesisAvvmBalances = give

genesisAvvmBalances :: HasGenesisAvvmBalances => GenesisAvvmBalances
genesisAvvmBalances = given
