{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.GenesisData
       ( HasGenesisData
       , withGenesisData
       , genesisData
       ) where

import           Data.Reflection        (Given (..), give)
import           Pos.Core.Genesis.Types (GenesisData (..))

type HasGenesisData = Given GenesisData

withGenesisData :: GenesisData -> (HasGenesisData => r) -> r
withGenesisData = give

genesisData :: HasGenesisData => GenesisData
genesisData = given
