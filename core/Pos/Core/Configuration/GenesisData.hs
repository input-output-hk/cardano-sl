{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.GenesisData
       ( HasGenesisData
       , withGenesisData
       , genesisData
       , genesisVssCerts
       ) where

import           Universum

import           Data.Reflection        (Given (..), give)
import           Pos.Core.Genesis.Types (GenesisData (..), getGenesisVssCertificatesMap)
import           Pos.Core.Vss           (VssCertificatesMap)

type HasGenesisData = Given GenesisData

withGenesisData :: GenesisData -> (HasGenesisData => r) -> r
withGenesisData = give

genesisData :: HasGenesisData => GenesisData
genesisData = given

genesisVssCerts :: HasGenesisData => VssCertificatesMap
genesisVssCerts = getGenesisVssCertificatesMap $ gdVssCerts genesisData
