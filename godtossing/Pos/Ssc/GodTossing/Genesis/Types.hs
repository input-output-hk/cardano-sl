module Pos.Ssc.GodTossing.Genesis.Types
       ( GenesisGtData(..)
       ) where

import           Universum

import           Pos.Core.Vss (VssCertificatesMap)

-- | GodTossing data hardcoded in genesis.
data GenesisGtData = GenesisGtData
    { ggdVssCertificates :: VssCertificatesMap
    } deriving (Generic, Show, Eq)

instance Monoid GenesisGtData where
    mempty = GenesisGtData mempty
    (GenesisGtData vssA) `mappend` (GenesisGtData vssB) =
        GenesisGtData (vssA <> vssB)
