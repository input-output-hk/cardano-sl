module Pos.Core.Genesis.VssCertificatesMap
       ( GenesisVssCertificatesMap (..)
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, (%))
import           Serokell.Util (mapJson)

import           Pos.Core.Ssc (VssCertificatesMap, getVssCertificatesMap)

-- | Predefined balances of non avvm entries.
newtype GenesisVssCertificatesMap = GenesisVssCertificatesMap
    { getGenesisVssCertificatesMap :: VssCertificatesMap
    } deriving (Show, Eq, Monoid)

instance Buildable GenesisVssCertificatesMap where
    build (GenesisVssCertificatesMap m) =
        bprint ("GenesisVssCertificatesMap: "%mapJson) (getVssCertificatesMap m)
