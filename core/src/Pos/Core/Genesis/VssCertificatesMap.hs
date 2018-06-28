{-# LANGUAGE CPP #-}
module Pos.Core.Genesis.VssCertificatesMap
       ( GenesisVssCertificatesMap (..)
       ) where

import           Universum

import           Data.Semigroup (Semigroup)
import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, (%))
import           Serokell.Util (mapJson)

import           Pos.Core.Ssc (VssCertificatesMap, getVssCertificatesMap)

-- | Predefined balances of non avvm entries.
newtype GenesisVssCertificatesMap = GenesisVssCertificatesMap
    { getGenesisVssCertificatesMap :: VssCertificatesMap
    }
#if MIN_VERSION_base(4,9,0)
    deriving (Show, Eq, Semigroup, Monoid)
#else
    deriving (Show, Eq, Monoid)
#endif

instance Buildable GenesisVssCertificatesMap where
    build (GenesisVssCertificatesMap m) =
        bprint ("GenesisVssCertificatesMap: "%mapJson) (getVssCertificatesMap m)
