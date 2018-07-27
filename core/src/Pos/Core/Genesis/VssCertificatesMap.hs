{-# LANGUAGE CPP #-}
module Pos.Core.Genesis.VssCertificatesMap
       ( GenesisVssCertificatesMap (..)
       ) where

import           Universum

import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import           Data.Semigroup (Semigroup)
import           Formatting (bprint, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util (mapJson)
import           Text.JSON.Canonical (ToJSON (..))

import           Pos.Core.Genesis.Canonical ()
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

instance Monad m => ToJSON m GenesisVssCertificatesMap where
    toJSON = toJSON . getGenesisVssCertificatesMap

instance Aeson.ToJSON GenesisVssCertificatesMap where
    toJSON = Aeson.toJSON . getGenesisVssCertificatesMap

instance Aeson.FromJSON GenesisVssCertificatesMap where
    parseJSON val = GenesisVssCertificatesMap <$> Aeson.parseJSON val
