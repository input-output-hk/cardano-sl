{-# LANGUAGE CPP #-}
module Pos.Core.Genesis.WStakeholders
       ( GenesisWStakeholders (..)
       ) where

import           Universum

import qualified Data.Aeson as Aeson (FromJSON, ToJSON)
import           Data.Semigroup (Semigroup)
import           Formatting (bprint, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util (mapJson)
import           Text.JSON.Canonical (FromJSON (..), ReportSchemaErrors,
                     ToJSON (..))

import           Pos.Core.Common (StakeholderId)
import           Pos.Core.Genesis.Canonical ()

-- | Wrapper around weighted stakeholders map to be used in genesis
-- core data.
--
-- Each 'Word16' is a weight. I.e. if stakeholder A has weight "1"
-- and stakeholder B has weight "3", during the bootstrap era
-- all stake in the system will be divided between A and B
-- in proportion of 1:3.
newtype GenesisWStakeholders = GenesisWStakeholders
    { getGenesisWStakeholders :: Map StakeholderId Word16
    }
#if MIN_VERSION_base(4,9,0)
  deriving (Show, Eq, Semigroup, Monoid)
#else
  deriving (Show, Eq, Monoid)
#endif

instance Buildable GenesisWStakeholders where
    build (GenesisWStakeholders m) =
        bprint ("GenesisWStakeholders: "%mapJson) m

instance Monad m => ToJSON m GenesisWStakeholders where
    toJSON (GenesisWStakeholders stks) = toJSON stks

instance ReportSchemaErrors m => FromJSON m GenesisWStakeholders where
    fromJSON = fmap GenesisWStakeholders . fromJSON

deriving instance Aeson.ToJSON GenesisWStakeholders

deriving instance Aeson.FromJSON GenesisWStakeholders
