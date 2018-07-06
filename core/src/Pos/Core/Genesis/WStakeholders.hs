{-# LANGUAGE CPP #-}
module Pos.Core.Genesis.WStakeholders
       ( GenesisWStakeholders (..)
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, (%))
import           Serokell.Util (mapJson)

import           Data.Semigroup (Semigroup)
import           Pos.Core.Common (StakeholderId)

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
