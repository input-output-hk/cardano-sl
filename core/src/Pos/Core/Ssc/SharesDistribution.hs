{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Core.Ssc.SharesDistribution
       ( SharesDistribution
       ) where

import           Universum hiding (id)

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, (%))

import           Pos.Core.Common (StakeholderId)

-- | This maps shareholders to amount of shares she should issue. Depends on
-- the stake distribution.
type SharesDistribution = HashMap StakeholderId Word16

instance Buildable (StakeholderId, Word16) where
    build (id, c) = bprint ("("%build%": "%build%" shares)") id c
