{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Pos.Slotting types (infra package)

module Pos.Slotting.Arbitrary () where

import           Data.DeriveTH       (derive, makeArbitrary)
import           Test.QuickCheck     (Arbitrary (..))
import           Universum

import           Pos.Slotting.Types  (EpochSlottingData (..), SlottingData (..))
import           Pos.Types.Arbitrary ()

derive makeArbitrary ''EpochSlottingData
derive makeArbitrary ''SlottingData
