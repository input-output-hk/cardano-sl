-- | Contains all Shared Seed Calculation algorithms implemented
-- in our prototype. For now only NIST Beacon and more smart algorithm
-- known as /Coin tossing with guaranteed output delivery/ (@GodTossing@).

module Pos.Ssc.SscAlgo
       ( SscAlgo (..)
       ) where

import           Universum

-- | Options for command line
data SscAlgo = GodTossingAlgo
             | NistBeaconAlgo
             deriving (Show, Eq)
