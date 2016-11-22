-- | Contains all Shared Seed Calculation algorithms implemented
-- in our prototype. For now only NIST Beacon and more smart algorithm
-- known as /Coin tossing with guaranteed output delivery/ (@GodTossing@).

module Pos.Ssc.SscAlgo
       (
         SscAlgo (..)
       ) where
import           Universum

-- | Options for command line
--
-- TODO: (mb move to other place? but I didn't find this place
-- due to cyclic dependencies.)
data SscAlgo = GodTossingAlgo
             | NistBeaconAlgo
              deriving (Show, Eq)
