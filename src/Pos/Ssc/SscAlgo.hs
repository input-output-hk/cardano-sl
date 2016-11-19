module Pos.Ssc.SscAlgo
       (
         SscAlgo (..)
       ) where
import           Universum

-- | Options for command line
-- | (mb move to other place? but I didn't find this place
-- | due to cyclic dependencies.)
data SscAlgo = GodTossingAlgo
             | NistBeaconAlgo
              deriving (Show, Eq)
