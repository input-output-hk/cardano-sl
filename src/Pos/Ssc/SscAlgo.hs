module Pos.Ssc.SscAlgo
       (
         SscAlgo (..)
       ) where
import           Universum

-- | Options for command line
-- | (mb move to other place? but I didn't find this place
-- | due fucking cyclic dependencies.)
data SscAlgo = DynamicStateAlgo
             | NistBeaconAlgo
              deriving (Show, Eq)
