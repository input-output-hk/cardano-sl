module Pos.Discovery.Constants
       ( neighborsSendThreshold
       ) where

import           Universum
import           Pos.Infra.Constants (ccNeighboursSendThreshold, infraConstants)

-- | See 'Pos.CompileConfig.ccNeighboursSendThreshold'.
neighborsSendThreshold :: Integral a => a
neighborsSendThreshold =
    fromIntegral . ccNeighboursSendThreshold $ infraConstants
