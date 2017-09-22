-- | Non-configurable constants
--   For configurable constants, see Pos.Core.Configuration.

module Pos.Core.Constants
       ( sharedSeedLength
       ) where

import           Universum

----------------------------------------------------------------------------
-- Constants which are not configurable
----------------------------------------------------------------------------

-- | Length of shared seed.
sharedSeedLength :: Integral a => a
sharedSeedLength = 32
