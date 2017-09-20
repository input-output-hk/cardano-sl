{-# LANGUAGE CPP #-}

-- | Non-configurable constants
--   For configurable constants, see Pos.Core.Configuration.

module Pos.Core.Constants
       ( sharedSeedLength
       , isDevelopment
       ) where

import           Universum

----------------------------------------------------------------------------
-- Constants which are not configurable
----------------------------------------------------------------------------

-- | Length of shared seed.
sharedSeedLength :: Integral a => a
sharedSeedLength = 32

-- | @True@ if current mode is 'Development'.
--
-- FIXME put it in Pos.Core.Configuration and don't use CPP.
isDevelopment :: Bool
#ifdef DEV_MODE
isDevelopment = True
#else
isDevelopment = False
#endif
