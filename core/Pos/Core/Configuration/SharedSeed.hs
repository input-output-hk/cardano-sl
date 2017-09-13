{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.SharedSeed
       ( HasSharedSeed
       , withSharedSeed
       , sharedSeed
       , genesisSeed
       ) where

import           Data.Reflection         (Given (..), give)
import           Pos.Core.Types          (SharedSeed (..))

type HasSharedSeed = Given SharedSeed

withSharedSeed :: SharedSeed -> (HasSharedSeed => r) -> r
withSharedSeed = give

-- | Seed that will be used for the 0th epoch. We must hardcode a seed
-- because we need to somehow determine leaders for the first ever epoch
-- (stakes are hardcoded as well so we can run FTS on those stakes using this
-- seed).
sharedSeed :: HasSharedSeed => SharedSeed
sharedSeed = given

-- | Old name for ease of refactoring
genesisSeed :: HasSharedSeed => SharedSeed
genesisSeed = sharedSeed
