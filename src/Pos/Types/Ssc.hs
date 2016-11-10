-- | Ssc related functions.

module Pos.Types.Ssc
       ( xorSharedSeed
       ) where

import           Universum

import           Pos.Types.Types (SharedSeed)

-- | Apply bitwise xor to two SharedSeeds
xorSharedSeed :: SharedSeed -> SharedSeed -> SharedSeed
xorSharedSeed = (<>)
