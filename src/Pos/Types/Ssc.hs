-- | Ssc related functions.

module Pos.Types.Ssc
       ( xorSharedSeed
       ) where

import qualified Data.ByteString as BS (pack, zipWith)
import           Universum

import           Pos.Types.Types (FtsSeed (..), SharedSeed)

-- | Apply bitwise xor to two SharedSeeds
xorSharedSeed :: SharedSeed -> SharedSeed -> SharedSeed
xorSharedSeed (FtsSeed a) (FtsSeed b) =
    FtsSeed $ BS.pack (BS.zipWith xor a b) -- fast due to rewrite rules
