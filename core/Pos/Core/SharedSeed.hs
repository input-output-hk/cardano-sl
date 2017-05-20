-- | Functionality related to SharedSeed.

module Pos.Core.SharedSeed
       (
       ) where

import           Universum

import qualified Data.ByteString       as BS (pack, zipWith)
import qualified Data.ByteString.Char8 as BSC (pack)
import qualified Data.Semigroup        (Semigroup (..))

import           Pos.Core.Constants    (sharedSeedLength)
import           Pos.Core.Types        (SharedSeed (..))

instance Semigroup SharedSeed where
    (<>) (SharedSeed a) (SharedSeed b) =
        SharedSeed $ BS.pack (BS.zipWith xor a b) -- fast due to rewrite rules

instance Monoid SharedSeed where
    mempty = SharedSeed $ BSC.pack $ replicate sharedSeedLength '\NUL'
    mappend = (Data.Semigroup.<>)
    mconcat = foldl' mappend mempty
