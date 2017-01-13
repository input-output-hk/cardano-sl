-- | Functionality related to SharedSeed.

module Pos.Types.SharedSeed
       (
       ) where

import qualified Data.ByteString       as BS (pack, zipWith)
import qualified Data.ByteString.Char8 as BSC (pack)
import qualified Data.Semigroup        (Semigroup (..))
import           Universum

import           Pos.Constants         (sharedSeedLength)
import           Pos.Types.Types       (SharedSeed (..))

instance Semigroup SharedSeed where
    (<>) (SharedSeed a) (SharedSeed b) =
        SharedSeed $ BS.pack (BS.zipWith xor a b) -- fast due to rewrite rules

instance Monoid SharedSeed where
    mempty = SharedSeed $ BSC.pack $ replicate sharedSeedLength '\NUL'
    mappend = (Data.Semigroup.<>)
    mconcat = foldl' mappend mempty
