module Pos.Core.Common.Coeff
       ( Coeff (..)
       ) where

import           Universum

import           Data.Fixed (Fixed (..), Nano, showFixed)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text.Buildable as Buildable

import           Pos.Binary.Class (Bi (..))

-- | A fractional coefficient of fixed precision.
newtype Coeff = Coeff Nano
    deriving (Eq, Ord, Show, Generic, NFData, Num)

instance Buildable Coeff where
    build (Coeff x) = fromString (showFixed True x)

instance Bi Coeff where
    encode (Coeff n) = encode n
    decode = Coeff <$> decode @Nano

instance Hashable Coeff

deriveSafeCopySimple 0 'base ''Coeff
