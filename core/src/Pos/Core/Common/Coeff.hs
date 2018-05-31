module Pos.Core.Common.Coeff
       ( Coeff (..)
       ) where

import           Universum

import           Data.Fixed (Fixed (..), Nano, showFixed)
import qualified Data.Text.Buildable as Buildable

-- | A fractional coefficient of fixed precision.
newtype Coeff = Coeff Nano
    deriving (Eq, Ord, Show, Generic, NFData, Num)

instance Buildable Coeff where
    build (Coeff x) = fromString (showFixed True x)

instance Hashable Coeff
