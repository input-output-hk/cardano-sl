module Pos.Types.Coin
       ( Coin(..)
       , coinF
       ) where

import           Data.Data           (Data)
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (Format, bprint, build, int, (%))
import           Universum

-- | Coin is the least possible unit of currency.
newtype Coin = Coin
    { getCoin :: Word64
    } deriving (Num, Enum, Integral, Show, Ord, Real, Eq, Bounded, Generic, Hashable, Data, NFData)

instance Buildable Coin where
    build = bprint (int%" coin(s)")

-- | Coin formatter which restricts type.
coinF :: Format r (Coin -> r)
coinF = build
