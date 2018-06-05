module Pos.Core.Common.BlockCount
       ( BlockCount (..)
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..))
import           System.Random (Random (..))

newtype BlockCount = BlockCount {getBlockCount :: Word64}
    deriving (Eq, Ord, Num, Real, Integral, Enum, Read, Show,
              Buildable, Generic, Typeable, NFData, Hashable, Random)

instance Bi BlockCount where
    encode = encode . getBlockCount
    decode = BlockCount <$> decode
