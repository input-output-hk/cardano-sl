module Pos.Core.Common.BlockCount
       ( BlockCount (..)
       ) where

import           Universum

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           System.Random (Random (..))

import           Pos.Binary.Class (Bi (..))

newtype BlockCount = BlockCount {getBlockCount :: Word64}
    deriving (Eq, Ord, Num, Real, Integral, Enum, Read, Show,
              Buildable, Generic, Typeable, NFData, Hashable, Random)

instance Bi BlockCount where
    encode = encode . getBlockCount
    decode = BlockCount <$> decode
    encodedSizeExpr size pxy = size (getBlockCount <$> pxy)

deriveJSON defaultOptions ''BlockCount

deriveSafeCopySimple 0 'base ''BlockCount
