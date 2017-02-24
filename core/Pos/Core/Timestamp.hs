-- | Slotting functionality.

module Pos.Core.Timestamp
       ( Timestamp (..)
       , timestampF
       ) where

import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (Format, build)
import           Prelude             (Read (..), Show (..))
import           Universum           hiding (show)

import           Pos.Core.Types      (Timestamp (..))

instance Show Timestamp where
  show = show . getTimestamp

instance Read Timestamp where
  readsPrec i = fmap (first Timestamp) . readsPrec i

instance Buildable Timestamp where
    build = Buildable.build @Integer . fromIntegral

-- | Specialized formatter for 'Timestamp' data type.
timestampF :: Format r (Timestamp -> r)
timestampF = build

instance NFData Timestamp where
    rnf Timestamp{..} = rnf (toInteger getTimestamp)
