-- | Slotting functionality.

module Pos.Types.Timestamp
       ( Timestamp (..)
       , timestampF
       ) where

import           Control.TimeWarp.Timed (Microsecond)
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Buildable    as Buildable

import           Formatting             (Format, build)
import           Prelude                (Read (..), Show (..))
import           Universum              hiding (show)


-- | Timestamp is a number which represents some point in time. It is
-- used in MonadSlots and its meaning is up to implementation of this
-- type class. The only necessary knowledge is that difference between
-- timestamps is microsecond. Hence underlying type is Microsecond.
newtype Timestamp = Timestamp
    { getTimestamp :: Microsecond
    } deriving (Num, Eq, Ord, Enum, Real, Integral, Typeable)

instance Show Timestamp where
  show = show . getTimestamp

instance Read Timestamp where
  readsPrec i = fmap (first Timestamp) . readsPrec i

instance Buildable Timestamp where
    build = Buildable.build @Integer . fromIntegral

-- | Specialized formatter for 'Timestamp' data type.
timestampF :: Format r (Timestamp -> r)
timestampF = build
