-- | Slotting functionality.

module Pos.Core.Timestamp
       ( Timestamp (..)
       , timestampF
       , getCurrentTimestamp
       , diffTimestamp
       , addMicrosecondsToTimestamp
       ) where

import           Universum

import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Data.Time.Units     (Microsecond)
import           Formatting          (Format, build)
import           Mockable            (CurrentTime, Mockable, currentTime)
import qualified Prelude

-- | Timestamp is a number which represents some point in time. It is
-- used in MonadSlots and its meaning is up to implementation of this
-- type class. The only necessary knowledge is that difference between
-- timestamps is microsecond. Hence underlying type is Microsecond.
newtype Timestamp = Timestamp
    { getTimestamp :: Microsecond
    } deriving (Num, Eq, Ord, Enum, Real, Integral, Typeable, Generic)

instance Show Timestamp where
    -- If we try to 'show' Microsecond it adds an “µ”, which breaks things
    -- sometimes when printed to the console, so we convert it to Integer
    -- first so that there wouldn't be a “µ”.
    show = show . toInteger . getTimestamp

instance Read Timestamp where
    readsPrec i = fmap (first (Timestamp . fromInteger)) . Prelude.readsPrec i

instance Buildable Timestamp where
    build = Buildable.build . toInteger

instance NFData Timestamp where
    rnf Timestamp{..} = rnf (toInteger getTimestamp)

-- | Specialized formatter for 'Timestamp' data type.
timestampF :: Format r (Timestamp -> r)
timestampF = build

-- Get the current time as a timestamp
getCurrentTimestamp :: Mockable CurrentTime m => m Timestamp
getCurrentTimestamp = Timestamp <$> currentTime

diffTimestamp :: Timestamp -> Timestamp -> Microsecond
diffTimestamp t1 t2 = getTimestamp t1 - getTimestamp t2

addMicrosecondsToTimestamp :: Microsecond -> Timestamp -> Timestamp
addMicrosecondsToTimestamp m t = Timestamp { getTimestamp = (getTimestamp t) + m }
