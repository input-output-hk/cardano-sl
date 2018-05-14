-- | Timestamp/diff types and helpers.

module Pos.Core.Slotting.Timestamp
       ( Timestamp (..)
       , _Timestamp
       , timestampF
       , parseTimestamp
       , getCurrentTimestamp
       , diffTimestamp
       , addMicrosecondsToTimestamp
       , timestampToPosix
       , timestampSeconds
       , timestampToUTCTimeL

       , TimeDiff (..)
       , addTimeDiffToTimestamp
       , subTimeDiffSafe
       ) where

import           Universum

import           Control.Lens (Iso', iso, makePrisms, from)
import qualified Data.Text.Buildable as Buildable
import           Data.Time (UTCTime, defaultTimeLocale, iso8601DateFormat, parseTimeM)
import           Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Data.Time.Units (Microsecond)
import           Formatting (Format, build)
import           Mockable (CurrentTime, Mockable, currentTime)
import           Numeric.Lens (dividing)
import qualified Prelude

-- | Timestamp is a number which represents some point in time. It is
-- used in MonadSlots and its meaning is up to implementation of this
-- type class. The only necessary knowledge is that difference between
-- timestamps is microsecond. Hence underlying type is Microsecond.
-- Amount of microseconds since Jan 1, 1970 UTC.
newtype Timestamp = Timestamp
    { getTimestamp :: Microsecond
    } deriving (Num, Eq, Ord, Enum, Real, Integral, Typeable, Generic)

makePrisms ''Timestamp

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

-- | Attempt to parse a 'Timestamp' out of a 'Text' value. Formats include:
--
-- * Fractional timestamps: @123456789.1234@
-- * ISO8601 Datetime: @1999-10-12T08:09:10@
-- * ISO8601 Date: @1999-10-12@
parseTimestamp :: Text -> Maybe Timestamp
parseTimestamp t = utcTimeParser <|> timePosixParser
  where
    str = toString t
    parseFmt :: String -> Maybe UTCTime
    parseFmt fmt =
        parseTimeM True defaultTimeLocale fmt str
    utcTimeParser =
        view (from timestampToUTCTimeL)
        <$> asum
            [ parseFmt (iso8601DateFormat (Just "%H:%M:%S%Q"))
            , parseFmt (iso8601DateFormat Nothing)
            ]
    timePosixParser =
        view (from timestampSeconds)
        <$> readMaybe @Double str

-- Get the current time as a timestamp
getCurrentTimestamp :: Mockable CurrentTime m => m Timestamp
getCurrentTimestamp = Timestamp <$> currentTime

diffTimestamp :: Timestamp -> Timestamp -> Microsecond
diffTimestamp t1 t2 = getTimestamp t1 - getTimestamp t2

addMicrosecondsToTimestamp :: Microsecond -> Timestamp -> Timestamp
addMicrosecondsToTimestamp m t = Timestamp { getTimestamp = (getTimestamp t) + m }

-- | Lens to convert timestamp to fractional seconds and vice versa.
--
-- >>> (Timestamp $ fromMicroseconds 12340000) ^. timestampSeconds
-- 12.34
-- >>> (1 :: Double) ^. from timestampSeconds :: Timestamp
-- 1000000
timestampSeconds
    :: (Eq a, RealFrac a)
    => Iso' Timestamp a
timestampSeconds = _Timestamp . iso fromIntegral round . dividing 1e6

timestampToPosix :: Timestamp -> POSIXTime
timestampToPosix = view timestampSeconds

-- | Lens to convert timestamp to 'UTCTime'.
-- Ignores leap seconds.
timestampToUTCTimeL :: Iso' Timestamp UTCTime
timestampToUTCTimeL = timestampSeconds . posixSecondsToUTCTimeL
  where
    posixSecondsToUTCTimeL = iso posixSecondsToUTCTime utcTimeToPOSIXSeconds

-- | Difference between two timestamps
newtype TimeDiff = TimeDiff
    { getTimeDiff :: Microsecond
    } deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show TimeDiff where
    show = show . toInteger . getTimeDiff

instance Read TimeDiff where
    readsPrec i = fmap (first (TimeDiff . fromInteger)) . Prelude.readsPrec i

instance Buildable TimeDiff where
    build = Buildable.build . toInteger

instance NFData TimeDiff where
    rnf TimeDiff{..} = rnf (toInteger getTimeDiff)

addTimeDiffToTimestamp :: TimeDiff -> Timestamp -> Timestamp
addTimeDiffToTimestamp = addMicrosecondsToTimestamp . getTimeDiff

subTimeDiffSafe :: TimeDiff -> TimeDiff -> TimeDiff
subTimeDiffSafe (TimeDiff t1) (TimeDiff t2)
    | t1 >= t2  = TimeDiff (t1 - t2)
    | otherwise = error "subTimeDiffSafe: first TimeDiff must be more or equal second TimeDiff"
