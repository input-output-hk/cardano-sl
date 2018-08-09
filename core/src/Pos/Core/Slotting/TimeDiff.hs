module Pos.Core.Slotting.TimeDiff
       ( TimeDiff (..)
       , addTimeDiffToTimestamp
       , subTimeDiffSafe
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Data.Time.Units (Microsecond)
import qualified Prelude

import           Pos.Core.Slotting.Timestamp

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
