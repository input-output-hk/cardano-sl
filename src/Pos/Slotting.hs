{-# LANGUAGE ScopedTypeVariables #-}

-- | Slotting functionality.

module Pos.Slotting
       ( Timestamp (..)
       , timestampF

       , MonadSlots (..)
       , getCurrentSlot
       , getCurrentSlotFlat
       , onNewSlot
       ) where

import           Control.Monad.Catch      (MonadCatch, catch)
import           Control.TimeWarp.Logging (WithNamedLogger, logError)
import           Control.TimeWarp.Timed   (Microsecond, MonadTimed, for, fork_, wait)
import           Data.Text.Buildable      (Buildable)
import qualified Data.Text.Buildable      as Buildable
import           Formatting               (Format, build, sformat, (%))
import           Prelude                  (Read (..), Show (..))
import           Serokell.Util.Exceptions ()
import           Universum                hiding (catch, show)

import           Pos.Constants            (slotDuration)
import           Pos.DHT                  (DHTResponseT)
import           Pos.Types                (FlatSlotId, SlotId (..), flattenSlotId,
                                           unflattenSlotId)

-- | Timestamp is a number which represents some point in time. It is
-- used in MonadSlots and its meaning is up to implementation of this
-- type class. The only necessary knowledge is that difference between
-- timestamps is microsecond. Hence underlying type is Microsecond.
newtype Timestamp = Timestamp
    { getTimestamp :: Microsecond
    } deriving (Num)

instance Show Timestamp where
  show = show . getTimestamp

instance Read Timestamp where
  readsPrec i = fmap (\(a, s) -> (Timestamp a, s)) . readsPrec i

instance Buildable Timestamp where
    build = Buildable.build . (fromIntegral :: Microsecond -> Integer) . getTimestamp

timestampF :: Format r (Timestamp -> r)
timestampF = build

-- | Type class providing information about time when system started
-- functioning.
class Monad m => MonadSlots m where
    getSystemStartTime :: m Timestamp
    getCurrentTime :: m Timestamp

instance MonadSlots m => MonadSlots (DHTResponseT m) where
    getSystemStartTime = lift getSystemStartTime
    getCurrentTime = lift getCurrentTime

-- | Get id of current slot based on MonadSlots.
getCurrentSlot :: MonadSlots m => m SlotId
getCurrentSlot =
    f . getTimestamp <$> ((-) <$> getCurrentTime <*> getSystemStartTime)
  where
    f :: Microsecond -> SlotId
    f t = unflattenSlotId (fromIntegral $ t `div` slotDuration)

-- | Get flat id of current slot based on MonadSlots.
getCurrentSlotFlat :: MonadSlots m => m FlatSlotId
getCurrentSlotFlat = flattenSlotId <$> getCurrentSlot

-- | Run given action as soon as new slot starts, passing SlotId to
-- it.  This function uses MonadTimed and assumes consistency between
-- MonadSlots and MonadTimed implementations.
onNewSlot
    :: (MonadIO m, MonadTimed m, MonadSlots m, MonadCatch m, WithNamedLogger m)
    => Bool -> (SlotId -> m ()) -> m a
onNewSlot = onNewSlotDo Nothing

onNewSlotDo
    :: (MonadIO m, MonadTimed m, MonadSlots m, MonadCatch m, WithNamedLogger m)
    => Maybe SlotId -> Bool -> (SlotId -> m ()) -> m a
onNewSlotDo expectedSlotId startImmediately action = do
    waitUntilPredicate
        (maybe (const True) (<=) expectedSlotId <$> getCurrentSlot)
    curSlot <- getCurrentSlot
    -- fork is necessary because action can take more time than slotDuration
    when startImmediately $ fork_ $ actionWithCatch curSlot
    Timestamp curTime <- getCurrentTime
    let timeToWait = slotDuration - curTime `mod` slotDuration
    wait $ for timeToWait
    onNewSlotDo (Just $ succ curSlot) True actionWithCatch
  where
    waitUntilPredicate predicate =
        unlessM predicate (shortWait >> waitUntilPredicate predicate)
    shortWaitTime = (10 :: Microsecond) `max` (slotDuration `div` 10000)
    shortWait = wait $ for shortWaitTime
    -- TODO: think about exceptions more carefully.
    actionWithCatch s = action s `catch` handler
    handler :: (MonadIO m, WithNamedLogger m) => SomeException -> m ()
    handler = logError . sformat ("Error occurred: "%build)
