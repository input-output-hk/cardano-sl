{-# LANGUAGE TypeFamilies #-}

-- | Convenience functions for logging stats

module Pos.Statistics.Helpers
       ( statlogCountEvent
       , statlogValueEvent
       ) where

import           Universum

import           Pos.Statistics.MonadStats (MonadStats (..))
import           Pos.Statistics.StatEntry  (CountStat, StatLabel (..), ValueStat (..))

-- | Log count statistic event.
statlogCountEvent :: (MonadStats m, StatLabel l, EntryType l ~ CountStat)
                   => l -> Word64 -> m ()
statlogCountEvent l = statLog l . fromIntegral

-- | Log value statistic event.
statlogValueEvent :: (MonadStats m, StatLabel l, EntryType l ~ ValueStat)
                   => l -> Double -> m ()
statlogValueEvent l v = statLog l $ ValueStat 1 v v v
