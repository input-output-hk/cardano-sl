{-# LANGUAGE TypeFamilies #-}

-- | Convenience functions for logging stats

module Pos.Statistics.Helpers
       ( statlogCountEvent
       , statlogValueEvent
       ) where

import           Universum

import           Pos.Statistics.MonadStats (MonadStats (..))
import           Pos.Statistics.StatEntry  (CountStat, StatLabel (..), ValueStat (..))

statlogCountEvent :: (MonadStats m, StatLabel l, EntryType l ~ CountStat)
                   => l -> Word64 -> m ()
statlogCountEvent l = statLog l . fromIntegral

statlogValueEvent :: (MonadStats m, StatLabel l, EntryType l ~ ValueStat)
                   => l -> Double -> m ()
statlogValueEvent l v = statLog l $ ValueStat 1 v v v
