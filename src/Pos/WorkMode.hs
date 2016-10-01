{-# LANGUAGE ConstraintKinds #-}

-- | WorkMode constraint.

module Pos.WorkMode
       ( WorkMode
       , RealMode
       ) where

import           Control.Monad.Catch      (MonadCatch)
import           Control.Monad.Trans      (MonadIO)
import           Control.TimeWarp.Logging (LoggerNameBox, WithNamedLogger)
import           Control.TimeWarp.Timed   (MonadTimed, TimedIO)

type WorkMode m
    = ( WithNamedLogger m
      , MonadTimed m
      , MonadCatch m
      , MonadIO m)

type RealMode = LoggerNameBox TimedIO
