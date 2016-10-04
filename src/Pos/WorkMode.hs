{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

-- | WorkMode constraint.

module Pos.WorkMode
       ( WorkMode
       , NodeContext (..)
       , RealMode
       , defaultLoggerName
       , runRealMode
       ) where

import           Control.Monad.Catch      (MonadCatch, MonadThrow)
import           Control.Monad.Trans      (MonadIO)
import           Control.TimeWarp.Logging (LoggerName, LoggerNameBox,
                                           WithNamedLogger (..), usingLoggerName)
import           Control.TimeWarp.Timed   (MonadTimed (..), ThreadId, TimedIO, runTimedIO)
import           Universum                hiding (ThreadId)

import           Pos.Slotting             (MonadSlots (..), Timestamp)

type WorkMode m
    = ( WithNamedLogger m
      , MonadTimed m
      , MonadCatch m
      , MonadIO m
      , MonadSlots m)

-- | NodeContext contains runtime parameters of node.
data NodeContext = NodeContext
    { -- | Time when system started working.
      ncSystemStart :: !Timestamp  -- TODO
    } deriving (Show)

newtype ContextHolder m a = ContextHolder
    { getContextHolder :: ReaderT NodeContext m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch, MonadIO, WithNamedLogger)

type instance ThreadId (ContextHolder m) = ThreadId m

instance Monad m => MonadSlots (ContextHolder m) where
    getSystemStartTime = ContextHolder $ asks ncSystemStart
    getCurrentTime = notImplemented

type RealMode = ContextHolder (LoggerNameBox TimedIO)

defaultLoggerName :: LoggerName
defaultLoggerName = "node"

runRealMode :: NodeContext -> RealMode a -> IO a
runRealMode ctx =
    runTimedIO . usingLoggerName defaultLoggerName . flip runReaderT ctx . getContextHolder
