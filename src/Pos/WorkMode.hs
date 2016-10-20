{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | WorkMode constraint.

module Pos.WorkMode
       ( WorkMode
       , NodeContext (..)
       , NodeParams (..)
       , RealMode
       , runRealMode
       ) where

import           Control.Monad.Catch      (MonadCatch, MonadThrow)
import           Control.TimeWarp.Logging (LoggerName, LoggerNameBox, Severity,
                                           WithNamedLogger (..), initLogging,
                                           usingLoggerName)
import           Control.TimeWarp.Timed   (MonadTimed (..), ThreadId, TimedIO, runTimedIO)
import           Formatting               (sformat, (%))
import           Universum                hiding (ThreadId)

import           Pos.Slotting             (MonadSlots (..), Timestamp (..), timestampF)
import           Pos.State                (MonadDB (..), NodeState, openMemState,
                                           openState)

type WorkMode m
    = ( WithNamedLogger m
      , MonadTimed m
      , MonadCatch m
      , MonadIO m
      , MonadSlots m
      , MonadDB m)

----------------------------------------------------------------------------
-- MonadDB
----------------------------------------------------------------------------

newtype DBHolder m a = DBHolder
    { getDBHolder :: ReaderT NodeState m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch, MonadIO, WithNamedLogger)

type instance ThreadId (DBHolder m) = ThreadId m

instance (Monad m) =>
         MonadDB (DBHolder m) where
    getNodeState = DBHolder ask

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

-- | NodeContext contains runtime context of node.
data NodeContext = NodeContext
    { -- | Time when system started working.
      ncSystemStart :: !Timestamp
    } deriving (Show)

newtype ContextHolder m a = ContextHolder
    { getContextHolder :: ReaderT NodeContext m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch, MonadIO, WithNamedLogger, MonadDB)

type instance ThreadId (ContextHolder m) = ThreadId m

instance (MonadTimed m, Monad m) =>
         MonadSlots (ContextHolder m) where
    getSystemStartTime = ContextHolder $ asks ncSystemStart
    getCurrentTime = Timestamp <$> currentTime

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

-- | Parameters necessary to run node.
data NodeParams = NodeParams
    { npDbPath          :: !(Maybe FilePath)
    , npRebuildDb       :: !Bool
    , npSystemStart     :: !(Maybe Timestamp)
    , npLoggerName      :: !LoggerName
    , npLoggingSeverity :: !Severity
    } deriving (Show)

----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RealMode is an instance of WorkMode which can be used to really run system.
type RealMode = ContextHolder (DBHolder (LoggerNameBox TimedIO))

-- TODO: use bracket
runRealMode :: NodeParams -> RealMode a -> IO a
runRealMode NodeParams {..} action = do
    initLogging [npLoggerName] npLoggingSeverity
    startTime <- getStartTime
    db <- (runTimed . runCH startTime) openDb
    (runTimed . runDH db . runCH startTime) action
  where
    getStartTime =
        case npSystemStart of
            Just t -> pure t
            Nothing ->
                runTimed $
                do t <- Timestamp <$> currentTime
                   t <$ putText (sformat ("System start: " %timestampF) t)
    openDb = maybe openMemState (openState npRebuildDb) npDbPath
    ctx startTime = NodeContext {ncSystemStart = startTime}
    runCH :: Timestamp -> ContextHolder m a -> m a
    runCH startTime = flip runReaderT (ctx startTime) . getContextHolder
    runTimed :: LoggerNameBox TimedIO a -> IO a
    runTimed = runTimedIO . usingLoggerName npLoggerName
    runDH :: NodeState -> DBHolder m a -> m a
    runDH db = flip runReaderT db . getDBHolder
