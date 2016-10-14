{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | WorkMode constraint.

module Pos.WorkMode
       ( WorkMode
       , NodeContext (..)
       , RealMode
       , defaultLoggerName
       , runRealMode
       ) where

import           Control.Monad.Catch      (MonadCatch, MonadThrow)
import           Control.TimeWarp.Logging (LoggerName, LoggerNameBox,
                                           WithNamedLogger (..), usingLoggerName)
import           Control.TimeWarp.Timed   (MonadTimed (..), ThreadId, TimedIO, runTimedIO)
import           Universum                hiding (ThreadId)

import           Pos.Slotting             (MonadSlots (..), Timestamp (..))
import           Pos.State                (MonadDB (..), NodeState, openState)

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

-- | NodeContext contains runtime parameters of node.
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
-- Concrete types
----------------------------------------------------------------------------

-- | RealMode is an instance of WorkMode which can be used to really run system.
type RealMode = ContextHolder (DBHolder (LoggerNameBox TimedIO))

defaultLoggerName :: LoggerName
defaultLoggerName = "node"

-- TODO: use bracket
runRealMode :: FilePath -> NodeContext -> RealMode a -> IO a
runRealMode dbPath ctx action = do
    db <-
        runTimedIO .
        usingLoggerName defaultLoggerName .
        flip runReaderT ctx . getContextHolder $
        openState False dbPath
    runTimedIO .
        usingLoggerName defaultLoggerName .
        flip runReaderT db .
        getDBHolder . flip runReaderT ctx . getContextHolder $
        action
