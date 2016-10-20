{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | WorkMode constraint.

module Pos.WorkMode
       ( WorkMode
       , NodeContext (..)
       , WithNodeContext (..)
       , ncPublicKey
       , ncVssPublicKey
       , NodeParams (..)
       , RealMode
       , runRealMode
       ) where

import           Control.Monad.Catch      (MonadCatch, MonadThrow)
import           Control.TimeWarp.Logging (LoggerName, LoggerNameBox, Severity,
                                           WithNamedLogger (..), initLogging,
                                           usingLoggerName)
import           Control.TimeWarp.Rpc     (ResponseT)
import           Control.TimeWarp.Timed   (MonadTimed (..), ThreadId, TimedIO, runTimedIO)
import           Formatting               (sformat, (%))
import           Universum                hiding (ThreadId)

import           Pos.Crypto               (PublicKey, SecretKey, VssKeyPair, VssPublicKey,
                                           toPublic, toVssPublicKey)
import           Pos.Slotting             (MonadSlots (..), Timestamp (..), timestampF)
import           Pos.State                (MonadDB (..), NodeState, openMemState,
                                           openState)

type WorkMode m
    = ( WithNamedLogger m
      , MonadTimed m
      , MonadCatch m
      , MonadIO m
      , MonadSlots m
      , MonadDB m
      , WithNodeContext m)

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
    , -- | Secret key used for blocks creation.
      ncSecretKey   :: !SecretKey
    , -- | Vss key pair used for MPC.
      ncVssKeyPair  :: !VssKeyPair
    } deriving (Show)

ncPublicKey :: NodeContext -> PublicKey
ncPublicKey = toPublic . ncSecretKey

ncVssPublicKey :: NodeContext -> VssPublicKey
ncVssPublicKey = toVssPublicKey . ncVssKeyPair

class WithNodeContext m where
    getNodeContext :: m NodeContext

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (ReaderT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (StateT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (ExceptT e m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (ResponseT m) where
    getNodeContext = lift getNodeContext

newtype ContextHolder m a = ContextHolder
    { getContextHolder :: ReaderT NodeContext m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch, MonadIO, WithNamedLogger, MonadDB)

type instance ThreadId (ContextHolder m) = ThreadId m

instance Monad m => WithNodeContext (ContextHolder m) where
    getNodeContext = ContextHolder ask

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
    , npSecretKey       :: !SecretKey
    , npVssKeyPair      :: !VssKeyPair
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
    ctx startTime =
        NodeContext
        { ncSystemStart = startTime
        , ncSecretKey = npSecretKey
        , ncVssKeyPair = npVssKeyPair
        }
    runCH :: Timestamp -> ContextHolder m a -> m a
    runCH startTime = flip runReaderT (ctx startTime) . getContextHolder
    runTimed :: LoggerNameBox TimedIO a -> IO a
    runTimed = runTimedIO . usingLoggerName npLoggerName
    runDH :: NodeState -> DBHolder m a -> m a
    runDH db = flip runReaderT db . getDBHolder
