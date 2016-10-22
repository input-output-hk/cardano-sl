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

import           Control.Monad.Catch      (MonadCatch, MonadMask, MonadThrow,
                                           catch, throwM)
import           Control.TimeWarp.Logging (LoggerName, LoggerNameBox, Severity,
                                           WithNamedLogger (..), initLogging,
                                           logInfo, usingLoggerName)
import           Control.TimeWarp.Rpc     (BinaryDialog, MonadDialog,
                                           MonadTransfer, ResponseT, Transfer,
                                           runBinaryDialog, runTransfer)
import           Control.TimeWarp.Timed   (MonadTimed (..), ThreadId, TimedIO,
                                           runTimedIO)
import           Formatting               (build, sformat, (%))
import           Pos.Crypto               (PublicKey, SecretKey, VssKeyPair,
                                           VssPublicKey, toPublic,
                                           toVssPublicKey)
import           Pos.DHT                  (DHTException (..), DHTNode,
                                           DHTNodeType (..), MonadDHT (..))
import           Pos.DHT.Real             (KademliaDHT, runKademliaDHT)
import           Pos.Slotting             (MonadSlots (..), Timestamp (..),
                                           timestampF)
import           Pos.State                (MonadDB (..), NodeState,
                                           openMemState, openState)
import           Universum                hiding (ThreadId, catch)

type WorkMode m
    = ( WithNamedLogger m
      , MonadTimed m
      , MonadMask m
      , MonadIO m
      , MonadSlots m
      , MonadDB m
      , WithNodeContext m
      , MonadDHT m
      , MonadDialog m
      )

----------------------------------------------------------------------------
-- MonadDB
----------------------------------------------------------------------------

newtype DBHolder m a = DBHolder
    { getDBHolder :: ReaderT NodeState m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
               MonadMask, MonadIO, WithNamedLogger, MonadTransfer, MonadDialog)

type instance ThreadId (DBHolder m) = ThreadId m

instance Monad m => MonadDB (DBHolder m) where
    getNodeState = DBHolder ask

instance (MonadDB m, Monad m) => MonadDB (KademliaDHT m) where
    getNodeState = lift getNodeState

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
         WithNodeContext (KademliaDHT m) where
    getNodeContext = lift getNodeContext

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
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow,
               MonadCatch, MonadMask, MonadIO, WithNamedLogger, MonadDB, MonadTransfer, MonadDialog)

type instance ThreadId (ContextHolder m) = ThreadId m

instance Monad m => WithNodeContext (ContextHolder m) where
    getNodeContext = ContextHolder ask

instance MonadSlots m => MonadSlots (KademliaDHT m) where
    getSystemStartTime = lift getSystemStartTime
    getCurrentTime = lift getCurrentTime

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
    , npPort            :: !Word16
    , npDHTPort         :: !Word16
    , npDHTPeers        :: ![DHTNode]
    } deriving (Show)

----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RealMode is an instance of WorkMode which can be used to really run system.
type RealMode = KademliaDHT (ContextHolder (DBHolder (LoggerNameBox (BinaryDialog Transfer))))

-- TODO: use bracket
runRealMode :: NodeParams -> RealMode a -> IO a
runRealMode NodeParams {..} action = do
    initLogging [npLoggerName] npLoggingSeverity
    startTime <- getStartTime
    db <- (runTimed . runCH startTime) openDb
    (runTimed . runDH db . runCH startTime . runKademliaDHT DHTFull npDHTPort) $ do
      logInfo $ sformat ("Started node, joining to DHT network " %build) npDHTPeers
      joinNetwork npDHTPeers `catch` handleJoinE
      action
  where
    handleJoinE AllPeersUnavailable
      = logInfo $ sformat ("Not connected to any of peers "%build) npDHTPeers
    handleJoinE e = throwM e

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
    runTimed :: LoggerNameBox (BinaryDialog Transfer) a -> IO a
    runTimed = runTimedIO . runTransfer . runBinaryDialog . usingLoggerName npLoggerName
    runDH :: NodeState -> DBHolder m a -> m a
    runDH db = flip runReaderT db . getDBHolder
