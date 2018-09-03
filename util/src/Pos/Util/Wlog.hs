{-# LANGUAGE Rank2Types #-}

-- | an interface to 'log-warper'
--  functions and types gradually migrate towards 'katip'

module Pos.Util.Wlog
        ( -- * CanLog
          CanLog (..)
        , WithLogger
          -- * Pure logging
        , dispatchEvents        -- call sites: 1 chain/src/Pos/Chain/Ssc/Toss/Pure.hs
        , LogEvent (..)         -- call sites: 3 chain/src/Pos/Chain/Ssc/Toss/Pure.hs,db/src/Pos/DB/Update/Poll/Pure.hs,wallet-new/test/unit/UTxO/Verify.hs
        , NamedPureLogger (..)
        , launchNamedPureLog    -- call sites: 8   chain,db,explorer
        , runNamedPureLog       -- call sites: 2   chain,db
          -- * Setup
        , setupLogging          -- call sites: 7 generator,lib,networking(bench),tools:keygen|launcher
          -- * Logging functions
        , logDebug
        , logError
        , logInfo
        , logNotice
        , logWarning
        , logMessage            -- call sites: 3  infra,wallet-new
          -- * LoggerName
        , LoggerName (..)
        , LoggerNameBox (..)    -- call sites: 9  core,db,infra,lib,tools,util
        , HasLoggerName (..)
        , usingLoggerName       -- call sites: 22 db,explorer,infra,lib,networking,node-ipc,tools,wallet,wallet-new
          -- * LoggerConfig
        , LoggerConfig (..)     -- call sites: 13  (mostly together with 'setupLogging')
        , lcLogsDirectory       -- call sites: 1 tools/src/launcher/Main.hs
        , lcTermSeverityOut     -- call sites: 1 tools/src/launcher/Main.hs
        , lcTree                -- call sites: 4 infra,lib,networking,tools
        , parseLoggerConfig     -- call sites: 2 lib,networking
          -- * Hierarchical tree of loggers (with lenses)
        , HandlerWrap (..)      -- call sites: 1 tools/src/launcher/Main.hs
        , fromScratch           -- call sites: 1 networking/src/Bench/Network/Commons.hs
        , hwFilePath            -- call sites: 1 infra/src/Pos/Infra/Reporting/Wlog.hs
        , ltFiles               -- call sites: 2 infra/.../Reporting/Wlog.hs,tools/src/launcher/Main.hs
        , ltSeverity            -- call sites: 5 networking/src/Bench/Network/Commons.hs,tools/src/launcher/Main.hs
        , zoomLogger            -- call sites: 3 networking/src/Bench/Network/Commons.hs
        , ltSubloggers          -- call sites: 1 infra/src/Pos/Infra/Reporting/Wlog.hs
          -- * Builders for 'LoggerConfig'
        , consoleActionB        -- call sites: 3 generator/app/VerificationBench.hs,lib/src/Pos/Launcher/Resource.hs
        , maybeLogsDirB         -- call sites: 2 lib/src/Pos/Launcher/Resource.hs,networking/src/Bench/Network/Commons.hs
        , showTidB              -- call sites: 1 lib/src/Pos/Launcher/Resource.hs
        , productionB           -- call sites: 6 lib,networking,tools
        , termSeveritiesOutB    -- call sites: 2 generator/app/VerificationBench.hs,tools/src/keygen/Main.hs
          -- * Severity
        , Severity (..)
        , debugPlus             -- call sites: 4 generator/app/VerificationBench.hs,tools:keygen|launcher
        , errorPlus             -- call sites: 1 networking/src/Bench/Network/Commons.hs
        , infoPlus              -- call sites: 2 networking/src/Bench/Network/Commons.hs
        , noticePlus            -- call sites: 1 networking/src/Network/Broadcast/OutboundQueue/Demo.hs
        , warningPlus           -- call sites: 1 networking/src/Bench/Network/Commons.hs
          -- * Saving Changes
        , retrieveLogContent    -- call sites: 1 infra/src/Pos/Infra/Reporting/Wlog.hs
        , updateGlobalLogger    -- call sites: 1 networking/src/Network/Broadcast/OutboundQueue/Demo.hs
          -- * LogHandler
        , defaultHandleAction   -- call sites: 2 generator/app/VerificationBench.hs,lib/src/Pos/Launcher/Resource.hs
          -- * Logging messages with a condition
        , logMCond              -- call sites: 1 core/src/Pos/Core/Util/LogSafe.hs
          -- * LogHandler
        , LogHandlerTag (HandlerFilelike)  -- call sites: 1 core/src/Pos/Core/Util/LogSafe.hs
          -- * Utility functions
        , removeAllHandlers     -- call sites: 2 lib/src/Pos/Launcher/Resource.hs,networking/test/Test/Network/Broadcast/OutboundQueueSpec.hs
        , centiUtcTimeF         -- call sites: 1 networking/bench/LogReader/Main.hs
        , setLevel              -- call sites: 1 networking/src/Network/Broadcast/OutboundQueue/Demo.hs
        ) where

import           System.Wlog (HandlerWrap (..), HasLoggerName (..),
                     LoggerConfig (..), LoggerName (..), LoggerNameBox (..),
                     Severity (..), consoleActionB, debugPlus,
                     defaultHandleAction, errorPlus, fromScratch, hwFilePath,
                     infoPlus, lcLogsDirectory, lcTermSeverityOut, lcTree,
                     logMCond, ltFiles, ltSeverity, ltSubloggers,
                     maybeLogsDirB, modifyLoggerName, noticePlus,
                     parseLoggerConfig, productionB, removeAllHandlers,
                     retrieveLogContent, setLevel, setupLogging, showTidB,
                     termSeveritiesOutB, updateGlobalLogger, usingLoggerName,
                     warningPlus, zoomLogger)
import           System.Wlog.Formatter (centiUtcTimeF)
import           System.Wlog.LogHandler (LogHandlerTag (HandlerFilelike))

--import qualified Pos.Util.Log as Log

import           Universum

import           Control.Monad.Morph (MFunctor (..))
import qualified Control.Monad.State.Lazy as StateLazy
import           Data.Sequence ((|>))
import           Data.Text (Text)

{-
  attempt for reducing complexity:

  1. count call sites per function
  2. comment out 0 usages
  3. group by "common usage pattern"
      * just logging functions
      * setup logging
      * logging configuration
      * pure logger?
      * only 'CanLog' imported
      * CanLog and WithLogger imported => redundant
      * CanLog and HasLoggerName imported => replace with 'WithLogger'
-}

{- compatibility -}

-- setupLogging will setup global shared logging state (MVar)
-- usingLoggerName launches an action (of 'LoggerNameBox m a' ~ 'ReaderT LoggerName m a')
-- LoggerNameBox is an instance of CanLog, HasLoggerName
-- our functions are running in this context: 'type WithLogger m = (CanLog m, HasLoggerName m)'

-- this fails with 'MonadIO' redundant constraint
-- type WithLogger m = (CanLog m, HasLoggerName m, Log.LogContext m)

{-
class GHC.Base.Monad m => CanLog (m :: * -> *) where
  dispatchMessage :: LoggerName
                     -> Severity -> Data.Text.Internal.Text -> m ()
  default dispatchMessage :: (Control.Monad.Trans.Class.MonadTrans t,
                              t n ~ m, CanLog n) =>
                             LoggerName -> Severity -> Data.Text.Internal.Text -> m ()
  -- Defined in ‘System.Wlog.CanLog’
instance GHC.Base.Monad m => CanLog (NamedPureLogger m)
  -- Defined in ‘System.Wlog.PureLogging’
instance CanLog m => CanLog (LoggerNameBox m)
  -- Defined in ‘System.Wlog.CanLog’
-}
class Monad m => CanLog m where
    dispatchMessage :: LoggerName -> Severity -> Text -> m ()

    default dispatchMessage :: (MonadTrans t, t n ~ m, CanLog n)
                            => LoggerName
                            -> Severity
                            -> Text
                            -> m ()
    dispatchMessage name sev t = lift $ dispatchMessage name sev t

instance CanLog m => CanLog (LoggerNameBox m)
instance CanLog m => CanLog (ReaderT r m)
instance CanLog m => CanLog (StateT s m)
instance CanLog m => CanLog (StateLazy.StateT s m)
instance CanLog m => CanLog (ExceptT s m)
instance CanLog IO where
    dispatchMessage ln _ t = putStrLn $ "[" ++ (show ln) ++ "] " ++ (show t)

type WithLogger m = (CanLog m, HasLoggerName m)

{- dummies for now -}
logDebug, logError, logInfo, logNotice, logWarning
  :: (CanLog m) => Text -> m ()
logDebug _ = return ()
logInfo _ = return ()
logError _ = return ()
logNotice _ = return ()
logWarning _ = return ()

logMessage :: (CanLog m) => Severity -> Text -> m ()
logMessage _ _ = return ()

{-
type role LoggerNameBox representational nominal
newtype LoggerNameBox (m :: * -> *) a
  = LoggerNameBox {loggerNameBoxEntry :: ReaderT LoggerName m a}
  	-- Defined in ‘System.Wlog.LoggerNameBox’
instance Applicative m => Applicative (LoggerNameBox m)
  -- Defined in ‘System.Wlog.LoggerNameBox’
instance Functor m => Functor (LoggerNameBox m)
  -- Defined in ‘System.Wlog.LoggerNameBox’
instance Monad m => Monad (LoggerNameBox m)
  -- Defined in ‘System.Wlog.LoggerNameBox’
instance MonadIO m => MonadIO (LoggerNameBox m)
  -- Defined in ‘System.Wlog.LoggerNameBox’
instance MonadTrans LoggerNameBox
  -- Defined in ‘System.Wlog.LoggerNameBox’
instance MonadCatch m => MonadCatch (LoggerNameBox m)
  -- Defined in ‘System.Wlog.LoggerNameBox’
instance MonadMask m => MonadMask (LoggerNameBox m)
  -- Defined in ‘System.Wlog.LoggerNameBox’
instance MonadReader r m => MonadReader r (LoggerNameBox m)
  -- Defined in ‘System.Wlog.LoggerNameBox’
instance MonadState s m => MonadState s (LoggerNameBox m)
  -- Defined in ‘System.Wlog.LoggerNameBox’
instance MonadThrow m => MonadThrow (LoggerNameBox m)
  -- Defined in ‘System.Wlog.LoggerNameBox’
instance Monad m => HasLoggerName (LoggerNameBox m)
  -- Defined in ‘System.Wlog.LoggerNameBox’
-}

{-
class HasLoggerName (m :: * -> *) where
  askLoggerName :: m LoggerName
  default askLoggerName :: (MonadTrans t, t n ~ m, Monad n,
                            HasLoggerName n) =>
                           m LoggerName
  modifyLoggerName :: (LoggerName -> LoggerName) -> m a -> m a
  default modifyLoggerName :: (mmorph-1.1.2:Control.Monad.Morph.MFunctor
                                 t,
                               t n ~ m, Monad n, HasLoggerName n) =>
                              (LoggerName -> LoggerName) -> m a -> m a
  	-- Defined in ‘System.Wlog.HasLoggerName’
instance Monad m => HasLoggerName (NamedPureLogger m)
  -- Defined in ‘System.Wlog.PureLogging’
instance (Monad m, HasLoggerName m) => HasLoggerName (StateT a m)
  -- Defined in ‘System.Wlog.HasLoggerName’
instance (Monad m, HasLoggerName m) => HasLoggerName (ReaderT a m)
  -- Defined in ‘System.Wlog.HasLoggerName’
instance HasLoggerName Identity
  -- Defined in ‘System.Wlog.HasLoggerName’
instance (Monad m, HasLoggerName m) => HasLoggerName (ExceptT e m)
  -- Defined in ‘System.Wlog.HasLoggerName’
instance Monad m => HasLoggerName (LoggerNameBox m)
  -- Defined in ‘System.Wlog.LoggerNameBox’
-}

{-
launchNamedPureLog ::
  (System.Wlog.CanLog.WithLogger n, Monad m) =>
  (forall (f :: * -> *). Functor f => m (f a) -> n (f b))
  -> NamedPureLogger m a -> n b
  	-- Defined in ‘System.Wlog.PureLogging’
-}
launchNamedPureLog
    :: (WithLogger n, Monad m)
    => (forall f. Functor f => m (f a) -> n (f b))
    -> NamedPureLogger m a
    -> n b
launchNamedPureLog hoist' namedPureLogger = do
    name <- askLoggerName
    (logs, res) <- hoist' $ swap <$> usingNamedPureLogger name namedPureLogger
    res <$ dispatchEvents logs

usingNamedPureLogger :: Functor m
                     => LoggerName
                     -> NamedPureLogger m a
                     -> m (a, [LogEvent])
usingNamedPureLogger name (NamedPureLogger action) =
    usingLoggerName name $ runPureLog action

data LogEvent = LogEvent
    { leLoggerName :: !LoggerName
    , leSeverity   :: !Severity
    , leMessage    :: !Text
    } deriving (Show)

runPureLog :: Functor m => PureLogger m a -> m (a, [LogEvent])
runPureLog = fmap (second toList) . usingStateT mempty . runPureLogger

-- |
newtype PureLogger m a = PureLogger
    { runPureLogger :: StateT (Seq LogEvent) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadState (Seq LogEvent),
                MonadThrow, HasLoggerName)

instance Monad m => CanLog (PureLogger m) where
    dispatchMessage name severity message = modify' (|> (LogEvent name severity message))
instance MFunctor PureLogger where
    hoist f = PureLogger . hoist f . runPureLogger

-- |
newtype NamedPureLogger m a = NamedPureLogger
    { runNamedPureLogger :: PureLogger (LoggerNameBox m) a
    } deriving (Functor, Applicative, Monad, MonadState (Seq LogEvent),
                MonadThrow, HasLoggerName)

instance MonadTrans NamedPureLogger where
    lift = NamedPureLogger . lift . lift
instance Monad m => CanLog (NamedPureLogger m) where
    dispatchMessage name sev msg =
        NamedPureLogger $ dispatchMessage name sev msg
instance MFunctor NamedPureLogger where
    hoist f = NamedPureLogger . hoist (hoist f) . runNamedPureLogger

runNamedPureLog
    :: (Monad m, HasLoggerName m)
    => NamedPureLogger m a -> m (a, [LogEvent])
runNamedPureLog (NamedPureLogger action) =
    askLoggerName >>= (`usingLoggerName` runPureLog action)

dispatchEvents :: CanLog m => [LogEvent] -> m ()
dispatchEvents = mapM_ dispatchLogEvent
  where
    dispatchLogEvent (LogEvent name sev t) = dispatchMessage name sev t

