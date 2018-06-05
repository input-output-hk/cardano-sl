-- | 'Trace' backed by log-warper for unstructured logging.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Pos.Util.Trace.Wlog
    ( LogNamed (..)
    , LoggerName (..)
    , wlogTrace
    , modifyName
    , appendName
    , setName
    , named
    ) where

import           Universum
import           Data.Functor.Contravariant (Op (..), contramap)
-- Trivia: Universum exports isPrefixOf, but not isSuffixOf.
import           Data.List (isSuffixOf)
import           System.Wlog (LoggerName (..), logMCond)
import           System.Wlog.LogHandler (LogHandlerTag (..))
import qualified System.Wlog as Wlog (Severity (..))

import           Pos.Util.Trace (Trace (..))
import           Pos.Util.Trace.Unstructured (Severity (..), LogItem (..), LogPrivacy (..))

-- | Attach a 'LoggerName' to something.
data LogNamed item = LogNamed
    { lnName :: LoggerName
    , lnItem :: item
    }

modifyName
    :: (LoggerName -> LoggerName)
    -> Trace m (LogNamed i)
    -> Trace m (LogNamed i)
modifyName k = contramap f
  where
    f (LogNamed name item) = LogNamed (k name) item

appendName :: LoggerName -> Trace m (LogNamed i) -> Trace m (LogNamed i)
appendName lname = modifyName (<> lname)

setName :: LoggerName -> Trace m (LogNamed i) -> Trace m (LogNamed i)
setName name = modifyName (const name)

-- | Use a 'LogNamed'. A typical usage pattern is
--
--     named . appendName "world" . appendName "hello" $ wlogTrace
--       :: Trace IO LogItem
--
--   which will use the "hello"."world" logger name.
--
named :: Trace m (LogNamed i) -> Trace m i
named = contramap (LogNamed mempty)

wlogSeverity :: Severity -> Wlog.Severity
wlogSeverity Debug = Wlog.Debug
wlogSeverity Warning = Wlog.Warning
wlogSeverity Error = Wlog.Error
wlogSeverity Info = Wlog.Info
wlogSeverity Notice = Wlog.Notice

-- | A general log-warper-backed 'Trace', which allows for logging to public,
-- private, or both, and the choice of a 'LoggerName'.
-- NB: log-warper uses global shared mutable state. You have to initialize it
-- or else 'wlogTrace' won't do anything.
wlogTrace :: Trace IO (LogNamed LogItem)
wlogTrace = Trace $ Op $ \namedLogItem ->
    let privacy = liPrivacy (lnItem namedLogItem)
        loggerName = lnName namedLogItem
        severity = wlogSeverity (liSeverity (lnItem namedLogItem))
        message = liMessage (lnItem namedLogItem)
     in case privacy of
            Private  -> logMCond loggerName severity message selectPrivateLogs
            Public   -> logMCond loggerName severity message selectPublicLogs
            Both     -> logMCond loggerName severity message selectBoth
  where
    selectPrivateLogs :: LogHandlerTag -> Bool
    selectPrivateLogs = not . selectPublicLogs
    selectPublicLogs :: LogHandlerTag -> Bool
    selectPublicLogs lt = case lt of
        HandlerFilelike p -> ".pub" `isSuffixOf` p
        _ -> False
    selectBoth :: LogHandlerTag -> Bool
    selectBoth = const True
