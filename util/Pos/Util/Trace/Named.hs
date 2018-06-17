-- | 'Trace' for named logging.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Pos.Util.Trace.Named
    ( LogNamed (..)
    --, namedTrace
    , setupLogging
    , modifyName
    , appendName
    , setName
    , named
    -- * log functions
    , logDebug
    , logError
    , logInfo
    , logNotice
    , logWarning
    ) where

import           Universum
import           Data.Functor.Contravariant (Op (..), contramap)
import qualified Pos.Util.Log as Log
import           Pos.Util.Trace (Trace (..), traceWith)
import qualified Pos.Util.Trace.Unstructured as TrU (LogItem (..), LogPrivacy (..))

-- | Attach a 'LoggerName' to something.
data LogNamed item = LogNamed
    { lnName :: Log.LoggerName
    , lnItem :: item
    } deriving (Show)

{-
namedItem :: Log.LoggerName -> TrU.LogItem -> LogNamed TrU.LogItem
namedItem ln li = LogNamed {lnName=ln, lnItem=li}
-}

traceNamedItem
    :: Trace m (LogNamed TrU.LogItem)
    -> TrU.LogPrivacy
    -> Log.Severity
    -> Text
    -> m ()
traceNamedItem logTrace liPrivacy liSeverity liMessage =
    traceWith (named logTrace) TrU.LogItem{..}

logDebug, logInfo, logNotice, logWarning, logError
    :: Trace m (LogNamed TrU.LogItem) -> Text -> m ()
logDebug logTrace   = traceNamedItem logTrace TrU.Both Log.Debug
logInfo logTrace    = traceNamedItem logTrace TrU.Both Log.Info
logNotice logTrace  = traceNamedItem logTrace TrU.Both Log.Notice
logWarning logTrace = traceNamedItem logTrace TrU.Both Log.Warning
logError logTrace   = traceNamedItem logTrace TrU.Both Log.Error

modifyName
    :: (Log.LoggerName -> Log.LoggerName)
    -> Trace m (LogNamed i)
    -> Trace m (LogNamed i)
modifyName k = contramap f
  where
    f (LogNamed name item) = LogNamed (k name) item

appendName :: Log.LoggerName -> Trace m (LogNamed i) -> Trace m (LogNamed i)
appendName lname = modifyName (<> lname)

setName :: Log.LoggerName -> Trace m (LogNamed i) -> Trace m (LogNamed i)
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

-- | setup logging and return a Trace
setupLogging :: Log.LoggerConfig -> Log.LoggerName -> IO (Trace IO (LogNamed TrU.LogItem))
setupLogging lc ln = do
    lh <- Log.setupLogging lc
    let nt = namedTrace lh
    return $ setName ln nt

-- FIXME needs exporting of 'logMCond' and 'LogHandlerTag' from
-- Pos.Util.Log or rewriting wlogTrace according to Katip API.
-- This removal breaks only 'lib/src/Pos/Launcher/Runner.hs'.
-- | A general log-warper-backed 'Trace', which allows for logging to public,
-- private, or both, and the choice of a 'LoggerName'.
-- NB: log-warper uses global shared mutable state. You have to initialize it
-- or else 'wlogTrace' won't do anything.
namedTrace :: Log.LoggingHandler -> Trace IO (LogNamed TrU.LogItem)
namedTrace lh = Trace $ Op $ \namedLogitem ->
    let --privacy = liPrivacy (lnItem namedLogitem)
        loggerName = lnName namedLogitem
        severity = TrU.liSeverity (lnItem namedLogitem)
        message = TrU.liMessage (lnItem namedLogitem)
    in
    Log.usingLoggerName lh loggerName $ Log.logMessage severity message
    -- ^ pass message to underlying logging

{- testing:

logTrace' <- setupLogging (Pos.Util.LoggerConfig.defaultInteractiveConfiguration Log.Debug) "named"
let li = publicLogItem (Log.Debug, "testing")
    ni = namedItem "Tests" li

traceWith logTrace' ni
traceWith (named $ appendName "more" logTrace') li


logTrace' <- setupLogging (Pos.Util.LoggerConfig.defaultInteractiveConfiguration Log.Debug) "named"
logDebug logTrace' "hello"
logDebug (appendName "blabla" logTrace') "hello"
-}
