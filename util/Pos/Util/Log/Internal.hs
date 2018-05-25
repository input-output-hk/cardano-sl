
module Pos.Util.Log.Internal
       ( setConfig
       , s2kname
       , sev2klog
       , getConfig
       , getLogEnv
       ) where

import           Universum hiding (newMVar)

import           System.IO.Unsafe (unsafePerformIO)

import           Control.Concurrent.MVar (newMVar, withMVar, modifyMVar_)

import           Pos.Util.Log.StdoutScribe (mkStdoutScribe)
import           Pos.Util.Log.Severity
import           Pos.Util.LoggerConfig

import qualified Katip                      as K


-- | translate Severity to Katip.Severity
sev2klog :: Severity -> K.Severity
sev2klog = \case
    Debug   -> K.DebugS
    Info    -> K.InfoS
    Notice  -> K.NoticeS
    Warning -> K.WarningS
    Error   -> K.ErrorS

-- | translate
s2kname :: Text -> K.Namespace
s2kname s = K.Namespace [s]


-- | a global MVar keeping our internal state
data LoggingStateInternal = LoggingStateInternal
  { lsiConfig :: Maybe LoggerConfig
  , lsiLogEnv :: Maybe K.LogEnv
  }

{-# NOINLINE makeLSI #-}
makeLSI :: MVar LoggingStateInternal
-- note: only kick up tree if handled locally
makeLSI = unsafePerformIO $ do
    let lsiConfig = Nothing
        lsiLogEnv = Nothing
    newMVar LoggingStateInternal {..}

getConfig :: IO (Maybe LoggerConfig)
getConfig = withMVar makeLSI $ \LoggingStateInternal{..} -> return lsiConfig

getLogEnv:: IO (Maybe K.LogEnv)
getLogEnv = withMVar makeLSI $ \LoggingStateInternal{..} -> return lsiLogEnv

-- | setup logging environment according to given configuration
setConfig :: LoggerConfig -> IO ()
setConfig lc@LoggerConfig{..} = modifyMVar_ makeLSI $ \LoggingStateInternal{..} -> do
    -- for now we just create a simple scribe for logging to stdout
    let minSev = Debug
        lname  = "cardano-sl"
    hScribe <- mkStdoutScribe (sev2klog minSev) K.V0
    le <- K.registerScribe "stdout" hScribe K.defaultScribeSettings =<< K.initLogEnv (s2kname lname) "production"
    return $ LoggingStateInternal (Just lc) (Just le)

