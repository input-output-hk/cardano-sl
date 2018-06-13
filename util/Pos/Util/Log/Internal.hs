module Pos.Util.Log.Internal
       ( newConfig
       , registerBackends
       , s2kname
       , sev2klog
       , updateConfig
       , getConfig
       , getLogEnv
       , getLinesLogged
       , incrementLinesLogged
       , modifyLinesLogged
       , LoggingHandler
       ) where

import           Control.Concurrent.MVar (modifyMVar_, newMVar, withMVar)

import qualified Data.Text as T
import           Universum hiding (newMVar)

import qualified Katip as K

import           Pos.Util.Log.Severity
import           Pos.Util.LoggerConfig (LoggerConfig)


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


-- | A global MVar keeping our internal state
data LoggingHandlerInternal = LoggingHandlerInternal
  { lsiConfig      :: !(Maybe LoggerConfig)
  , lsiLogEnv      :: !(Maybe K.LogEnv)
  -- | Counter for the number of lines that are logged
  , lsiLinesLogged :: !Integer
  }

-- | internal data structure to be passed around
type LoggingMVar = MVar LoggingHandlerInternal
newtype LoggingHandler = LoggingHandler
    {
      getLSI :: LoggingMVar
    }

getConfig :: LoggingHandler -> IO (Maybe LoggerConfig)
getConfig lh = withMVar (getLSI lh) $ \LoggingHandlerInternal{..} -> return lsiConfig

getLogEnv:: LoggingHandler -> IO (Maybe K.LogEnv)
getLogEnv lh = withMVar (getLSI lh) $ \LoggingHandlerInternal{..} -> return lsiLogEnv

getLinesLogged :: LoggingHandler -> IO Integer
getLinesLogged lh = withMVar (getLSI lh) $ \LoggingHandlerInternal{..} -> return lsiLinesLogged

incrementLinesLogged :: LoggingHandler -> IO ()
incrementLinesLogged lh = modifyLinesLogged lh (+1)
modifyLinesLogged :: LoggingHandler -> (Integer -> Integer) -> IO ()
modifyLinesLogged lh f = do
    LoggingHandlerInternal cfg env counter <- takeMVar (getLSI lh)
    putMVar (getLSI lh) $ LoggingHandlerInternal cfg env $ f counter

updateConfig :: LoggingHandler -> LoggerConfig -> IO ()
updateConfig lh lc = modifyMVar_ (getLSI lh) $ \LoggingHandlerInternal{..} -> do
    return $ LoggingHandlerInternal (Just lc) lsiLogEnv lsiLinesLogged

newConfig :: LoggerConfig -> IO LoggingHandler
newConfig lc = do
    mv <- newMVar $ LoggingHandlerInternal (Just lc) Nothing 0
    return $ LoggingHandler mv

registerBackends :: LoggingHandler -> [(T.Text, K.Scribe)] -> IO ()
registerBackends lh scribes = do
    LoggingHandlerInternal cfg _ counter <- takeMVar (getLSI lh)
    le0 <- K.initLogEnv (s2kname "cardano-sl") "production"
    le <- register scribes le0
    putMVar (getLSI lh) $ LoggingHandlerInternal cfg (Just le) counter
      where
        register :: [(T.Text, K.Scribe)] -> K.LogEnv -> IO K.LogEnv
        register [] le = return le
        register ((n, s):scs) le =
            register scs =<< K.registerScribe n s K.defaultScribeSettings le
