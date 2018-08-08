-- | internal definitions for "Pos.Util.Log"
module Pos.Util.Log.Internal
       ( newConfig
       , registerBackends
       , s2kname
       , s2knames
       , sev2klog
       , updateConfig
       , getConfig
       , getLinesLogged
       , getLogEnv
       , incrementLinesLogged
       , modifyLinesLogged
       , LoggingHandler      --  only export name
       ) where

import           Control.Concurrent.MVar (modifyMVar_, newMVar, withMVar)

import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime)
import           Universum hiding (newMVar)

import qualified Katip as K

import           Pos.Util.Log.Severity
import           Pos.Util.LoggerConfig (LoggerConfig (..))


-- | translate Severity to @Katip.Severity@
sev2klog :: Severity -> K.Severity
sev2klog = \case
    Debug   -> K.DebugS
    Info    -> K.InfoS
    Notice  -> K.NoticeS
    Warning -> K.WarningS
    Error   -> K.ErrorS

-- | translate Name to @Katip.Namespace@
s2kname :: Text -> K.Namespace
s2kname s = K.Namespace [s]
s2knames :: [Text] -> K.Namespace
s2knames s = K.Namespace s


-- | Our internal state
data LoggingHandlerInternal = LoggingHandlerInternal
    { lsiConfig      :: !(Maybe LoggerConfig)
    , lsiLogEnv      :: !(Maybe K.LogEnv)
    -- | Counter for the number of lines that are logged
    , lsiLinesLogged :: !Integer
    }

-- | internal data structure to be passed around
type LoggingMVar = MVar LoggingHandlerInternal
newtype LoggingHandler = LoggingHandler
    { getLSI :: LoggingMVar
    }

getConfig :: LoggingHandler -> IO (Maybe LoggerConfig)
getConfig lh = withMVar (getLSI lh) $ \LoggingHandlerInternal{..} -> return lsiConfig

getLogEnv :: LoggingHandler -> IO (Maybe K.LogEnv)
getLogEnv lh = withMVar (getLSI lh) $ \LoggingHandlerInternal{..} -> return lsiLogEnv

getLinesLogged :: LoggingHandler -> IO Integer
getLinesLogged lh = withMVar (getLSI lh) $ \LoggingHandlerInternal{..} -> return lsiLinesLogged

-- | from tests, we want to count the numbe of lines logged
incrementLinesLogged :: LoggingHandler -> IO ()
incrementLinesLogged lh = modifyLinesLogged lh (+1)
modifyLinesLogged :: LoggingHandler -> (Integer -> Integer) -> IO ()
modifyLinesLogged lh f = do
    LoggingHandlerInternal cfg env counter <- takeMVar (getLSI lh)
    putMVar (getLSI lh) $ LoggingHandlerInternal cfg env $ f counter

updateConfig :: LoggingHandler -> LoggerConfig -> IO ()
updateConfig lh lc = modifyMVar_ (getLSI lh) $ \LoggingHandlerInternal{..} ->
    return $ LoggingHandlerInternal (Just lc) lsiLogEnv lsiLinesLogged

-- | create internal state given a configuration @LoggerConfig@
newConfig :: LoggerConfig -> IO LoggingHandler
newConfig lc = do
    mv <- newMVar $ LoggingHandlerInternal (Just lc) Nothing 0
    return $ LoggingHandler mv

-- | register scribes in `katip`
registerBackends :: LoggingHandler -> [(T.Text, K.Scribe)] -> IO ()
registerBackends lh scribes = do
    LoggingHandlerInternal cfg _ counter <- takeMVar (getLSI lh)
    le0 <- K.initLogEnv (s2kname "cardano-sl") "production"
    let le1 = updateEnv le0 getCurrentTime
    le <- register scribes le1
    putMVar (getLSI lh) $ LoggingHandlerInternal cfg (Just le) counter
      where
        register :: [(T.Text, K.Scribe)] -> K.LogEnv -> IO K.LogEnv
        register [] le = return le
        register ((n, s):scs) le =
            register scs =<< K.registerScribe n s K.defaultScribeSettings le
        updateEnv :: K.LogEnv -> IO UTCTime -> K.LogEnv
        updateEnv le f = le { K._logEnvTimer = f }
