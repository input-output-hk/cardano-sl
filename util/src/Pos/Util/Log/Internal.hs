{-# LANGUAGE RecordWildCards #-}

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
       , getLogContext
       , incrementLinesLogged
       , modifyLinesLogged
       , LoggingHandler      --  only export name
       , FileDescription (..)
       , mkFileDescription
       ) where

import           Control.AutoUpdate (UpdateSettings (..), defaultUpdateSettings,
                     mkAutoUpdate)
import           Control.Concurrent.MVar (modifyMVar_, newMVar, withMVar)

import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime)
import           System.FilePath (splitFileName, (</>))
import           Universum hiding (newMVar)

import qualified Katip as K
import qualified Katip.Core as KC

import           Pos.Util.Log.LoggerConfig (LoggerConfig (..))
import           Pos.Util.Log.Severity


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

-- | log files have a prefix and a name
data FileDescription = FileDescription {
                         prefixpath :: FilePath,
                         filename   :: FilePath }
                       deriving (Show)

mkFileDescription :: FilePath -> FilePath -> FileDescription
mkFileDescription bp fp =
    -- if fp contains a filename in a directory path
    --    move this path to the prefix and only keep the name
    let (extbp, fname) = splitFileName fp
    in
    FileDescription { prefixpath = bp </> extbp
                    , filename = fname }

-- | Our internal state
data LoggingHandlerInternal = LoggingHandlerInternal
    { lsiConfig      :: !(Maybe LoggerConfig)
    , lsiLogEnv      :: !(Maybe K.LogEnv)
    , lsiLogContext  :: !(Maybe K.LogContexts)
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

getLogContext :: LoggingHandler -> IO (Maybe K.LogContexts)
getLogContext lh = withMVar (getLSI lh) $ \LoggingHandlerInternal{..} -> return lsiLogContext

getLinesLogged :: LoggingHandler -> IO Integer
getLinesLogged lh = withMVar (getLSI lh) $ \LoggingHandlerInternal{..} -> return lsiLinesLogged

-- | from tests, we want to count the numbe of lines logged
incrementLinesLogged :: LoggingHandler -> IO ()
incrementLinesLogged lh = modifyLinesLogged lh (+1)
modifyLinesLogged :: LoggingHandler -> (Integer -> Integer) -> IO ()
modifyLinesLogged lh f = do
    LoggingHandlerInternal cfg env ctx counter <- takeMVar (getLSI lh)
    putMVar (getLSI lh) $ LoggingHandlerInternal cfg env ctx $ f counter

updateConfig :: LoggingHandler -> LoggerConfig -> IO ()
updateConfig lh lc = modifyMVar_ (getLSI lh) $ \LoggingHandlerInternal{..} ->
    return $ LoggingHandlerInternal (Just lc) lsiLogEnv lsiLogContext lsiLinesLogged

-- | create internal state given a configuration @LoggerConfig@
newConfig :: LoggerConfig -> IO LoggingHandler
newConfig lc = do
    mv <- newMVar $ LoggingHandlerInternal (Just lc) Nothing Nothing 0
    return $ LoggingHandler mv

-- | register scribes in `katip`
registerBackends :: LoggingHandler -> [(T.Text, K.Scribe)] -> IO ()
registerBackends lh scribes = do
    LoggingHandlerInternal cfg _ ctx counter <- takeMVar (getLSI lh)
    le0 <- K.initLogEnv (s2kname "cardano-sl") "production"
    -- use 'getCurrentTime' to get a more precise timestamp
    -- as katip uses per default some internal buffered time variable
    timer <- mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime, updateFreq = 10000 }
    let le1 = updateEnv le0 timer
    le <- register scribes le1
    putMVar (getLSI lh) $ LoggingHandlerInternal cfg (Just le) ctx counter
      where
        register :: [(T.Text, K.Scribe)] -> K.LogEnv -> IO K.LogEnv
        register [] le = return le
        register ((n, s):scs) le =
            register scs =<< K.registerScribe n s scribeSettings le
        updateEnv :: K.LogEnv -> IO UTCTime -> K.LogEnv
        -- request a new time 'getCurrentTime' at most 100 times a second
        updateEnv le timer =
            le { K._logEnvTimer = timer, K._logEnvHost = "hostname" }

scribeSettings :: KC.ScribeSettings
scribeSettings = KC.ScribeSettings bufferSize
  where
    bufferSize = 5000   -- size of the queue (in log items)
