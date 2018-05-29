module Pos.Util.Log.Internal
       ( setConfig
       , s2kname
       , sev2klog
       , getConfig
       , getLogEnv
       , getLinesLogged
       , modifyLinesLogged
       ) where

import           Control.Concurrent.MVar (modifyMVar_, newMVar, withMVar)
import           System.IO.Unsafe (unsafePerformIO)

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
data LoggingStateInternal = LoggingStateInternal
  { lsiConfig      :: !(Maybe LoggerConfig)
  , lsiLogEnv      :: !(Maybe K.LogEnv)
  -- | Counter for the number of lines that are logged
  , lsiLinesLogged :: !Integer
  }

{-# NOINLINE makeLSI #-}
makeLSI :: MVar LoggingStateInternal
-- note: only kick up tree if handled locally
makeLSI = unsafePerformIO $ do
    let lsiConfig = Nothing
        lsiLogEnv = Nothing
        lsiLinesLogged = 0
    newMVar LoggingStateInternal {..}

getConfig :: IO (Maybe LoggerConfig)
getConfig = withMVar makeLSI $ \LoggingStateInternal{..} -> return lsiConfig

getLogEnv:: IO (Maybe K.LogEnv)
getLogEnv = withMVar makeLSI $ \LoggingStateInternal{..} -> return lsiLogEnv

getLinesLogged :: IO Integer
getLinesLogged = withMVar makeLSI $ \LoggingStateInternal{..} -> return lsiLinesLogged

modifyLinesLogged :: (Integer -> Integer) -> IO ()
modifyLinesLogged f = do
    LoggingStateInternal cfg env counter <- takeMVar makeLSI
    putMVar makeLSI $ LoggingStateInternal cfg env $ f counter

setConfig :: [(T.Text, K.Scribe)] -> LoggerConfig -> IO ()
setConfig scribes lc = modifyMVar_ makeLSI $ \LoggingStateInternal{..} -> do
    le0 <- K.initLogEnv (s2kname "cardano-sl") "production"
    le <- register scribes le0

    return $ LoggingStateInternal (Just lc) (Just le) 0
      where
        register :: [(T.Text, K.Scribe)] -> K.LogEnv -> IO K.LogEnv
        register [] le = return le
        register ((n, s):scs) le =
            register scs =<< K.registerScribe n s K.defaultScribeSettings le
