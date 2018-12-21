{-# LANGUAGE RecordWildCards #-}

module Test.Pos.Util.LogStructuredSpec
    ( spec)
where

import           Universum

import           Data.Aeson (ToJSON (..))
import qualified Data.HashMap.Strict as HM
import           System.Directory (doesFileExist, getCurrentDirectory,
                     removeFile)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck.Monadic (monadicIO)

import           Pos.Util.Log (ToObject (..), closeLogScribes)
import           Pos.Util.Log.Internal (FileDescription (..))
import           Pos.Util.Log.LoggerConfig (BackendKind (..), LogHandler (..),
                     LogSecurityLevel (..), LoggerConfig (..), LoggerTree (..))
import           Pos.Util.Log.Rotator (latestLogFile)
import           Pos.Util.Log.Structured (logDebugX, logErrorX, logInfoX,
                     logNoticeX, logWarningX)
import           Pos.Util.Wlog
import           Pos.Util.Wlog.Compatibility (setupLogging')

someLogging :: IO ()
someLogging = do
    lh <- setupLogging' "test" loggerConfig
    usingLoggerName "test_structured_log" $ do
        testLog
        closeLogScribes lh
  where
    loggerConfig :: LoggerConfig
    loggerConfig = let
        _lcRotation = Nothing
        _lcBasePath = Nothing
        _lcLoggerTree = LoggerTree {
            _ltMinSeverity = Debug,
            _ltNamedSeverity = HM.empty,
            _ltHandlers = [ LogHandler {
                _lhBackend = StderrBE,
                _lhName = "stderr",
                _lhFpath = Nothing,
                _lhSecurityLevel = Just SecretLogLevel,
                _lhMinSeverity = Just Debug }
                          , LogHandler {
                _lhBackend = FileJsonBE,
                _lhName = "json",
                _lhFpath = Just "node.json",
                _lhSecurityLevel = Just SecretLogLevel,
                _lhMinSeverity = Just Debug}
                          ]
          }
        in
        LoggerConfig{..}

-- | a loggable data structure with JSON representation
data Loggable = Loggable {
                  _fieldN :: Int
                , _fieldS :: String
                }
                deriving (Show, Generic)
instance ToJSON Loggable
instance ToObject Loggable

testLog :: (MonadIO m, WithLogger m) => m ()
testLog = do
    logDebug "debug"
    -- the following will be shown only in JSON file
    logDebugX item

    logInfo "info"
    logInfoX item

    addLoggerName "note" $ do
        logNotice "notice"
        logNoticeX item

    logWarning "warning"
    logWarningX item

    logError "error"
    logErrorX item
  where
    item = Loggable 42 "number"

spec :: Spec
spec = describe "Strucutured logging" $ do
    modifyMaxSuccess (const 1) $ modifyMaxSize (const 1) $
      it "demonstrate structured logging (see node.json-{timestamp})" $
        monadicIO $ do
            lift $ someLogging
            dir <- liftIO $ getCurrentDirectory
            mayLogFile <- liftIO $ latestLogFile $
                FileDescription { prefixpath = dir, filename = "node.json"}
            case mayLogFile of
                Just logFile -> do
                    putStrLn ("\nContents of JSON file: " <> (show logFile) :: Text)
                    contents <- readFile logFile
                    putStrLn contents
                    liftIO $ removeFile logFile
                    liftIO $ whenM (doesFileExist "node.json") (removeFile "node.json")
                Nothing -> putStrLn ("JSON file NOT found:" :: Text)
