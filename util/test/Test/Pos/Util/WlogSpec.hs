module Test.Pos.Util.WlogSpec
    ( spec)
where

import           Universum

import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck.Monadic (monadicIO)

import           Pos.Util.Log.LoggerConfig (defaultInteractiveConfiguration)
import           Pos.Util.Wlog

someLogging :: IO ()
someLogging = do
    setupLogging loggerConfig
    usingLoggerName "testing" $ do
        testLog
  where
    loggerConfig :: LoggerConfig
    loggerConfig = defaultInteractiveConfiguration Debug

testLog :: (MonadIO m, WithLogger m) => m ()
testLog = do
    logDebug "debug"
    logInfo "info"
    logNotice "notice"
    logWarning "warning"
    logError "error"

spec :: Spec
spec = describe "Logging" $ do
    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "demonstrate logging" $
        monadicIO $ lift $ someLogging

