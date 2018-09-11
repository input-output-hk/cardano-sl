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
    -- context: cardano-sl.testing
    usingLoggerName "testing" $ do
        testLog
        -- context: cardano-sl.testing.more
        modifyLoggerName (<> ".more") $ do
            testLog
            -- context: cardano-sl.final
            modifyLoggerName (const "final") $ do
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
    modifyMaxSuccess (const 1) $ modifyMaxSize (const 1) $
      it "demonstrate logging" $
        monadicIO $ lift $ someLogging

