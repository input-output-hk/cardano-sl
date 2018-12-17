module Test.Pos.Util.WlogSpec
    ( spec)
where

import           Universum hiding (replicate)

import           Control.Concurrent (threadDelay)

import qualified Data.HashMap.Strict as HM
import           Data.Text (replicate)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck (Property, property)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Pos.Util.Log.LoggerConfig (defaultTestConfiguration,
                     lcLoggerTree, ltNamedSeverity)
import           Pos.Util.Wlog (Severity (..), WithLogger, addLoggerName,
                     getLinesLogged, logDebug, logError, logInfo, logNotice,
                     logWarning, setLoggerName, setupLogging, usingLoggerName)

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

nominalDiffTimeToMicroseconds :: POSIXTime -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . round . (* 1000000)

prop_small :: Property
prop_small =
    monadicIO $ do
        (diffTime,_) <- run (run_logging Debug 1 20 10)
        assert (diffTime > 0)

prop_large :: Property
prop_large =
    monadicIO $ do
        (diffTime,_) <- run (run_logging Debug 100 200 100)
        assert (diffTime > 0)

-- | Count as many lines as you intended to log.
prop_lines :: Property
prop_lines =
    monadicIO $ do
        let n0 = 20
            n1 = 1
        (_, linesLogged) <- run (run_logging Debug 10 n0 n1)
        -- multiply by 5 because we log 5 different messages (n0 * n1) times
        assert (linesLogged == n0 * n1 * 5)

run_logging :: Severity -> Int -> Integer -> Integer -> IO (Microsecond, Integer)
run_logging _ n n0 n1= do
        startTime <- getPOSIXTime
        lineslogged0 <- getLinesLogged
        forM_ [1..n0] $ \_ ->
            usingLoggerName "test_log" $
                forM_ [1..n1] $ \_ -> do
                    logDebug msg
                    logInfo msg
                    logNotice msg
                    logWarning msg
                    logError msg
        endTime <- getPOSIXTime
        threadDelay $ fromIntegral (8000 * n0)
        let diffTime = nominalDiffTimeToMicroseconds (endTime - startTime)
        putStrLn $ "  time for " ++ (show (n0*n1)) ++ " iterations: " ++ (show diffTime)
        lineslogged1 <- getLinesLogged
        let lineslogged = lineslogged1 - lineslogged0
        putStrLn $ "  lines logged :" ++ (show lineslogged)
        threadDelay 0500000   -- wait for empty queue
        return (diffTime, lineslogged)
        where msg :: Text
              msg = replicate n "abcdefghijklmnopqrstuvwxyz"

someLogging :: IO ()
someLogging = do
    -- context: cardano-sl.testing
    usingLoggerName "testing" $ do
        testLog
        -- context: cardano-sl.testing.more
        addLoggerName "more" $ do
            testLog
            -- context: cardano-sl.final
            setLoggerName "final" $ do
                testLog

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
      it "setup logging" $
        monadicIO $ do
            let lc0 = defaultTestConfiguration Debug
                newlt = lc0 ^. lcLoggerTree & ltNamedSeverity .~ HM.fromList [("cardano-sl.silent", Error)]
                lc = lc0 & lcLoggerTree .~ newlt
            setupLogging "test" lc

    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "change minimum severity filter for a specific context" $
        monadicIO $ do
            lineslogged0 <- lift $ getLinesLogged
            lift $ usingLoggerName "silent" $ do { logWarning "you won't see this!" }
            lift $ threadDelay 0300000
            lift $ usingLoggerName "verbose" $ do { logWarning "now you read this!" }
            lift $ threadDelay 0300000
            lineslogged1 <- lift $ getLinesLogged
            let lineslogged = lineslogged1 - lineslogged0
            putStrLn $ "lines logged: " ++ (show lineslogged)
            assert (lineslogged == 1)

    modifyMaxSuccess (const 1) $ modifyMaxSize (const 1) $
      it "demonstrate logging" $
        monadicIO $ lift $ someLogging

    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "measure time for logging small messages" $
        property prop_small

    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "measure time for logging LARGE messages" $
        property prop_large

    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "lines counted as logged must be equal to how many was intended to be written" $
        property prop_lines

