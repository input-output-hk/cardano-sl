module Test.Pos.Util.LogSpec
    ( spec)
where

import           Universum hiding (replicate)

import           Control.Concurrent (threadDelay)

import           Data.Text (replicate)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Core.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck (Property, property)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Pos.Util.Log
import           Pos.Util.LoggerConfig (defaultTestConfiguration)
import           Pos.Util.Log.Internal (getLinesLogged)


nominalDiffTimeToMicroseconds :: POSIXTime -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . round . (* 1000000)

prop_small :: Property
prop_small =
    monadicIO $ do
        (diffTime,_) <- run (run_logging 1 20 10)
        assert (diffTime > 0)

prop_large :: Property
prop_large =
    monadicIO $ do
        (diffTime,_) <- run (run_logging 100 200 100)
        assert (diffTime > 0)

-- | Count as many lines as you itented to log.
prop_lines :: Property
prop_lines =
    monadicIO $ do
        let n0 = 20
            n1 = 1
        (_, linesLogged) <- run (run_logging 10 n0 n1)
        -- multiply by 5 because we log 5 different messages (no * n1) times
        assert (linesLogged == n0 * n1 * 5)

run_logging :: Int -> Integer -> Integer-> IO (Microsecond, Integer)
run_logging n n0 n1= do
        startTime <- getPOSIXTime
{- -}
        setupLogging defaultTestConfiguration
        forM_ [1..n0] $ \_ ->
            usingLoggerName "test_log" $
                forM_ [1..n1] $ \_ -> do
                    logDebug msg
                    logInfo msg
                    logNotice msg
                    logWarning msg
                    logError msg
{- -}
        endTime <- getPOSIXTime
        threadDelay 0500000
        diffTime <- return $ nominalDiffTimeToMicroseconds (endTime - startTime)
        putStrLn $ "  time for " ++ (show (n0*n1)) ++ " iterations: " ++ (show diffTime)
        linesLogged <- getLinesLogged
        putStrLn $ "  lines logged :" ++ (show linesLogged)
        return (diffTime, linesLogged)
        where msg :: Text
              msg = replicate n "abcdefghijklmnopqrstuvwxyz"

spec :: Spec
spec = describe "Log" $ do
    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $ it "measure time for logging small messages" $ property prop_small
    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $ it "measure time for logging LARGE messages" $ property prop_large
    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $ it "lines counted as logged must be    equal to how many was itended to be written" $ property prop_lines
