module Test.Pos.Util.LogSpec
    ( spec)
where

import           Universum hiding (replicate)

import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)

import           Data.Text (replicate)

import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Core.QuickCheck (modifyMaxSuccess, modifyMaxSize)
import           Test.QuickCheck (Property, property)
import           Test.QuickCheck.Monadic (monadicIO, run, assert)

import           Pos.Util.Log


nominalDiffTimeToMicroseconds :: POSIXTime -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . round . (* 1000000)

prop_small :: Property
prop_small =
    monadicIO $ do
        diffTime <- run (run_logging 1)
        assert (diffTime > 0)

prop_large :: Property
prop_large =
    monadicIO $ do
        diffTime <- run (run_logging 10)
        assert (diffTime > 0)

run_logging :: Int -> IO (Microsecond)
run_logging n = do
        startTime <- getPOSIXTime
{- -}
        setupLogging (mempty :: LoggerConfig)
        forM_ [1..n0] $ \_ ->
            usingLoggerName "prop_small" $
                forM_ [1..n1] $ \_ -> do
                    logDebug msg
                    logInfo msg
                    logNotice msg
                    logWarning msg
                    logError msg
{- -}
        endTime <- getPOSIXTime
        diffTime <- return $ nominalDiffTimeToMicroseconds (endTime - startTime)
        putStrLn $ "  time for " ++ (show (n0*n1)) ++ " iterations: " ++ (show diffTime) ++ " us"
        return diffTime
        where msg :: Text
              msg = replicate n "abcdefghijklmnopqrstuvwxyz"
              n0 = 2000 :: Integer
              n1 = 1000 :: Integer

spec :: Spec
spec = describe "Log" $ do
    modifyMaxSuccess (const 3) $ modifyMaxSize (const 3) $ it "measure time for logging small messages" $ property prop_small
    modifyMaxSuccess (const 3) $ modifyMaxSize (const 3) $ it "measure time for logging LARGE messages" $ property prop_large
