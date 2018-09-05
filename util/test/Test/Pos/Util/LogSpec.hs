module Test.Pos.Util.LogSpec
    ( spec)
where

import           Universum hiding (replicate)

import           Control.Concurrent (threadDelay)

import qualified Data.HashMap.Strict as HM
import           Data.Text (append, replicate)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Test.Hspec (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck (Property, property)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Pos.Util.Log.LoggerConfig (BackendKind (..), LogHandler (..),
                     LogSecurityLevel (..), LoggerConfig (..), LoggerTree (..),
                     defaultInteractiveConfiguration, defaultTestConfiguration,
                     lcLoggerTree, ltMinSeverity, ltNamedSeverity)
import           Pos.Util.Wlog (Severity (..), WithLogger, getLinesLogged,
                     logDebug, logError, logInfo, logNotice, logWarning,
                     setupLogging, usingLoggerName)
--import           Pos.Util.Log.LogSafe (logDebugS, logErrorS, logInfoS,
--                     logNoticeS, logWarningS)

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
        -- assert (linesLogged >= n0 * n1 * 5 `div` 2)    -- weaker

  {-
-- | Count as many lines as you intended to log.
prop_sev :: Property
prop_sev =
    monadicIO $ do
        let n0 = 20
            n1 = 1
        (_, linesLogged) <- run (run_logging Warning 10 n0 n1)
        -- multiply by 2 because Debug, Info and Notice messages must not be logged
        assert (linesLogged == n0 * n1 * 2)
        -- assert (linesLogged >= n0 * n1 * 2 `div` 2)    -- weaker
-}
run_logging :: Severity -> Int -> Integer -> Integer -> IO (Microsecond, Integer)
run_logging _ n n0 n1= do
        startTime <- getPOSIXTime
        --setupLogging $ defaultTestConfiguration sev
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
        threadDelay $ fromIntegral (5000 * n0)
        let diffTime = nominalDiffTimeToMicroseconds (endTime - startTime)
        putStrLn $ "  time for " ++ (show (n0*n1)) ++ " iterations: " ++ (show diffTime)
        lineslogged1 <- getLinesLogged
        let lineslogged = lineslogged1 - lineslogged0
        putStrLn $ "  lines logged :" ++ (show lineslogged)
        return (diffTime, lineslogged)
        where msg :: Text
              msg = replicate n "abcdefghijklmnopqrstuvwxyz"

  {-
prop_sevS :: Property
prop_sevS =
    monadicIO $ do
        let n0 = 200
            n1 = 1
        (_, linesLogged) <- run (run_loggingS Warning 10 n0 n1)
        -- multiply by 2 because Debug, Info and Notice messages must not be logged
        assert (linesLogged == 0)

run_loggingS :: Severity -> Int -> Integer -> Integer-> IO (Microsecond, Integer)
run_loggingS sev n n0 n1= do
        startTime <- getPOSIXTime
        --setupLogging $ defaultTestConfiguration sev
        forM_ [1..n0] $ \_ ->
            usingLoggerName "test_log" $
                forM_ [1..n1] $ \_ -> do
                    logDebugS   lh msg
                    logInfoS    lh msg
                    logNoticeS  lh msg
                    logWarningS lh msg
                    logErrorS   lh msg
        endTime <- getPOSIXTime
        threadDelay 0500000
        let diffTime = nominalDiffTimeToMicroseconds (endTime - startTime)
        putStrLn $ "  time for " ++ (show (n0*n1)) ++ " iterations: " ++ (show diffTime)
        linesLogged <- getLinesLogged
        putStrLn $ "  lines logged :" ++ (show linesLogged)
        return (diffTime, linesLogged)
        where msg :: Text
              msg = replicate n "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
-}
-- | example: setup logging
example_setup :: IO ()
example_setup = do
    --setupLogging (defaultTestConfiguration Debug)
    usingLoggerName "processXYZ" $ do
        logInfo "entering"
        complexWork "42"
        logInfo "done."

    where
        complexWork :: WithLogger m => Text -> m ()
        complexWork m = do
            logDebug $ "let's see: " `append` m

{-
-- | example: bracket logging
example_bracket :: IO ()
example_bracket = do
    setupLogging (defaultTestConfiguration Debug)
    loggerBracket "processXYZ" $ do
        logInfo "entering"
        complexWork "42"
        logInfo "done."

    where
        complexWork :: WithLogger m => Text -> m ()
        complexWork m =
            addLoggerName "in_complex" $ do
                logDebug $ "let's see: " `append` m
-}
spec :: Spec
spec = describe "Logging" $ do
    modifyMaxSuccess (const 1) $ modifyMaxSize (const 1) $
      it "setup logging" $
        monadicIO $ do
            let lc0 = defaultTestConfiguration Debug
                newlt = lc0 ^. lcLoggerTree & ltNamedSeverity .~ HM.fromList [("cardano-sl.silent", Error)]
                lc = lc0 & lcLoggerTree .~ newlt
            setupLogging lc

    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "measure time for logging small messages" $
        property prop_small

    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "measure time for logging LARGE messages" $
        property prop_large

    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "lines counted as logged must be equal to how many was intended to be written" $
        property prop_lines

{-
    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "Debug, Info and Notice messages must not be logged" $
        property prop_sev
-}

{-  disabled for now
    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "DebugS, InfoS, NoticeS, WarningS and ErrorS messages must not be logged in public logs" $
        property prop_sevS
-}
    it "demonstrating setup and initialisation of logging" $
        example_setup

{-  disabled for now
    it "demonstrating bracket logging" $
        example_bracket
-}
    it "compose default LoggerConfig" $
        ((mempty :: LoggerConfig) <> (LoggerConfig { _lcBasePath = Nothing, _lcRotation = Nothing
                                             , _lcLoggerTree = mempty }))
        `shouldBe`
        (mempty :: LoggerConfig)

    it "compose LoggerConfig - minimum of severities" $ do
        let lc1 = (LoggerConfig { _lcBasePath = Nothing, _lcRotation = Nothing
                        , _lcLoggerTree = LoggerTree {_ltMinSeverity = Info,
                                                      _ltNamedSeverity = HM.empty,
                             _ltHandlers = [LogHandler {_lhName = "file1.log", _lhFpath = Just "file1.log",
                                                        _lhSecurityLevel = Just PublicLogLevel,
                                                        _lhBackend = FileTextBE,
                                                        _lhMinSeverity = Just Info
                                                       }]}
                        })
            lc2 = (LoggerConfig { _lcBasePath = Nothing, _lcRotation = Nothing
                        , _lcLoggerTree = LoggerTree {_ltMinSeverity = Warning,
                                                      _ltNamedSeverity = HM.empty,
                             _ltHandlers = [LogHandler {_lhName = "file2.log", _lhFpath = Just "file2.log",
                                                        _lhSecurityLevel = Just PublicLogLevel,
                                                        _lhBackend = FileTextBE,
                                                        _lhMinSeverity = Just Error
                                                       }]}
                        })
            lc3 = lc1 <> lc2
        lc3 ^. lcLoggerTree . ltMinSeverity `shouldBe` Info

    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "compose complex LoggerConfig" $
        ( (defaultInteractiveConfiguration Debug) <>
          (LoggerConfig { _lcBasePath = Nothing, _lcRotation = Nothing
                                             , _lcLoggerTree = mempty }))
        `shouldBe`
        (defaultInteractiveConfiguration Debug) <>
          (LoggerConfig { _lcBasePath = Nothing, _lcRotation = Nothing
                        , _lcLoggerTree = LoggerTree {_ltMinSeverity = Debug,
                                                      _ltNamedSeverity = HM.empty,
                             _ltHandlers = [LogHandler {_lhName = "console", _lhFpath = Nothing,
                                                        _lhSecurityLevel = Just PublicLogLevel,
                                                        _lhBackend = StdoutBE,
                                                        _lhMinSeverity = Just Debug
                                                       }]}
                        })

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

