module Test.Pos.Util.TraceSpec
    ( spec)
where

import           Universum hiding (replicate)

import           Control.Concurrent (threadDelay)
import           Control.Monad (when)

import           Data.Text (append, replicate)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck (Property, property)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Pos.Util.Log as Log
import           Pos.Util.Log.Internal (getLinesLogged)
import           Pos.Util.LoggerConfig (defaultInteractiveConfiguration,
                     defaultTestConfiguration)
import qualified Pos.Util.Trace as Tr
import qualified Pos.Util.Trace.Named as Tn
import qualified Pos.Util.Trace.Unstructured as Tu

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

nominalDiffTimeToMicroseconds :: POSIXTime -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . round . (* 1000000)

prop_small :: Property
prop_small =
    monadicIO $ do
        (diffTime,_) <- run (run_logging Log.Debug 1 20 10)
        assert (diffTime > 0)

prop_large :: Property
prop_large =
    monadicIO $ do
        (diffTime,_) <- run (run_logging Log.Debug 100 200 100)
        assert (diffTime > 0)

-- | Count as many lines as you itented to log.
prop_lines :: Property
prop_lines =
    monadicIO $ do
        let n0 = 20
            n1 = 1
        (_, linesLogged) <- run (run_logging Log.Debug 10 n0 n1)
        -- multiply by 5 because we log 5 different messages (no * n1) times
        assert (linesLogged == n0 * n1 * 5)

-- | Count as many lines as you itented to log.
prop_sev :: Property
prop_sev =
    monadicIO $ do
        let n0 = 20
            n1 = 1
        (_, linesLogged) <- run (run_logging Log.Warning 10 n0 n1)
        -- multiply by 2 because Debug, Info and Notice messages must not be logged
        assert (linesLogged == n0 * n1 * 2)

-- | Count as many lines as you itented to log.
prop_sevS :: Property
prop_sevS =
    monadicIO $ do
        let n0 = 20
            n1 = 1
        (_, linesLogged) <- run (run_loggingS Log.Warning 10 n0 n1)
        -- multiply by 2 because Debug, Info and Notice messages must not be logged
        assert (linesLogged == 0)

run_logging :: Log.Severity -> Int -> Integer -> Integer -> IO (Microsecond, Integer)
run_logging sev n n0 n1= do
    startTime <- getPOSIXTime
{- -}
    lh <- Log.setupLogging (defaultTestConfiguration sev)
    let logTrace' = Tr.logTrace lh "processXYZ"
    forM_ [1..n0] $ \_ ->
        forM_ [1..n1] $ \_ -> do
            Tr.traceWith (Tr.logDebug logTrace') msg
            Tr.traceWith (Tr.logInfo logTrace') msg
            Tr.traceWith (Tr.logNotice logTrace') msg
            Tr.traceWith (Tr.logWarning logTrace') msg
            Tr.traceWith (Tr.logError logTrace') msg
{- -}
    endTime <- getPOSIXTime
    threadDelay $ fromIntegral (5000 * n0)
    let diffTime = nominalDiffTimeToMicroseconds (endTime - startTime)
    putStrLn $ "  time for " ++ (show (n0*n1)) ++ " iterations: " ++ (show diffTime)
    linesLogged <- getLinesLogged lh
    putStrLn $ "  lines logged :" ++ (show linesLogged)
    return (diffTime, linesLogged)
    where msg :: Text
          msg = replicate n "abcdefghijklmnopqrstuvwxyz"

run_loggingS :: Log.Severity -> Int -> Integer -> Integer -> IO (Microsecond, Integer)
run_loggingS sev n n0 n1= do
    startTime <- getPOSIXTime
{- -}
    lh <- Log.setupLogging (defaultTestConfiguration sev)
    let logTrace' = Tn.appendName "run_loggingS" $ Tn.namedTrace lh

    Tn.logInfo logTrace' "entering"
    forM_ [1..n0] $ \_ ->
        forM_ [1..n1] $ \_ -> do
            Tn.logDebugS logTrace' msg
            Tn.logInfoS logTrace' msg
            Tn.logNoticeS logTrace' msg
            Tn.logWarningS logTrace' msg
            Tn.logErrorS logTrace' msg
{- -}
    endTime <- getPOSIXTime
    threadDelay $ fromIntegral (5000 * n0)
    let diffTime = nominalDiffTimeToMicroseconds (endTime - startTime)
    putStrLn $ "  time for " ++ (show (n0*n1)) ++ " iterations: " ++ (show diffTime)
    linesLogged <- getLinesLogged lh
    putStrLn $ "  lines logged :" ++ (show linesLogged)
    return (diffTime, linesLogged)
    where msg :: Text
          msg = replicate n "abcdefghijklmnopqrstuvwxyz"

-- | example: setup trace
example_setup :: IO ()
example_setup = do
    logTrace' <- Tr.setupLogging (defaultTestConfiguration Log.Debug) "example"
    Tr.traceWith logTrace' (Log.Info, "entering")
    complexWork logTrace' "42"
    Tr.traceWith logTrace' (Log.Info, "done.")
    where
        --complexWork :: MonadIO m => TraceIO -> Text -> m ()
        complexWork tr msg = do
            Tr.traceWith tr (Log.Debug, "let's see: " `append` msg)

-- | example: unstructured trace
example_unstructured :: IO ()
example_unstructured = do
    logTrace' <- Tu.setupLogging (defaultTestConfiguration Log.Debug) "unstructured"
    Tu.logInfo logTrace' "entering"
    complexWork logTrace' "42"
    Tu.logInfo logTrace' "done."
    where
        --complexWork :: MonadIO m => TraceIO -> Text -> m ()
        complexWork tr msg = do
            Tu.logDebug tr ("let's see: " `append` msg)

-- | example: named context trace
example_named :: IO ()
example_named = do
    logTrace' <- Tn.setupLogging (defaultInteractiveConfiguration Log.Debug) "named"
    Tn.logInfo logTrace' "entering"
    complexWork (Tn.appendName "complex" logTrace') "42"
    -- ^ the named context will include "complex" in the logged message
    Tn.logInfo logTrace' "done."
    where
        --complexWork :: MonadIO m => TraceIO -> Text -> m ()
        complexWork tr msg = do
            Tn.logDebug tr ("let's see: " `append` msg)
            when (msg == "42") $
                 complexWork (Tn.appendName "work" tr) "done."


spec :: Spec
spec = describe "Trace" $ do
    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "measure time for logging small messages" $
        property prop_small

    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "measure time for logging LARGE messages" $
        property prop_large

    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "lines counted as logged must be equal to how many was itended to be written" $
        property prop_lines

    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "Debug, Info and Notice messages must not be logged" $
        property prop_sev

    modifyMaxSuccess (const 2) $ modifyMaxSize (const 2) $
      it "DebugS, InfoS, NoticeS, WarningS and ErrorS messages must not be logged in public logs" $
        property prop_sevS

    it "demonstrating setup and initialisation of logging" $
        example_setup

    it "demonstrating unstructured logging" $
        example_unstructured

    it "demonstrating named context logging" $
        example_named

