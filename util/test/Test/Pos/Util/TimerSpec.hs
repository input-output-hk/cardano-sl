module Test.Pos.Util.TimerSpec
    ( spec) where

import           Universum hiding (newTVarIO)
import           Control.Concurrent.STM (newTVarIO, writeTVar, readTVar, retry)
import           Control.Concurrent.Async (async)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds, toMicroseconds)
import           Test.Hspec (Spec, describe, it)
import           Test.QuickCheck (Property, Gen, property, choose)
import           Test.QuickCheck.Monadic (forAllM, monadicIO, run, assert)

import           Pos.Util.Timer

nominalDiffTimeToMicroseconds :: POSIXTime -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . round . (* 1000000)

chooseMsc :: (Microsecond, Microsecond) -> Gen Microsecond
chooseMsc (a, b) = fromMicroseconds <$> choose (toMicroseconds a, toMicroseconds b)

-- | A timer should wait at least given interval.
prop_wait :: Property
prop_wait = 
    let tMIN = 250 :: Microsecond
        tMAX = 500 :: Microsecond
    in monadicIO
    $ forAllM (chooseMsc (tMIN, tMAX))
    $ \t -> do
        timer <- newTimer
        startTime <- run getPOSIXTime
        _ <- startTimer t timer
        _ <- atomically $ waitTimer timer
        endTime <- run getPOSIXTime
        let diffTime = nominalDiffTimeToMicroseconds (endTime - startTime)
        assert $ t <= diffTime 

-- | A timer should be additive: (re)-starting a timer @t1@ after @t0@
-- microseconds should wait at least @t0 + t1@ microseconds.
prop_additive :: Property
prop_additive = 
    let tMIN = 250 :: Microsecond
        tMAX = 500 :: Microsecond
    in monadicIO
    $ forAllM (chooseMsc (tMIN, tMAX))
    $ \tmax -> forAllM (chooseMsc (tMIN `div` 2 , tmax `div` 2))
    $ \tmin -> do
        timerMin <- newTimer
        timerMax <- newTimer
        hasBeenReset <- run $ newTVarIO True
        startTime <- run $
               startTimer tmin timerMin
            >> startTimer tmax timerMax
            >> getPOSIXTime
        -- Set the 'timerMax' once the 'timerMin' expires.
        _ <- run $ async $
               atomically (waitTimer timerMin)
            >> startTimer tmax timerMax
            >> atomically (writeTVar hasBeenReset True)
        -- Wait on the 'timerMax', and retry if the 'timerMin' has not expired
        -- and the 'timerMax' reset, to avoid the case in which the
        -- 'timerMax' finishes before it's reset by the above thread, causing
        -- a test failure.
        endTime <- run $
               atomically (waitTimer timerMax >> readTVar hasBeenReset >>= flip unless retry)
            >> getPOSIXTime
        let diffTime = nominalDiffTimeToMicroseconds (endTime - startTime)
        assert $ tmax + tmin <= diffTime

spec :: Spec
spec = describe "Timer" $ do
    it "should wait" $ property prop_wait
    it "should be additive" $ property prop_additive
