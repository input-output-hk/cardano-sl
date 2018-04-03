module Test.Pos.Util.TimerSpec
    ( spec) where

import           Universum
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
-- microseconds should wait @t0 + t1@ microseconds.
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
        startTime <- run $
               startTimer tmin timerMin
            >> startTimer tmax timerMax
            >> getPOSIXTime
        _ <- run $ async $
               atomically (waitTimer timerMin)
            >> startTimer tmax timerMax
        endTime <- run $
               atomically (waitTimer timerMax)
            >> getPOSIXTime
        let diffTime = nominalDiffTimeToMicroseconds (endTime - startTime)
        assert $ tmax + tmin <= diffTime

spec :: Spec
spec = describe "Timer" $ do
    it "should wait" $ property prop_wait
    it "should be additive" $ property prop_additive
