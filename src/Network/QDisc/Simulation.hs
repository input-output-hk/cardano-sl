{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import System.Environment (getArgs)
import Control.Monad
import Data.Word (Word32)
import Data.Time.Units
import Data.Time.Clock.POSIX
import Data.Vector (Vector, fromList)
import qualified Data.Vector as V (length)
import Data.IORef
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import qualified Statistics.Sample as Sample
import Statistics.Distribution
import Statistics.Distribution.Uniform
import Statistics.Distribution.Normal
import Statistics.Distribution.Exponential
import System.Random.MWC
import Network.Transport.TCP (QDisc(..), simpleOnePlaceQDisc, simpleUnboundedQDisc)
import Network.QDisc.Fair
import qualified Network.QDisc.FairSTM as STM

-- | A writer is determined by some continuous distribution giving the duration
--   (in microseconds) between successive data being made available.
data SimulationWriter = forall distr . ContGen distr => SimulationWriter distr

-- | A reader is determined by some continuous distribution giving the duration
--   (in microseconds) between successive reads (how long the reader thread
--   works between taking events).
data SimulationReader = forall distr . ContGen distr => SimulationReader distr

data SimulationParameters = SimulationParameters {
      -- | 1 reader.
      sim_reader :: SimulationReader
      -- | 0 or more writers.
    , sim_writers :: [SimulationWriter]
      -- | How long the simulation should run.
    , sim_duration :: Second
      -- | The QDisc to use.
    , sim_qdisc :: QDisc ()
      -- | A seed for randomness.
    , sim_seed :: Word32
    }

type Latency = Double

-- | The output of a simulation is, for each writer, the samples of actual
--   delays observed when trying to write: how long it was blocked on
--   trying to enqueue. The number of samples is the number of writes it
--   made.
data SimulationOutput = SimulationOutput {
      sim_writer_outputs :: [Vector Latency]
    }

instance Show SimulationOutput where
    show (SimulationOutput vecs) = concat $ flip fmap vecs $ \vec -> concat [
          "Samples (writes): ", show (V.length vec), "\n"
        , "Mean: ", show (Sample.mean vec), "\n"
        , "Std. Dev.: ", show (Sample.stdDev vec), "\n\n"
        ]

data QDiscChoice = Fair | FairSTM | OnePlace | Unbounded

makeQDisc :: QDiscChoice -> IO (QDisc t)
makeQDisc choice = case choice of
    Fair -> fairQDisc
    FairSTM -> STM.fairQDisc
    Unbounded -> simpleUnboundedQDisc
    OnePlace -> simpleOnePlaceQDisc

simpleSimulationParameters :: QDisc () -> Second -> SimulationParameters
simpleSimulationParameters qdisc duration = SimulationParameters {
      -- Some exponentially-distributed write delays.
      -- Denominators give their means.
      --
      -- With these writers and reader, the fair QDisc actually outperforms
      -- the one-place QDisc.
      {-
      sim_writers = [
            SimulationWriter (exponential (1/10))
          , SimulationWriter (exponential (1/20))
          , SimulationWriter (exponential (1/30))
          , SimulationWriter (exponential (1/40))
          , SimulationWriter (uniformDistr 10 40)
          ]
      -- Reader typically takes 50us between reads with little variability.
    , sim_reader = SimulationReader (normalDistr 50 2)
      -}
      {-
      -- With this one, fair and one-place are essentially the same: both
      -- fair and equally performant.
      sim_writers = [
            SimulationWriter (uniformDistr 0 100)
          , SimulationWriter (uniformDistr 5000 25000)
          , SimulationWriter (uniformDistr 5000 25000)
          , SimulationWriter (uniformDistr 5000 25000)
          , SimulationWriter (uniformDistr 5000 25000)
          ]
    , sim_reader = SimulationReader (normalDistr 5000 50)
      -}
      -- Difference between fair and one-place is very clear here.
      -- The two fast writers get the same number of writes in either case.
      -- One-place gives the slow writer half as many writes, but fair gives
      -- it a lot more. The cycle time (time to read from all 3 writers) is
      -- the slow writer's write delay, so we expect that the slow writer
      -- should be able to write on every cycle.
      sim_reader = SimulationReader (uniformDistr 5000 5001)
    , sim_writers = [
            SimulationWriter (uniformDistr 4999 5000)
          , SimulationWriter (uniformDistr 4999 5000)
          , SimulationWriter (uniformDistr 14999 15000)
          ]
    , sim_duration = duration
    , sim_qdisc = qdisc
    , sim_seed = 42
    }

-- | Run a simulation.
simulate :: SimulationParameters -> IO SimulationOutput
simulate SimulationParameters{..} = do

    -- All threads will wait on this.
    -- Threads will loop, and at each iteration will read it. If it's True,
    -- they'll stop.
    startStop :: MVar Bool <- newEmptyMVar

    -- Spawn the reader.
    refs <- withAsync (reader 0 sim_reader) $ \readerThread -> do
        -- Spawn the writers.
        writerThreadsAndRefs <- forM (zip [1..] sim_writers) $ \(seed', sim_writer) -> do
            (ref, doWrites) <- writer startStop seed' sim_writer
            --withAsync doWrites $ \thread -> return (ref, thread)
            thread <- async doWrites
            return (ref, thread)
        -- Duration is in seconds.
        putMVar startStop False
        threadDelay $ fromIntegral sim_duration * 1000000
        swapMVar startStop True
        forM writerThreadsAndRefs $ \(ref, thread) -> do
            wait thread
            return ref

    -- Reader and writers are all killed. Results available in the IORefs.
    vectors <- forM refs (fmap fromList . readIORef)

    return $ SimulationOutput ((fmap . fmap) fromIntegral vectors)

    where

    reader :: Word32 -> SimulationReader -> IO ()
    reader seed' (SimulationReader distribution) = do
        gen <- initialize (fromList [sim_seed, seed'])
        let readLoop = do
                () <- qdiscDequeue sim_qdisc
                -- Unlike for writers, the reader delays are always respected,
                -- as the delay is independent of how long the reader waits for
                -- a value from qdiscDequeue.
                delay :: Microsecond <- fromIntegral . round <$> genContVar distribution gen
                threadDelay (fromIntegral delay)
                readLoop
        readLoop

    writer :: MVar Bool -> Word32 -> SimulationWriter -> IO (IORef [Microsecond], IO ())
    writer control seed' (SimulationWriter distribution) = do
        ref <- newIORef []
        gen <- initialize (fromList [sim_seed, seed'])
        -- The writer's distribution determines the duration between the points
        -- in time when new data are made available, and this is independent
        -- of how long it takes to actually do the write. For instance, if
        -- the writer is blocked on enqueue for 2 seconds, and the distribution
        -- says there will be data available every 1 second, then 2 data will
        -- be immediately available after the enqueue finishes.
        let writeLoop :: Microsecond -> IO ()
            writeLoop surplusWait = do
                stop <- readMVar control
                unless stop $ do
                    nextDelay :: Microsecond <- fromIntegral . round <$> genContVar distribution gen
                    let actualDelay = nextDelay - surplusWait
                    when (actualDelay > 0) $ do
                        threadDelay (fromIntegral actualDelay)
                    start <- getPOSIXTime
                    qdiscEnqueue sim_qdisc undefined undefined ()
                    end <- getPOSIXTime
                    let latency :: Microsecond
                        !latency = fromIntegral . round $ (realToFrac (end - start) :: Double) * 1000000
                    modifyIORef' ref ((:) latency)
                    let !surplusWait' = max 0 (surplusWait + latency - nextDelay)
                    writeLoop surplusWait'
        return (ref, writeLoop 0)

main :: IO ()
main = do
    args <- getArgs
    qdiscChoice <- case args of
        "unbounded" : _ -> putStrLn "Using unbounded QDisc" >> return Unbounded
        "one_place" : _ -> putStrLn "Using one-place QDisc" >> return OnePlace
        "fair_stm" : _ -> putStrLn "Using fair STM QDisc" >> return FairSTM
        _ -> putStrLn "Using fair QDisc" >> return Fair
    duration :: Int <- case args of
        _ : n : _ -> case reads n of
            [(n', "")] -> return n'
        _ -> return 10
    qdisc <- makeQDisc qdiscChoice
    results <- simulate (simpleSimulationParameters qdisc (fromIntegral duration))
    print results
