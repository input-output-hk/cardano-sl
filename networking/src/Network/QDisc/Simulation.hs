{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.IORef
import           Data.Time.Clock.POSIX
import           Data.Time.Units
import           Data.Vector (Vector, fromList)
import qualified Data.Vector as V (length)
import           Data.Word (Word32)
import           Network.QDisc.Fair
import           Network.Transport.TCP (QDisc (..), simpleOnePlaceQDisc, simpleUnboundedQDisc)
import           Statistics.Distribution
import           Statistics.Distribution.Exponential
import           Statistics.Distribution.Normal
import           Statistics.Distribution.Uniform
import qualified Statistics.Sample as Sample
import           System.Environment (getArgs)
import           System.Random.MWC

-- | A writer is determined by some continuous distribution giving the duration
--   (in microseconds) between successive data being made available.
data SimulationWriter = forall distr . ContGen distr => SimulationWriter distr

-- | A reader is determined by some continuous distribution giving the duration
--   (in microseconds) between successive reads (how long the reader thread
--   works between taking events).
data SimulationReader = forall distr . ContGen distr => SimulationReader distr

data Scenario = Scenario {
      sim_reader  :: SimulationReader
    , sim_writers :: [SimulationWriter]
    }

data SimulationParameters = SimulationParameters {
      sim_scenario :: Scenario
      -- | How long the simulation should run.
    , sim_duration :: Second
      -- | The QDisc to use.
    , sim_qdisc    :: QDisc ()
      -- | A seed for randomness.
    , sim_seed     :: Word32
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
        , "Latency (microseconds):\n"
        , "  Mean: ", show (Sample.mean vec), "\n"
        , "  Std. Dev.: ", show (Sample.stdDev vec), "\n\n"
        ]

data QDiscChoice = Fair | OnePlace | Unbounded

makeQDisc :: QDiscChoice -> IO (QDisc t)
makeQDisc choice = case choice of
    Fair      -> fairQDisc (const (return Nothing))
    Unbounded -> simpleUnboundedQDisc
    OnePlace  -> simpleOnePlaceQDisc

unfairScenario :: Scenario
unfairScenario = Scenario {
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
    }

-- | A normally-distributed reader (mean and std. dev. configurable) and
--   n exponentially-distributed writers (means configurable).
typicalScenario :: (Double, Double) -> [Double] -> Scenario
typicalScenario (rmean, rstd_dev) writers = Scenario {
      sim_reader = SimulationReader (normalDistr rmean rstd_dev)
    , sim_writers = flip fmap writers $ \wmean -> SimulationWriter (exponential (1/wmean))
    }

simpleSimulationParameters :: QDisc () -> Second -> Scenario -> SimulationParameters
simpleSimulationParameters qdisc duration scenario = SimulationParameters {
      sim_scenario = scenario
    , sim_duration = duration
    , sim_qdisc = qdisc
    , sim_seed = 42
    }

-- | Run a simulation.
simulate :: SimulationParameters -> IO SimulationOutput
simulate SimulationParameters{..} = do

    let Scenario{..} = sim_scenario

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
        _               -> putStrLn "Using fair QDisc" >> return Fair
    duration :: Int <- case args of
        _ : n : _ -> case reads n of
            [(n', "")] -> return n'
        _ -> return 10
    qdisc <- makeQDisc qdiscChoice
    --let scenario = typicalScenario (1000, 25) [800, 900, 1000, 1100, 1200]
    let scenario = unfairScenario
    results <- simulate $ simpleSimulationParameters qdisc (fromIntegral duration) scenario
    putStrLn ""
    print results
