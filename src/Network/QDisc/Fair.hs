{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QDisc.Fair where

import Data.IORef
import Control.Concurrent.MVar
import Network.Transport.TCP (QDisc(..))

-- | A QDisc which is intended to always give fair queueing, even when there
--   are multiple writers writing faster than the reader can read.
fairQDisc :: IO (QDisc t)
fairQDisc = do
    fqd <- newFairQDisc
    return $ QDisc {
          qdiscEnqueue = \_ _ -> writeFairQDisc fqd
        , qdiscDequeue = readFairQDisc fqd
        }

newFairQDisc :: IO (FairQDisc t)
newFairQDisc = do
    cycle <- newIORef []
    state <- newMVar Idle
    return $ FairQDisc cycle state

-- | Fair FIFO QDisc where each write blocks until its value enters the read
--   queue.
data FairQDisc a = FairQDisc {
      -- An 'IORef' is suitable here because only one thread will modify
      -- it. 
      fqdiscCycle :: {-# UNPACK #-} !(IORef (FairQDiscCycle a))
    , fqdiscState :: {-# UNPACK #-} !(MVar (FairQDiscState a))
    }

type FairQDiscCycle a = [a]

data FairQDiscState a =
      Idle
      -- | The reader is waiting for a value (taking this 'MVar').
    | ReaderStarved {-# UNPACK #-} !(MVar a)
      -- | 1 or more writers have written and are waiting for the reader
      --   to clear this cycle (reading this 'MVar').
    | WritersBlocked !a ![a] {-# UNPACK #-} !(MVar ())

-- | Read from a FairQDisc. This will contend with writers only if there is
--   no data left to read.
readFairQDisc :: FairQDisc a -> IO a
readFairQDisc FairQDisc{..} = do
    xs <- readIORef fqdiscCycle
    case xs of
        -- The cycle is not over: update the cycle and return the next value.
        (x : xs') -> do
            writeIORef fqdiscCycle xs'
            return x
        -- The cycle is empty: try to take the next one, or wait until there's
        -- a write.
        [] -> do
            choice <- modifyMVar fqdiscState $ \st -> case st of
                Idle -> do
                    wakeReader <- newEmptyMVar
                    return (ReaderStarved wakeReader, Left wakeReader)
                WritersBlocked x xs wakeWriters -> do
                    putMVar wakeWriters ()
                    return (Idle, Right (x, xs))
                ReaderStarved _ -> error "multiple readers"
            case choice of
                Left wakeReader -> takeMVar wakeReader
                Right (x, xs) -> do
                    modifyIORef' fqdiscCycle (const xs)
                    return x

-- | Write to a FairQDisc. It blocks until a reader takes the written value
--   into a cycle.
writeFairQDisc :: FairQDisc a -> a -> IO ()
writeFairQDisc FairQDisc{..} !x = do
    choice <- modifyMVar fqdiscState $ \st -> case st of
        Idle -> do
            wakeWriters <- newEmptyMVar
            return (WritersBlocked x [] wakeWriters, Just wakeWriters)
        WritersBlocked x' xs wakeWriters ->
            return (WritersBlocked x (x' : xs) wakeWriters, Just wakeWriters)
        ReaderStarved wakeReader -> do
            putMVar wakeReader x
            return (Idle, Nothing)
    case choice of
        Nothing -> return ()
        Just wakeWriters -> readMVar wakeWriters
