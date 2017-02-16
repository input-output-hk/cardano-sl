{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QDisc.FairSTM where

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Network.Transport.TCP (QDisc(..))

fairQDisc :: IO (QDisc t)
fairQDisc = do
    fqd <- newFairQDisc
    return $ QDisc {
          qdiscEnqueue = \_ _ -> writeFairQDisc fqd
        , qdiscDequeue = readFairQDisc fqd
        }

newFairQDisc :: IO (FairQDisc a)
newFairQDisc = atomically $ do
    reads <- newTVar []
    wake <- newEmptyTMVar
    writes <- newTVar ([], wake)
    return $ FairQDisc reads writes


-- | Fair FIFO QDisc where each write blocks until its value enters the read
--   queue.
--   It's essentially a TQueue (classic purely-functional queue in TVars)
--   but with writes that block until the dequeue and enqueue lists are
--   swapped.
data FairQDisc a = FairQDisc {
      fqdiscReading :: !(TVar [a])
    , fqdiscWriting :: !(TVar ([a], TMVar ()))
    }

-- | Read from a FairQDisc. This will contend with writers only if there is
--   no data left to read.
readFairQDisc :: FairQDisc a -> IO a
readFairQDisc FairQDisc{..} = atomically $ do
    toRead <- readTVar fqdiscReading
    case toRead of
        (r : rs) -> do
            writeTVar fqdiscReading rs
            return r
        [] -> do
            -- Grab the written data.
            -- If there's none, wait for somebody to fill it.
            (written, wake) <- readTVar fqdiscWriting
            -- Reverse the list to give FIFO.
            case reverse written of
                -- Will wake up on the next write.
                [] -> retry
                -- There's written data! Swap it into the read queue and
                -- immediately yield the first one.
                (w : ws) -> do
                    wake' <- newEmptyTMVar
                    writeTVar fqdiscReading ws
                    writeTVar fqdiscWriting ([], wake')
                    putTMVar wake ()
                    return w

-- | Write to a FairQDisc.
writeFairQDisc :: FairQDisc a -> a -> IO ()
writeFairQDisc FairQDisc{..} !x = do
    wait <- atomically $ do
        (xs, wait) <- readTVar fqdiscWriting
        writeTVar fqdiscWriting (x : xs, wait)
        return wait
    atomically $ readTMVar wait
