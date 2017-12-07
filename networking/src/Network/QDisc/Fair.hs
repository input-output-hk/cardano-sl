{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.QDisc.Fair (

      fairQDisc

    ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM
import           Data.Time.Units (Microsecond)
import           Network.Transport (EndPointAddress)
import           Network.Transport.TCP (QDisc (..))

-- | Make a fair 'QDisc'. It's called fair, but it can also be unfair if you
--   want it to be, by having some 'EndPointAddress's delay before writing.
--   If the policy is 'const (return Nothing)' then the 'QDisc' is fair.
fairQDisc
    :: (EndPointAddress -> IO (Maybe Microsecond)) -- ^ Mutable delay policy.
    -> IO (QDisc t)
fairQDisc getDelay = do
    fqd <- newFairQDisc
    return QDisc {
          qdiscEnqueue = \addr _ t -> do
              getDelay addr >>= maybe (return ()) (threadDelay . fromIntegral)
              writeFairQDisc fqd t
        , qdiscDequeue = readFairQDisc fqd
        }

newFairQDisc :: IO (FairQDisc a)
newFairQDisc = atomically $ do
    reading <- newTVar []
    wake <- newEmptyTMVar
    writing <- newTVar ([], wake)
    return $ FairQDisc reading writing

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
