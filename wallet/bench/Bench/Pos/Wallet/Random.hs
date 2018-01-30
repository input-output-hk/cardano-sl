-- | Functions for random values.

module Bench.Pos.Wallet.Random
    ( pickRandomElementFrom
    , waitRandom
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Data.List          ((!!))
import           System.Random      (randomRIO)

-- | It is assumed that list isn't empty.
pickRandomElementFrom :: MonadIO m => [a] -> m a
pickRandomElementFrom aList = do
    someIndex <- liftIO $ randomRIO (0, length aList - 1)
    return $ aList !! someIndex

-- | Waiting some random delay.
-- Values of @from@ and @to@ are already checked:
-- both are positive and @to@ is greater than @from@.
waitRandom :: (Double, Double) -> IO ()
waitRandom (from, to) =
    randomRIO (fromInMicrosec, toInMicrosec) >>= threadDelay
  where
    fromInMicrosec, toInMicrosec :: Int
    fromInMicrosec = truncate $ from * asMicrosec
    toInMicrosec   = truncate $ to * asMicrosec
    asMicrosec = 1000000
