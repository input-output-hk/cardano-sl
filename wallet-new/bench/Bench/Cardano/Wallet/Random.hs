-- | Functions for random values.

module Bench.Cardano.Wallet.Random
    ( pickRandomValueBetween
    , pickRandomElementFrom
    , pickTwoRandomElementsFrom
    , waitRandom
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Data.List.NonEmpty ((!!))
import qualified Data.List.NonEmpty as NE
import           System.Random (Random (..), randomRIO)

pickRandomElementFrom
    :: MonadIO m
    => NonEmpty a
    -> m a
pickRandomElementFrom aList = do
    someIndex <- liftIO $ randomRIO (0, NE.length aList - 1)
    return $ aList !! someIndex

-- | It is assumed that 'aList' contains at least two elements.
pickTwoRandomElementsFrom
    :: MonadIO m
    => NonEmpty a
    -> m (a, a)
pickTwoRandomElementsFrom aList = do
    when (NE.length aList < 2) $ error "Unable to pick two random elements: too small list"
    let indexOfLastElement = NE.length aList - 1
    someIndex <- liftIO $ randomRIO (0, indexOfLastElement)
    let nextIndex = someIndex + 1
        prevIndex = someIndex - 1
        someOtherIndex = if someIndex /= indexOfLastElement then nextIndex else prevIndex
    return (aList !! someIndex, aList !! someOtherIndex)

pickRandomValueBetween :: (Random a, MonadIO m) => (a, a) -> m a
pickRandomValueBetween (minValue, maxValue) = liftIO $ randomRIO (minValue, maxValue)

-- | Waiting some random delay.
-- Values of @from@ and @to@ are already checked:
-- both are positive and @to@ is greater than @from@.
waitRandom :: (Double, Double) -> IO ()
waitRandom (from, to) =
    unless thereIsNoDelay $ randomRIO (fromInMicrosec, toInMicrosec) >>= threadDelay
  where
    fromInMicrosec, toInMicrosec :: Int
    fromInMicrosec = truncate $ from * asMicrosec
    toInMicrosec   = truncate $ to * asMicrosec
    thereIsNoDelay = fromInMicrosec == 0 && toInMicrosec == 0
    asMicrosec = 1000000
