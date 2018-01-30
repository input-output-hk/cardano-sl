-- | Functions for random values.

module Bench.Pos.Wallet.Random
    ( pickRandomElementFrom
    ) where

import           Universum

import           Data.List     ((!!))
import           System.Random (randomRIO)

-- | It is assumed that list isn't empty.
pickRandomElementFrom :: MonadIO m => [a] -> m a
pickRandomElementFrom aList = do
    someIndex <- liftIO $ randomRIO (0, length aList - 1)
    return $ aList !! someIndex
