module Cardano.Wallet.WalletLayer.ExecutionTimeLimit (
      limitExecutionTimeTo
    , TimeExecutionLimit(..)
    ) where

import           Universum

import           Formatting (bprint, shown, (%))
import qualified Formatting.Buildable

import           Test.QuickCheck (Arbitrary (..), Gen, Positive (..))

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (AsyncCancelled (..))
import qualified Control.Concurrent.Async as Async
import           Control.Exception (evaluate)
import           Data.Time.Units (Second)

data TimeExecutionLimit =
    TimeExecutionLimitReached Second
    deriving (Eq, Show)

instance Exception TimeExecutionLimit

instance Buildable TimeExecutionLimit where
    build (TimeExecutionLimitReached secs) =
        bprint ("TimeExecutionLimitReached " % shown) secs

instance Arbitrary TimeExecutionLimit where
    arbitrary = TimeExecutionLimitReached . fromIntegral
                                          . getPositive <$> (arbitrary :: Gen (Positive Int))

limitExecutionTimeTo :: Second
                     -> (TimeExecutionLimit -> b)
                     -> IO (Either b a)
                     -> IO (Either b a)
limitExecutionTimeTo secs onTimeLimit action = do
    let onCancellation = \AsyncCancelled ->
            return $ Left (onTimeLimit (TimeExecutionLimitReached secs))
    let limited = (action >>= evaluate) `catch` onCancellation
    result <- Async.race limited (threadDelay (1000000 * fromIntegral secs))
    case result of
         Left actionResult -> return actionResult
         Right ()          -> return $ Left (onTimeLimit (TimeExecutionLimitReached secs))

