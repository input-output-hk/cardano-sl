module Cardano.Wallet.Kernel.Submission.Worker (
    tickSubmissionLayer
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import qualified Data.Set as Set

import           Cardano.Wallet.Kernel.Submission (Evicted, WalletSubmission)

import           System.Wlog (Severity (..))


tickSubmissionLayer :: forall m. (MonadCatch m, MonadIO m)
                    => (Severity -> Text -> m ())
                    -- ^ A logging function
                    -> m (Evicted, WalletSubmission m)
                    -- ^ A function to call at each 'tick'
                    -> (Evicted -> WalletSubmission m -> m ())
                    -- ^ A callback to run whenever we get evicted
                    -- transactions back from the submission layer,
                    -- together with the new state.
                    -> m ()
tickSubmissionLayer logFunction tick onEvicted = go
    where
      go :: m ()
      go = do
          liftIO $ threadDelay 1000000 -- Wait 1 second between the next tick.
          logFunction Debug "ticking..."
          (evicted, newState) <- tick
          unless (Set.null evicted) $ do
              logFunction Debug "Calling onEvicted on the evicted transactions.."
              onEvicted evicted newState
          go
