module Cardano.Wallet.Kernel.Submission.Worker (
    tickSubmissionLayer
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import qualified Data.Set as Set
import           Formatting (sformat, (%))
import qualified Formatting as F

import           Cardano.Wallet.Kernel.Submission (Evicted, WalletSubmission)

import           System.Wlog (Severity (..))


tickSubmissionLayer :: forall m. (MonadCatch m, MonadIO m)
                    => (Severity -> Text -> m ())
                    -- ^ A logging function
                    -> IO (Evicted, WalletSubmission IO)
                    -- ^ A function to call at each 'tick'
                    -> (Evicted -> WalletSubmission IO -> IO ())
                    -- ^ A callback to run whenever we get evicted
                    -- transactions back from the submission layer,
                    -- together with the new state.
                    -> m ()
tickSubmissionLayer logFunction tick onEvicted =
    go `catch` (\(e :: SomeException) ->
                   let msg = "Terminating tickSubmissionLayer due to " % F.shown
                   in logFunction Error (sformat msg e)
               )
    where
      go :: m ()
      go = do
          logFunction Debug "ticking the slot in the submission layer..."
          (evicted, newState) <- liftIO tick
          unless (Set.null evicted) $ do
              logFunction Debug "Calling onEvicted on the evicted transactions.."
              liftIO (onEvicted evicted newState)
          liftIO $ threadDelay 20000000 -- Wait 20 seconds between the next tick.
          go
