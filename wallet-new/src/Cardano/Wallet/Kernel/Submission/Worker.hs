module Cardano.Wallet.Kernel.Submission.Worker (
    runSubmissionLayer
    ) where

import           Universum

import           Control.Concurrent (modifyMVar_, threadDelay, withMVar)
import           Control.Concurrent.Async (wait, withAsync)
import           Control.Exception.Safe (tryJust)

import           Cardano.Wallet.Kernel.Submission (SchedulingError, WalletSubmission, tick)

import           Formatting (sformat)
import qualified Formatting as F
import           System.Wlog (Severity (..))

runSubmissionLayer :: (Severity -> Text -> IO ())
                   -> MVar (WalletSubmission IO)
                   -> IO ()
runSubmissionLayer logFunction submissionLayer = withAsync go wait
    where
      go :: IO ()
      go = do
          threadDelay 1000000 -- Wait 1 second between the next tick.
          logFunction Info "ticking..."
          res <- tryJust justSchedulingError (withMVar submissionLayer (tick throwM))
          case res of
              Left schedulingError -> do
                  handleSchedulingError schedulingError
                  go
              Right (_evicted, newState) -> do
                  modifyMVar_ submissionLayer (return . const newState)
                  -- TODO(adn) deal with evicted.
                  return ()

      handleSchedulingError :: SchedulingError -> IO ()
      handleSchedulingError se =
          logFunction Error (sformat F.shown se)

      justSchedulingError :: SomeException -> Maybe SchedulingError
      justSchedulingError = fromException
