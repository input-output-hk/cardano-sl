{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Cardano.Wallet.Client.Wait
  ( waitForSomething
  , WaitOptions(..)
  , waitOptionsPID
  , SyncResult(..)
  , SyncError(..)
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (waitEitherCatchCancel, withAsync)
import           Control.Concurrent.STM.TVar (modifyTVar', newTVar, readTVar)
import           Control.Retry
import           Criterion.Measurement (getTime, initializeTime)
import           Data.Aeson (ToJSON (..), Value (..), object, (.=))
import           Data.Default
import qualified Data.DList as DList
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Formatting (bprint, fixed, sformat, shown, stext, (%))
import           Formatting.Buildable (Buildable (..))
import           Universum

import           Cardano.Wallet.Client (ClientError (..), Resp,
                     WalletClient (..), WalletResponse (..))
import           Cardano.Wallet.ProcessUtil


data WaitOptions = WaitOptions
  { waitTimeoutSeconds  :: !(Maybe Double)  -- ^ Timeout in seconds
  , waitProcessID       :: !(Maybe ProcessID) -- ^ Wallet process ID, so that crashes are handled
  , waitIntervalSeconds :: !Double -- ^ Time between polls
  } deriving (Show, Eq)

instance Default WaitOptions where
  def = WaitOptions Nothing Nothing 1.0

data SyncResult r = SyncResult
  { syncResultError     :: !(Maybe SyncError)
  , syncResultStartTime :: !UTCTime
  , syncResultDuration  :: !Double
  , syncResultData      :: ![(Double, r)]
  } deriving (Show, Typeable, Generic)

data SyncError = SyncErrorClient ClientError
               | SyncErrorProcessDied ProcessID
               | SyncErrorTimedOut Double
               | SyncErrorException SomeException
               | SyncErrorInterrupted
               deriving (Show, Typeable, Generic)

instance Buildable SyncError where
  build (SyncErrorClient err) = bprint ("There was an error connecting to the wallet: "%shown) err
  build (SyncErrorProcessDied pid) = bprint ("The cardano-node process with pid "%shown%" has gone") pid
  build (SyncErrorTimedOut t) = bprint ("Timed out after "%fixed 1%" seconds") t
  build (SyncErrorException e) = build e
  build SyncErrorInterrupted = build ("Interrupted" :: Text)

instance ToJSON r => ToJSON (SyncResult r) where
  toJSON (SyncResult err st dur rs) =
    object $ ["data" .= toJSON rs, "start_time" .= st, "duration" .= dur] <> status err
    where
      status Nothing  = [ "success" .= True ]
      status (Just e) = [ "success" .= False, "error" .= String (show e) ]

instance ToJSON SyncError where
  toJSON e = String (show e)


-- | Really basic timing information.
time :: ((IO Double) -> IO a) -> IO (UTCTime, Double, a)
time act = do
  initializeTime
  startUTC <- getCurrentTime
  start <- getTime
  res <- act (fmap (\t -> t - start) getTime)
  finish <- getTime
  pure (startUTC, finish - start, res)

waitOptionsPID :: Maybe ProcessID -> WaitOptions
waitOptionsPID pid = def { waitProcessID = pid }

waitForSomething :: (WalletClient IO -> Resp IO a) -- ^ Action to run on wallet
                 -> (a -> IO (Bool, Text, r)) -- ^ Action to interpret wallet response
                 -> WaitOptions
                 -> WalletClient IO -- ^ Wallet client
                 -> IO (SyncResult r)
waitForSomething req check WaitOptions{..} wc = do
  rv <- atomically $ newTVar DList.empty
  (start, dur, res) <- time $ \getElapsed -> do
    withAsync (timeoutSleep waitTimeoutSeconds) $ \sleep -> do
      withAsync (retrying policy (check' rv getElapsed) action) $ \poll -> cancelOnExit poll $
        waitEitherCatchCancel sleep poll

  rs <- atomically $ readTVar rv

  let e = case res of
            Left _ -> SyncErrorTimedOut <$> waitTimeoutSeconds
            Right (Left err) -> Just (SyncErrorException err)
            Right (Right (True, _)) -> (SyncErrorProcessDied <$> waitProcessID)
            Right (Right (_, Left err)) -> (Just (SyncErrorClient err))
            _ -> Nothing

  pure $ SyncResult e start dur (DList.toList rs)

  where
    policy = constantDelay (toMicroseconds waitIntervalSeconds)

    -- Run the given action and test that the server is still running
    action _st = (,) <$> checkProcessExists waitProcessID <*> req wc

    -- Interpret result of action, log some info, decide whether to continue
    check' rv getElapsed _st (_, Right resp) = do
      (unfinished, msg, res) <- check (wrData resp)
      elapsed <- getElapsed
      atomically $ modifyTVar' rv (flip DList.snoc (elapsed, res))
      when unfinished $
        putStrLn $ sformat (fixed 2%" "%stext) elapsed msg
      pure unfinished
    check' _ _ _st (False, Left _) = do
      putStrLn $ sformat "Wallet is no longer running"
      pure False
    check' _ _ _st (True, Left err) = do
      putStrLn $ sformat ("Error connecting to wallet: "%shown) err
      pure True

-- | Sleep for the given time in seconds, or indefinitely.
timeoutSleep :: Maybe Double -> IO ()
timeoutSleep (Just s) = threadDelay (toMicroseconds s)
timeoutSleep Nothing  = forever $ threadDelay (toMicroseconds 1000)

-- | Convert seconds to microseconds
toMicroseconds :: Double -> Int
toMicroseconds s = floor (s * oneSec)

oneSec :: Num a => a
oneSec = 1000000
