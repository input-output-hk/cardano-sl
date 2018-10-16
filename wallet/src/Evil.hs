module Evil where

import           Data.Time
import qualified Prelude
import           System.Environment
import           System.IO.Unsafe (unsafePerformIO)
import           Universum


evilMustTerminate :: MVar ()
evilMustTerminate = unsafePerformIO $ newEmptyMVar
{- # NOINLINE evilMustTerminate #-}

evilCounter :: IORef (UTCTime, Int)
evilCounter = unsafePerformIO $ do
    now <- getCurrentTime
    newIORef (now, 0)
{- # NOINLINE evilCounter #-}

evilTick :: IO ()
evilTick = do
    now <- getCurrentTime
    evilBlocks <- maybe 50 Prelude.read <$> lookupEnv "EVIL_BLOCKS"
    (elapsedMb, currentVal) <- atomicModifyIORef' evilCounter (modify now)
    whenJust elapsedMb $ \elapsed -> do
        putTextLn $ "======> WE REACHED " <> pretty currentVal <> ".IT TOOK US " <> pretty elapsed
    when (currentVal == evilBlocks) $ do
        putTextLn "======> WE REACHED THE EVIL COUNTER."
        putMVar evilMustTerminate ()
  where
      modify :: UTCTime
             -> (UTCTime, Int)
             -> ((UTCTime, Int), (Maybe NominalDiffTime, Int))
      modify now (oldTickTime, old) =
          case (old + 1) `mod` 1000 == 0 of
               True  -> ((now, old + 1), (Just (now `diffUTCTime` oldTickTime), old + 1))
               False -> ((oldTickTime, old + 1), (Nothing, old + 1))
