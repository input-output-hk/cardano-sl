-- | Workers for delegation logic

module Pos.Delegation.Worker
       ( dlgWorkers
       ) where

import           Control.Monad.Catch        (MonadCatch, catch)
import           Data.Time.Clock            (getCurrentTime)
import           Formatting                 (build, sformat, (%))
import           Mockable                   (Delay, Mockable, delay)
import           System.Wlog                (WithLogger, logError)
import           Universum

import           Pos.Communication.Protocol (OutSpecs, Worker, localWorker)
import           Pos.Delegation.Class       (MonadDelegation)
import           Pos.Delegation.Logic       (invalidateProxyCaches,
                                             runDelegationStateAction)
import           Pos.Util.TimeWarp          (sec)
import           Pos.WorkMode               (WorkMode)

-- | All workers specific to proxy sertificates processing.
dlgWorkers :: (WorkMode ssc m) => ([Worker m], OutSpecs)
dlgWorkers = first pure $ localWorker dlgInvalidateCaches

-- | Runs proxy caches invalidating action every second.
dlgInvalidateCaches
    :: ( MonadIO m
       , MonadDelegation m
       , MonadCatch m
       , WithLogger m
       , Mockable Delay m
       )
    => m ()
dlgInvalidateCaches = do
    invalidate `catch` handler
    delay (sec 1)
    dlgInvalidateCaches
  where
    handler :: WithLogger m => SomeException -> m ()
    handler =
        logError . sformat ("Delegation worker, error occurred: "%build)
    invalidate = do
        curTime <- liftIO getCurrentTime
        runDelegationStateAction $ invalidateProxyCaches curTime
