-- | Workers for delegation logic

module Pos.Delegation.Worker
       ( dlgWorkers
       ) where

import           Control.Monad.Catch        (catch)
import           Data.Time.Clock            (getCurrentTime)
import           Formatting                 (build, sformat, (%))
import           Mockable                   (Delay, Mockable, delay)
import           Serokell.Util              (sec)
import           System.Wlog                (WithLogger, logError)
import           Universum

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, localWorker)
import           Pos.Delegation.Class       (MonadDelegation)
import           Pos.Delegation.Logic       (invalidateProxyCaches,
                                             runDelegationStateAction)
import           Pos.Discovery.Class        (MonadDiscovery)
import           Pos.Reporting              (HasReportingContext)
import           Pos.Reporting.Methods      (reportingFatal)
import           Pos.Shutdown               (HasShutdownContext)
import           Pos.WorkMode.Class         (WorkMode)

-- | All workers specific to proxy sertificates processing.
dlgWorkers :: (WorkMode ssc ctx m) => ([WorkerSpec m], OutSpecs)
dlgWorkers = first pure $ localWorker dlgInvalidateCaches

-- | Runs proxy caches invalidating action every second.
dlgInvalidateCaches
    :: ( MonadIO m
       , MonadDelegation ctx m
       , MonadMask m
       , WithLogger m
       , Mockable Delay m
       , HasReportingContext ctx
       , HasShutdownContext ctx
       , MonadDelegation ctx m
       , MonadDiscovery m
       , MonadReader ctx m
       )
    => m ()
dlgInvalidateCaches = do
    reportingFatal invalidate `catch` handler
    delay (sec 1)
    dlgInvalidateCaches
  where
    handler :: WithLogger m => SomeException -> m ()
    handler =
        logError . sformat ("Delegation worker, error occurred: "%build)
    invalidate = do
        curTime <- liftIO getCurrentTime
        runDelegationStateAction $ invalidateProxyCaches curTime
