-- | Workers for delegation logic

module Pos.Delegation.Worker
       ( dlgWorkers
       ) where

import           Control.Monad.Catch        (catch)
import           Data.Time.Clock            (getCurrentTime)
import           Formatting                 (build, sformat, (%))
import           Mockable                   (Delay, Mockable, delay)
import           Paths_cardano_sl           (version)
import           Serokell.Util              (sec)
import           System.Wlog                (WithLogger, logError)
import           Universum

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, localWorker,
                                             NodeId)
import           Pos.Context.Class          (WithNodeContext)
import           Pos.Delegation.Class       (MonadDelegation)
import           Pos.Delegation.Logic       (invalidateProxyCaches,
                                             runDelegationStateAction)
import           Pos.Reporting              (MonadReportingMem)
import           Pos.Reporting.Methods      (reportingFatal)
import           Pos.Shutdown               (MonadShutdownMem, runIfNotShutdown)
import           Pos.WorkMode               (WorkMode)

-- | All workers specific to proxy sertificates processing.
dlgWorkers :: (WorkMode ssc m) => m (Set NodeId) -> ([WorkerSpec m], OutSpecs)
dlgWorkers getPeers = first pure $ localWorker (dlgInvalidateCaches getPeers)

-- | Runs proxy caches invalidating action every second.
dlgInvalidateCaches
    :: ( MonadIO m
       , MonadDelegation m
       , MonadMask m
       , WithLogger m
       , Mockable Delay m
       , WithNodeContext ssc m
       , MonadReportingMem m
       , MonadShutdownMem m
       )
    => m (Set NodeId)
    -> m ()
dlgInvalidateCaches getPeers = runIfNotShutdown $ do
    reportingFatal getPeers version invalidate `catch` handler
    delay (sec 1)
    dlgInvalidateCaches getPeers
  where
    handler :: WithLogger m => SomeException -> m ()
    handler =
        logError . sformat ("Delegation worker, error occurred: "%build)
    invalidate = do
        curTime <- liftIO getCurrentTime
        runDelegationStateAction $ invalidateProxyCaches curTime
