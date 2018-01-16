-- | Workers for delegation logic.

module Pos.Delegation.Worker
       ( dlgWorkers
       ) where

import           Universum

import           Control.Lens ((%=))
import           Data.Time.Clock (UTCTime, addUTCTime)
import           Mockable (CurrentTime, Delay, Mockable, currentTime, delay)
import           Serokell.Util (sec)

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, localWorker)
import           Pos.Delegation.Class (MonadDelegation, dwMessageCache)
import           Pos.Delegation.Configuration (HasDlgConfiguration, dlgMessageCacheTimeout)
import           Pos.Delegation.Logic (DelegationStateAction, runDelegationStateAction)
import           Pos.Reporting (MonadReporting, reportOrLogE)
import           Pos.Shutdown (HasShutdownContext)
import           Pos.Util (microsecondsToUTC)
import           Pos.Util.LRU (filterLRU)

-- | This is a subset of 'WorkMode'.
type DlgWorkerConstraint ctx m
     = ( MonadIO m
       , MonadDelegation ctx m
       , MonadMask m
       , Mockable Delay m
       , HasShutdownContext ctx
       , MonadDelegation ctx m
       , MonadReporting ctx m
       , MonadReader ctx m
       , Mockable CurrentTime m
       , HasDlgConfiguration)


-- | All workers specific to proxy sertificates processing.
dlgWorkers :: (DlgWorkerConstraint ctx m) => ([WorkerSpec m], OutSpecs)
dlgWorkers = first pure $ localWorker dlgInvalidateCaches

-- | Runs proxy caches invalidating action every second.
dlgInvalidateCaches :: DlgWorkerConstraint ctx m => m ()
dlgInvalidateCaches =
    -- When dlgInvalidateCaches calls itself directly, it leaks memory. The
    -- reason for that is that reference to dlgInvalidateCaches is kept in
    -- memory (by usage of dlgWorkers) and as it is executed it expands
    -- indefinitely, hence more and more space is needed to store it. Using fix
    -- fixes the problem as it makes dlgInvalidateCaches itself finite in
    -- size. Relevant GHC ticket: https://ghc.haskell.org/trac/ghc/ticket/13080
    fix $ \loop -> do
        -- REPORT:ERROR 'reportOrLogE' in delegation worker.
        invalidate `catchAny` reportOrLogE "Delegation worker, error occurred: "
        delay (sec 1)
        loop
  where
    invalidate = do
        curTime <- microsecondsToUTC <$> currentTime
        runDelegationStateAction $ invalidateProxyCaches curTime

-- | Invalidates proxy caches using built-in constants.
invalidateProxyCaches :: HasDlgConfiguration => UTCTime -> DelegationStateAction ()
invalidateProxyCaches curTime =
    dwMessageCache %=
        filterLRU (\t -> addUTCTime (toDiffTime dlgMessageCacheTimeout) t > curTime)
  where
    toDiffTime (t :: Integer) = fromIntegral t
