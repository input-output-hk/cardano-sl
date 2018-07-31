-- | Workers for delegation logic.

module Pos.Worker.Delegation
       ( dlgWorkers
       ) where

import           Universum

import           Control.Lens ((%=))
import           Data.Time.Clock (UTCTime, addUTCTime)
import           Data.Time.Units (Second)

import           Pos.Chain.Delegation (HasDlgConfiguration, MonadDelegation,
                     dlgMessageCacheTimeout, dwMessageCache)
import           Pos.Core.Conc (currentTime, delay)
import           Pos.DB.Delegation (DelegationStateAction,
                     runDelegationStateAction)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Reporting (MonadReporting, reportOrLogE)
import           Pos.Infra.Shutdown (HasShutdownContext)
import           Pos.Util (microsecondsToUTC)
import           Pos.Util.LRU (filterLRU)
import           Pos.Util.Trace.Named (TraceNamed)

-- | This is a subset of 'WorkMode'.
type DlgWorkerConstraint ctx m
     = ( MonadIO m
       , MonadDelegation ctx m
       , MonadMask m
       , HasShutdownContext ctx
       , MonadDelegation ctx m
       , MonadReporting m
       , MonadReader ctx m
       , HasDlgConfiguration)


-- | All workers specific to proxy sertificates processing.
dlgWorkers
    :: (DlgWorkerConstraint ctx m)
    => TraceNamed m
    -> [Diffusion m -> m ()]
dlgWorkers logTrace = [\_ -> dlgInvalidateCaches logTrace]

-- | Runs proxy caches invalidating action every second.
dlgInvalidateCaches
    :: DlgWorkerConstraint ctx m
    => TraceNamed m
    -> m ()
dlgInvalidateCaches logTrace =
    -- When dlgInvalidateCaches calls itself directly, it leaks memory. The
    -- reason for that is that reference to dlgInvalidateCaches is kept in
    -- memory (by usage of dlgWorkers) and as it is executed it expands
    -- indefinitely, hence more and more space is needed to store it. Using fix
    -- fixes the problem as it makes dlgInvalidateCaches itself finite in
    -- size. Relevant GHC ticket: https://ghc.haskell.org/trac/ghc/ticket/13080
    fix $ \loop -> do
        -- REPORT:ERROR 'reportOrLogE' in delegation worker.
        invalidate `catchAny` reportOrLogE logTrace "Delegation worker, error occurred: "
        delay (1 :: Second)
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
