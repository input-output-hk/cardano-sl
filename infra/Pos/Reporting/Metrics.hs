{-# LANGUAGE Rank2Types #-}

-- | This module basically combines reporting functionality with metrics.

module Pos.Reporting.Metrics
       ( DistrMonitor (..)
       , recordValue
       ) where

import           Universum

import           Formatting                  (Format, sformat)
import           System.Metrics.Distribution (Distribution)
import qualified System.Metrics.Distribution as Metrics
import           System.Wlog                 (logDebug)

import           Pos.Reporting.Methods       (MonadReporting, reportMisbehaviour)

data DistrMonitor = DistrMonitor
    { dmDistr              :: !(Maybe Distribution)
    -- ^ An optional 'Distribution' where we track some value.
    , dmReportMisbehaviour :: Double -> Maybe Bool
    -- ^ A predicate which determines which values should be also
    -- reported to reporting server, apart from tracking them in ekg.
    -- 'Nothing' means that value is good and shouldn't be reported at all.
    -- 'Just isCritical' means that value should be reported and 'isCritical'
    -- flag says whether misbehaviour is critical.
    --
    -- A note on this type: one can imagine many things one can do with bad
    -- values, not only reporting it as a misbehaviour. A simple extension is
    -- to support other types of reports, e. g. error.
    -- More general approach is to permit doing arbitrary monadic action.
    -- Also we can use a custom ADT.
    -- 'Maybe Bool' was choosen as a reasonable trade-off between simplicity
    -- and generalization.
    , dmMisbehFormat       :: forall r . Format r (Double -> r)
    -- ^ A 'Format' of misbehaviour message.
    , dmDebugFormat        :: forall r . Maybe (Format r (Double -> r))
    -- ^ A 'Format' of debug message which is just logged with debug
    -- severity (if this format is 'Just' and value is not reported).
    }

-- | Add a value to the dsitribution stored in the
-- 'DistrMonitor'. Report this value if it should be reported
-- according to 'DistrMonitor'.
recordValue :: MonadReporting ctx m => DistrMonitor -> Double -> m ()
recordValue DistrMonitor {..} v = do
    whenJust dmDistr $ \distr -> liftIO $ Metrics.add distr v
    case dmReportMisbehaviour v of
        Nothing -> whenJust dmDebugFormat $ logDebug . flip sformat v
        Just isCritical ->
            -- REPORT:MISBEHAVIOUR(?) Some metric is bad.
            reportMisbehaviour isCritical (sformat dmMisbehFormat v)
