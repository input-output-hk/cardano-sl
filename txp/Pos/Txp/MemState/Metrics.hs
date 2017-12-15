-- | 'StateLockMetrics' for txp.

module Pos.Txp.MemState.Metrics
    ( recordTxpMetrics
    ) where

import           Universum

import           Formatting (sformat, shown, (%))
import qualified System.Metrics as Metrics
import qualified System.Metrics.Gauge as Metrics.Gauge
import           System.Wlog (logDebug)

import           Pos.StateLock (StateLockMetrics (..))
import           Pos.System.Metrics.Constants (withCardanoNamespace)
import           Pos.Txp.Toil.Types (MemPool (_mpSize))

-- | 'StateLockMetrics' to record txp MemPool metrics.
recordTxpMetrics :: Metrics.Store -> TVar MemPool -> IO StateLockMetrics
recordTxpMetrics ekgStore memPoolVar = do
    ekgMemPoolSize        <- Metrics.createGauge (withCardanoNamespace "MemPoolSize") ekgStore
    ekgMemPoolWaitTime    <- Metrics.createGauge (withCardanoNamespace "MemPoolWaitTime_microseconds") ekgStore
    ekgMemPoolModifyTime  <- Metrics.createGauge (withCardanoNamespace "MemPoolModifyTime_microseconds") ekgStore
    ekgMemPoolQueueLength <- Metrics.createGauge (withCardanoNamespace "MemPoolQueueLength") ekgStore

    -- An exponential moving average is used for the time gauges (wait
    -- and modify durations). The parameter alpha is chosen somewhat
    -- arbitrarily.
    -- FIXME take alpha from configuration/CLI, or use a better
    -- estimator.
    let alpha :: Double
        alpha = 0.75

    -- This TxpMetrics specifies what to do when waiting on the
    -- mempool lock, when the mempool lock has been granted, and
    -- when that lock has been released. It updates EKG metrics
    -- and also logs each data point at debug level.
    pure StateLockMetrics
        { slmWait = \reason -> do
              liftIO $ Metrics.Gauge.inc ekgMemPoolQueueLength
              qlen <- liftIO $ Metrics.Gauge.read ekgMemPoolQueueLength
              logDebug $ sformat ("MemPool metrics wait: "%shown%" queue length is "%shown) reason qlen

        , slmAcquire = \timeWaited -> do
              liftIO $ Metrics.Gauge.dec ekgMemPoolQueueLength
              timeWaited' <- liftIO $ Metrics.Gauge.read ekgMemPoolWaitTime
              -- Assume a 0-value estimate means we haven't taken
              -- any samples yet.
              let new_ = if timeWaited' == 0
                        then fromIntegral timeWaited
                        else round $ alpha * fromIntegral timeWaited + (1 - alpha) * fromIntegral timeWaited'
              liftIO $ Metrics.Gauge.set ekgMemPoolWaitTime new_
              logDebug $ sformat ("MemPool metrics acquire: wait time was "%shown) timeWaited

        , slmRelease = \timeElapsed -> do
              newMemPoolSize <- _mpSize <$> readTVarIO memPoolVar
              liftIO $ Metrics.Gauge.set ekgMemPoolSize (fromIntegral newMemPoolSize)
              timeElapsed' <- liftIO $ Metrics.Gauge.read ekgMemPoolModifyTime
              let new_ = if timeElapsed' == 0
                        then fromIntegral timeElapsed
                        else round $ alpha * fromIntegral timeElapsed + (1 - alpha) * fromIntegral timeElapsed'
              liftIO $ Metrics.Gauge.set ekgMemPoolModifyTime new_
              logDebug $ sformat ("MemPool metrics release: modify time was "%shown%" size is "%shown)
                         timeElapsed newMemPoolSize
        }
