module Pos.Txp.MemState.Metrics
    ( ignoreTxpMetrics
    , recordTxpMetrics
    ) where

import           Formatting                      (sformat, shown, (%))
import qualified System.Metrics                  as Metrics
import qualified System.Metrics.Gauge            as Metrics.Gauge
import           System.Wlog                     (logDebug)
import           Universum

import           Pos.Txp.MemState.Types          (TxpMetrics(..))

-- | A TxpMetrics that never does any writes. Use it if you don't care about
-- metrics.
ignoreTxpMetrics :: TxpMetrics
ignoreTxpMetrics = TxpMetrics
    { txpMetricsWait = (const (pure ()))
    , txpMetricsAcquire = (const (pure ()))
    , txpMetricsRelease = (const (const (pure ())))
    }

-- | Record MemPool metrics.
recordTxpMetrics :: Metrics.Store -> IO TxpMetrics
recordTxpMetrics ekgStore = do
    ekgMemPoolSize        <- Metrics.createGauge "MemPoolSize" ekgStore
    ekgMemPoolWaitTime    <- Metrics.createGauge "MemPoolWaitTime" ekgStore
    ekgMemPoolModifyTime  <- Metrics.createGauge "MemPoolModifyTime" ekgStore
    ekgMemPoolQueueLength <- Metrics.createGauge "MemPoolQueueLength" ekgStore

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
    pure TxpMetrics
        { txpMetricsWait = \reason -> do
              liftIO $ Metrics.Gauge.inc ekgMemPoolQueueLength
              qlen <- liftIO $ Metrics.Gauge.read ekgMemPoolQueueLength
              logDebug $ sformat ("MemPool metrics wait: "%shown%" queue length is "%shown) reason qlen

        , txpMetricsAcquire = \timeWaited -> do
              liftIO $ Metrics.Gauge.dec ekgMemPoolQueueLength
              timeWaited' <- liftIO $ Metrics.Gauge.read ekgMemPoolWaitTime
              -- Assume a 0-value estimate means we haven't taken
              -- any samples yet.
              let new_ = if timeWaited' == 0
                        then fromIntegral timeWaited
                        else round $ alpha * fromIntegral timeWaited + (1 - alpha) * fromIntegral timeWaited'
              liftIO $ Metrics.Gauge.set ekgMemPoolWaitTime new_
              logDebug $ sformat ("MemPool metrics acquire: wait time was "%shown) timeWaited

        , txpMetricsRelease = \timeElapsed memPoolSize -> do
              liftIO $ Metrics.Gauge.set ekgMemPoolSize (fromIntegral memPoolSize)
              timeElapsed' <- liftIO $ Metrics.Gauge.read ekgMemPoolModifyTime
              let new_ = if timeElapsed' == 0
                        then fromIntegral timeElapsed
                        else round $ alpha * fromIntegral timeElapsed + (1 - alpha) * fromIntegral timeElapsed'
              liftIO $ Metrics.Gauge.set ekgMemPoolModifyTime new_
              logDebug $ sformat ("MemPool metrics release: modify time was "%shown%" size is "%shown) timeElapsed memPoolSize
        }
