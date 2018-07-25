{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
module System.Remote.Monitoring.Prometheus
  ( registerEKGStore
  , AdapterOptions(..)
  , HelpMap
  , metricInfo
  , samplingFrequency
  , defaultOptions
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import           Data.Foldable (for_)
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Prometheus (Counter, Gauge, Info (..), Label, LabelPairs,
                             Metric, Vector)
import qualified Prometheus as Prometheus
import qualified System.Metrics as EKG

--------------------------------------------------------------------------------
data LabelInfo = forall l. Label l => LabelInfo (l, l)

-- | Map of EKG metric name to help text and any labels
type HelpMap = Map.Map Text (String, Maybe LabelInfo)

data AdapterOptions = AdapterOptions {
    _metricInfo        :: HelpMap
  , _samplingFrequency :: !Int
    -- ^ How often update the registry (in seconds).
  }

makeLenses ''AdapterOptions

--------------------------------------------------------------------------------

type MetricsMap = Map.Map Text (Double -> IO ())

--------------------------------------------------------------------------------
defaultOptions :: AdapterOptions
defaultOptions = AdapterOptions Map.empty 15

--------------------------------------------------------------------------------
registerEKGStore :: MonadIO m => EKG.Store -> AdapterOptions -> m ()
registerEKGStore store opts = do
  mmap <- liftIO $ toPrometheusRegistry store opts
  void $ liftIO $ forkIO $ do
    let loop = forever $ do
                 threadDelay (_samplingFrequency opts * 10^6)
                 updateMetrics store opts mmap
                 loop
    loop

--------------------------------------------------------------------------------
toPrometheusRegistry :: EKG.Store -> AdapterOptions -> IO MetricsMap
toPrometheusRegistry store opts = do
  samples <- EKG.sampleAll store
  foldM (mkMetric opts) Map.empty (HMap.toList samples)

metricUpater :: Metric m -> Maybe LabelInfo -> (Double -> Metric m -> IO ()) -> IO (Double -> IO ())
metricUpater m Nothing f = do
    Prometheus.register m
    return $ flip f m
metricUpater m (Just (LabelInfo (l, v))) f = do
    vec <- Prometheus.vector l (return m)
    Prometheus.register vec
    return $ \val -> do
       Prometheus.withLabel v (f val) vec

--------------------------------------------------------------------------------
mkMetric :: AdapterOptions -> MetricsMap -> (T.Text, EKG.Value) -> IO MetricsMap
mkMetric AdapterOptions{..} mmap (key, value) = do
  let (i, mLabels) = mkInfo key _metricInfo
  case value of
   EKG.Counter c -> do
     counter <- Prometheus.counter i
     mu <- metricUpater counter mLabels (setCount)
     return $! Map.insert key mu mmap
   EKG.Gauge g   -> do
     gauge <- Prometheus.gauge i
     mu <- metricUpater gauge mLabels Prometheus.setGauge
     return $! Map.insert key mu mmap
   EKG.Label _   -> return $! mmap
   EKG.Distribution _ -> return $! mmap
   where
       setCount ekgVal c = do
         oldCounterValue <- Prometheus.getCounter c
         let slack = ekgVal - oldCounterValue
         when (slack >= 0) $ void $ Prometheus.addCounter slack c

--------------------------------------------------------------------------------
updateMetrics :: EKG.Store -> AdapterOptions -> MetricsMap -> IO ()
updateMetrics store opts mmap = do
  samples <- EKG.sampleAll store
  for_ (HMap.toList samples) (updateMetric opts mmap)


--------------------------------------------------------------------------------
mkInfo :: T.Text -> HelpMap -> (Info, Maybe LabelInfo)
mkInfo key hm = case Map.lookup key hm of
    Just (hlp, labels) -> (Info (T.unpack key) hlp, labels)
    Nothing            -> (Info (T.unpack key) "", Nothing)

--------------------------------------------------------------------------------
updateMetric :: AdapterOptions -> MetricsMap -> (T.Text, EKG.Value) -> IO ()
updateMetric AdapterOptions{..} mmap (key, value) = do
  case (,) <$> fromEkg value <*> Map.lookup key mmap  of
      Just (ekgVal, updater) -> do
          updater ekgVal

fromEkg :: EKG.Value -> Maybe Double
fromEkg (EKG.Counter c) = Just $ fromIntegral c
fromEkg (EKG.Gauge g)   = Just $ fromIntegral g
fromEkg _               = Nothing
