{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mockable.Metrics (

      Gauge
    , Counter
    , Distribution
    , Metrics(..)
    , Stats(..)

    , newGauge
    , incGauge
    , decGauge
    , setGauge
    , readGauge

    , newCounter
    , incCounter
    , readCounter

    , newDistribution
    , addSample
    , readDistribution

    ) where

import Data.Int (Int64)
import Mockable.Class

type family Gauge (m :: * -> *) :: *
type family Counter (m :: * -> *) :: *
type family Distribution (m :: * -> *) :: *

-- | Various statistics.
data Stats = Stats {
      mean :: Double
    , variance :: Double
    , count :: Int64
    , sum :: Double
    , min :: Double
    , max :: Double
    }

-- | Counters, gauages, and distributions.
data Metrics (m :: * -> *) (t :: *) where

    NewGauge :: Metrics m (Gauge m)
    IncGauge :: Gauge m -> Metrics m ()
    DecGauge :: Gauge m -> Metrics m ()
    SetGauge :: Gauge m -> Int64 -> Metrics m ()
    ReadGauge :: Gauge m -> Metrics m Int64

    NewCounter :: Metrics m (Counter m)
    IncCounter :: Counter m -> Metrics m ()
    ReadCounter :: Counter m -> Metrics m Int64

    NewDistribution :: Metrics m (Distribution m)
    AddSample :: Distribution m -> Double -> Metrics m ()
    ReadDistribution :: Distribution m -> Metrics m Stats

instance
    ( Gauge m ~ Gauge n
    , Counter m ~ Counter n
    , Distribution m ~ Distribution n
    ) => MFunctor' Metrics m n
    where
    hoist' _ term = case term of

        NewGauge -> NewGauge
        IncGauge gauge -> IncGauge gauge
        DecGauge gauge -> DecGauge gauge
        SetGauge gauge n -> SetGauge gauge n
        ReadGauge gauge -> ReadGauge gauge

        NewCounter -> NewCounter
        IncCounter counter -> IncCounter counter
        ReadCounter counter -> ReadCounter counter

        NewDistribution -> NewDistribution
        AddSample distr s -> AddSample distr s
        ReadDistribution distr -> ReadDistribution distr

newGauge :: ( Mockable Metrics m ) => m (Gauge m)
newGauge = liftMockable NewGauge

incGauge :: ( Mockable Metrics m ) => Gauge m -> m ()
incGauge = liftMockable . IncGauge

decGauge :: ( Mockable Metrics m ) => Gauge m -> m ()
decGauge = liftMockable . DecGauge

setGauge :: ( Mockable Metrics m ) => Gauge m -> Int64 -> m ()
setGauge gauge = liftMockable . SetGauge gauge

readGauge :: ( Mockable Metrics m ) => Gauge m -> m Int64
readGauge = liftMockable . ReadGauge

newCounter :: ( Mockable Metrics m ) => m (Counter m)
newCounter = liftMockable NewCounter

incCounter :: ( Mockable Metrics m ) => Counter m -> m ()
incCounter = liftMockable . IncCounter

readCounter :: ( Mockable Metrics m ) => Counter m -> m Int64
readCounter = liftMockable . ReadCounter

newDistribution :: ( Mockable Metrics m ) => m (Distribution m)
newDistribution = liftMockable NewDistribution

addSample :: ( Mockable Metrics m ) => Distribution m -> Double -> m ()
addSample distr = liftMockable . AddSample distr

readDistribution :: ( Mockable Metrics m ) => Distribution m -> m Stats
readDistribution = liftMockable . ReadDistribution
