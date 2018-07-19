{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Pos.Core.Mockable.Instances where

import           Prelude

import           Control.Monad.Trans.Reader (ReaderT (..))
import           Pos.Core.Mockable.Channel (ChannelT)
import           Pos.Core.Mockable.Class (MFunctor' (..), Mockable (..))
import           Pos.Core.Mockable.Concurrent (Promise, ThreadId)
import           Pos.Core.Mockable.Metrics
import           Pos.Core.Mockable.SharedAtomic (SharedAtomicT)
import           Pos.Core.Mockable.SharedExclusive (SharedExclusiveT)
import           System.Wlog (LoggerNameBox)

instance (Mockable d m, MFunctor' d (ReaderT r m) m) => Mockable d (ReaderT r m) where
    liftMockable dmt = ReaderT $ \r -> liftMockable $ hoist' (flip runReaderT r) dmt

type instance ThreadId (LoggerNameBox m) = ThreadId m
type instance Promise (LoggerNameBox m) = Promise m
type instance SharedAtomicT (LoggerNameBox m) = SharedAtomicT m
type instance SharedExclusiveT (LoggerNameBox m) = SharedExclusiveT m
type instance ChannelT (LoggerNameBox m) = ChannelT m
type instance Gauge (LoggerNameBox m) = Gauge m
type instance Counter (LoggerNameBox m) = Counter m
type instance Distribution (LoggerNameBox m) = Distribution m

type instance ThreadId (ReaderT r m) = ThreadId m
type instance Promise (ReaderT r m) = Promise m
type instance SharedAtomicT (ReaderT r m) = SharedAtomicT m
type instance SharedExclusiveT (ReaderT r m) = SharedExclusiveT m
type instance ChannelT (ReaderT r m) = ChannelT m
type instance Gauge (ReaderT r m) = Gauge m
type instance Counter (ReaderT r m) = Counter m
type instance Distribution (ReaderT r m) = Distribution m
