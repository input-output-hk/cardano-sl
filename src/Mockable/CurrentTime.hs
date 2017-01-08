{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

-- | This module contains mocks for getting current time
-- and useful helper functions to operate with 'TimeUnit's.

module Mockable.CurrentTime
       ( CurrentTime (..)
       , currentTime
       , realTime
       , realTimeUnits
       ) where

import           Universum

import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Units       (Microsecond, TimeUnit, convertUnit)

import           Mockable.Class        (MFunctor' (..), Mockable (..))

data CurrentTime (m :: * -> *) (t :: *) where
    CurrentTime :: CurrentTime m Microsecond

instance MFunctor' CurrentTime m n where
    hoist' _ CurrentTime = CurrentTime

currentTime :: (Mockable CurrentTime m) => m Microsecond
currentTime = liftMockable CurrentTime

realTime :: MonadIO m => m Microsecond
realTime = liftIO $ round . (* 1000000) <$> getPOSIXTime

realTimeUnits :: (TimeUnit t, MonadIO m) => m t
realTimeUnits = convertUnit <$> realTime
