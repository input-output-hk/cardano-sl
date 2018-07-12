{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Cardano.Faucet.Metrics (
   incWithDrawn
 , setWalletBalance
 ) where

import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge
import           Universum

import           Cardano.Faucet.Types
import           Pos.Core (Coin (..))

--------------------------------------------------------------------------------
-- | Record a withdrawal
--
-- * Adds to 'feWithDrawn' 'Counter'
-- * Increments 'feNumWithDrawn' 'Counter'
-- * Adds to 'feWalletBalance'
incWithDrawn :: (MonadReader e m, HasFaucetEnv e, MonadIO m) => Coin -> m ()
incWithDrawn (Coin (fromIntegral -> c)) = do
  wd <- view feWithdrawn
  wc <- view feNumWithdrawn
  bal <- view feWalletBalance
  liftIO $ do
    Counter.add wd c
    Counter.inc wc
    Gauge.subtract bal c

--------------------------------------------------------------------------------
-- | Resets the wallet balance in 'feWalletBalance'
setWalletBalance :: (MonadReader e m, HasFaucetEnv e, MonadIO m) => Coin -> m ()
setWalletBalance (Coin (fromIntegral -> c)) = do
  bal <- view feWalletBalance
  liftIO $ Gauge.set bal c
