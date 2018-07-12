{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Cardano.Faucet.Types (
    M, runM, liftToM
  , MonadFaucet
  , module Cardano.Faucet.Types.Config
  , module Cardano.Faucet.Types.API
  ) where

import           Control.Monad.Except
import           Control.Monad.Morph (hoist)
import           Control.Monad.Reader
import           Servant (Handler (..), ServantErr)
import           System.Wlog (CanLog, HasLoggerName, LoggerNameBox (..),
                     WithLogger)
import           Universum

import           Cardano.Faucet.Types.API
import           Cardano.Faucet.Types.Config

--------------------------------------------------------------------------------
-- | Faucet monad
--
-- | Concrete monad stack for server server
newtype M a = M { unM :: ReaderT FaucetEnv (ExceptT ServantErr (LoggerNameBox IO)) a }
  deriving ( Functor, Applicative, Monad, MonadReader FaucetEnv, CanLog
           , HasLoggerName, MonadIO, MonadError ServantErr)

-- | Runs the 'M' monad
runM :: FaucetEnv -> M a -> LoggerNameBox IO (Either ServantErr a)
runM c =  runExceptT
       . flip runReaderT c
       . unM

liftToM :: Handler a -> M a
liftToM = M . lift . hoist lift . runHandler'

type MonadFaucet c m = ( MonadIO m, MonadReader c m, HasFaucetEnv c, WithLogger m
                       , HasLoggerName m, MonadError ServantErr m)
