module Cardano.Wallet.API.Development.Helpers where

import           Universum

import           Cardano.Wallet.Server.CLI (RunMode (..), isDebugMode)
import           Cardano.Wallet.API.V1.Errors

import           Control.Exception.Safe (throwM)

developmentOnly :: MonadThrow m => RunMode -> m a -> m a
developmentOnly runMode api
    | isDebugMode runMode = api
    | otherwise = throwM $ toError DevelopmentModeOnly
