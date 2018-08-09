module Cardano.Wallet.API.Development.Helpers where

import           Universum

import           Cardano.Wallet.Server.CLI (RunMode (..), isDebugMode)
import           Servant (err403)


developmentOnly :: MonadThrow m => RunMode -> m a -> m a
developmentOnly runMode api
    | isDebugMode runMode = api
    | otherwise = throwM err403
