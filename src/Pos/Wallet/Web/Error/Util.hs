{-# LANGUAGE TypeOperators #-}

-- | Wallet web error utilities.

module Pos.Wallet.Web.Error.Util
       ( rewrapToWalletError
       , catchEndpointErrors
       ) where

import           Universum

import           Control.Monad.Catch        (Handler (..), catches, handleAll, try,
                                             tryJust)
import           Formatting                 (sformat, shown, (%))
import           Servant.Server             (err500)
import           System.Wlog                (CanLog, logError, usingLoggerName)

import           Pos.Constants              (isDevelopment)
import           Pos.Wallet.Web.Error.Types (WalletError (..), _RequestError)

rewrapToWalletError :: MonadCatch m => m a -> m a
rewrapToWalletError = flip catches
     [ Handler $ \e@(RequestError _) -> throwM e
     , Handler $ \(SomeException e)  -> throwM . RequestError $ show e
     ]

-- | Catches all errors, and either returns them on client, or
-- just logs error.
-- It has such strange type, because it's used to modify servant handler
-- in general case (see 'Pos.Wallet.Web.Api').
catchEndpointErrors
    :: (MonadCatch m, CanLog m)
    => m a -> m (Either WalletError a)
catchEndpointErrors action = catchOtherError $ tryWalletError action
  where
    tryWalletError
        | isDevelopment = try
        | otherwise     = tryJust $ \e -> (e ^? _RequestError) $> e
    catchOtherError = handleAll $ \e -> do
        usingLoggerName logName $
            logError $ sformat ("Uncaught error in wallet method: "%shown) e
        throwM err500
    logName = "wallet-api" <> "handler"
