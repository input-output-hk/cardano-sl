{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeOperators #-}

-- | Wallet web error utilities.

module Pos.Wallet.Web.Error.Util
       ( rewrapToWalletError
       , catchEndpointErrors
       ) where

import           Universum

import           Control.Exception.Safe (Handler (..), catches, tryJust)
import           Formatting (sformat, shown, (%))
import           Servant.Server (ServantErr (..), err500)
import           System.Wlog (CanLog, logError, usingLoggerName)

import           Pos.Wallet.Web.Error.Types (WalletError (..), _RequestError)

rewrapToWalletError
    :: forall e m a.
       (MonadCatch m, Exception e)
    => (e -> Bool) -> (e -> WalletError) -> m a -> m a
rewrapToWalletError whetherCatch toWE = flip catches
     [ Handler $ throwM @_ @WalletError
     , Handler $ \e ->
           if whetherCatch e
           then throwM $ toWE e
           else throwM e
     ]

-- | Catches all errors, and either returns them on client, or
-- just logs error.
catchEndpointErrors
    :: (MonadCatch m, CanLog m)
    => m a -> m (Either WalletError a)
catchEndpointErrors action = catchOtherError $ tryWalletError action
  where
    tryWalletError  = tryJust $ \e -> (e ^? _RequestError) $> e
    catchOtherError = flip catches
        [ Handler $ throwM @_ @ServantErr
        , Handler $ \(SomeException e) -> do
            usingLoggerName logName $
                logError $ sformat ("Uncaught error in wallet method: "%shown) e
            throwM err500 { errBody = "Internal error occured: " <> show e }
        ]
    logName = "wallet-api" <> "handler"
