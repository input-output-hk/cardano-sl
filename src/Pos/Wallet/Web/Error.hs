{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

-- | Types describing runtime errors related to Wallet.

module Pos.Wallet.Web.Error
       ( WalletError (..)
       , _InternalError
       , _RequestError
       , rewrapToWalletError
       , catchEndpointErrors
       ) where

import           Control.Lens        (makePrisms)
import           Control.Monad.Catch (Handler (..), catches, handleAll, try, tryJust)
import qualified Data.Text.Buildable
import           Formatting          (bprint, sformat, shown, stext, (%))
import           Servant.Server      (err500)
import           System.Wlog         (CanLog, logError, usingLoggerName)
import           Universum

import           Pos.Constants       (isDevelopment)

data WalletError
    -- | Reasonable error for given request
    -- (e.g. get info about non-existent wallet).
    -- However, this separation is still a bit conditional, may require remake
    = RequestError !Text
    -- | Internal info, which ideally should never happen
    | InternalError !Text
    deriving (Show, Generic)

makePrisms ''WalletError

instance Exception WalletError

instance Buildable WalletError where
    build (RequestError  msg) = bprint ("Request error ("%stext%")") msg
    build (InternalError msg) = bprint ("Internal error ("%stext%")") msg

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

