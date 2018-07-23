{- | This is a temporary module to help migration @V0@ datatypes into @V1@ datatypes.
-}
{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.API.V1.Migration.Monads (
      MonadV1
    , V1Context
    , v1MonadNat
    , lowerV1Monad
    ) where

import           Universum

import qualified Control.Monad.Catch as Catch
import           Servant

import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext)

-- | Temporary monad to handle the migration from the V0 & V1 stacks.
type MonadV1   = WalletWebMode
type V1Context = WalletWebModeContext

-- | Hoist a 'V1' monad to a Servant's Handler.
-- See: http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#natural-transformations
v1MonadNat :: V1Context -> (forall a. MonadV1 a -> Handler a)
v1MonadNat = lowerV1Monad

-- | Converts our domain-specific monad into a standard Servant `Handler`.
lowerV1Monad :: V1Context -> MonadV1 a -> Handler a
lowerV1Monad ctx handler =
    liftIO (hoistHandler handler) `Catch.catches` excHandlers
  where

    hoistHandler :: forall a . MonadV1 a -> IO a
    hoistHandler = flip runReaderT ctx

    excHandlers = [Catch.Handler throwError]
