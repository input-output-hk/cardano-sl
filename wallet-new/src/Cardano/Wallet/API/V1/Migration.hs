{- | This is a temporary module to help migration @V0@ datatypes into @V1@ datatypes.
-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Cardano.Wallet.API.V1.Migration (
      MonadV1
    , V1Context
    , v1MonadNat
    , lowerV1Monad
    , Migrate(..)
    , migrate
    -- * Configuration re-exports
    , HasCompileInfo
    , HasConfigurations
    , HasConfiguration
    , HasSscConfiguration
    , HasUpdateConfiguration
    , HasNodeConfiguration
    ) where

import           Universum

import           Cardano.Wallet.API.V1.Errors as Errors
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Pos.Core.Common as Core
import qualified Pos.Wallet.Web.ClientTypes.Types as V0

import qualified Control.Monad.Catch as Catch
import           Mockable (runProduction)
import           Servant

import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Ssc (HasSscConfiguration)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Util.CompileInfo (HasCompileInfo)
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
    hoistHandler = runProduction . flip runReaderT ctx

    excHandlers = [Catch.Handler throwError]

-- | 'Migrate' encapsulates migration between types, when possible.
class Migrate from to where
    eitherMigrate :: from -> Either V1.WalletError to

-- | "Run" the migration.
migrate :: ( Catch.MonadThrow m, Migrate from to ) => from -> m to
migrate from = case eitherMigrate from of
    Left e   -> Catch.throwM (migrationError e)
    Right to -> pure to

-- | A 'ServantError' representing a migration error,
-- represented here with a <https://httpstatuses.com/422 422> HTTP status code.
migrationError :: V1.WalletError -> ServantErr
migrationError = Errors.toError err422

-- | A 'WalletError' representing a failed migration.
migrationFailed :: Text -> V1.WalletError
migrationFailed e = V1.WalletError {
      V1.errCode = 422
    , V1.errMessage = e
    }

--
-- Instances
--

instance Migrate V0.CWallet V1.Wallet where
    eitherMigrate V0.CWallet{..} =
        V1.Wallet <$> eitherMigrate cwId
                  <*> pure (V0.cwName cwMeta)
                  <*> eitherMigrate cwAmount

--
instance Migrate V0.CWalletAssurance V1.AssuranceLevel where
    eitherMigrate V0.CWAStrict = pure V1.StrictAssurance
    eitherMigrate V0.CWANormal = pure V1.NormalAssurance

instance Migrate V1.AssuranceLevel V0.CWalletAssurance where
    eitherMigrate V1.StrictAssurance = pure V0.CWAStrict
    eitherMigrate V1.NormalAssurance = pure V0.CWANormal

--
instance Migrate V0.CCoin Core.Coin where
    eitherMigrate =
        maybe (Left $ migrationFailed "error migrating V0.CCoin -> Core.Coin, mkCoin failed.") Right
        . fmap Core.mkCoin
        . readMaybe
        . toString
        . V0.getCCoin

--
instance Migrate (V0.CId V0.Wal) V1.WalletId where
    eitherMigrate (V0.CId (V0.CHash h)) = pure (V1.WalletId h)
