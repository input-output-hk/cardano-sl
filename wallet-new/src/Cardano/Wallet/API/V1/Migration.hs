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
import qualified Pos.Util.Servant as V0
import qualified Pos.Wallet.Web.ClientTypes.Types as V0

import qualified Control.Exception.Safe as E
import           Mockable (runProduction)
import           Servant

import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Ssc (HasSscConfiguration)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail)

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
    liftIO (hoistHandler handler) `E.catches` excHandlers
  where

    hoistHandler :: forall a . MonadV1 a -> IO a
    hoistHandler = runProduction . flip runReaderT ctx

    excHandlers = [E.Handler throwError]

-- | 'Migrate' encapsulates migration between types, when possible.
class Migrate from to where
    eitherMigrate :: from -> Either Errors.WalletError to

-- | "Run" the migration.
migrate :: ( Migrate from to, E.MonadThrow m ) => from -> m to
migrate from = case eitherMigrate from of
    Left e   -> E.throwM e
    Right to -> pure to

--
-- Instances
--

instance Migrate from to => Migrate [from] [to] where
    eitherMigrate = mapM eitherMigrate

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
        first (const $ Errors.MigrationFailed "error migrating V0.CCoin -> Core.Coin, mkCoin failed.") .
        V0.decodeCType

--
instance Migrate (V0.CId V0.Wal) V1.WalletId where
    eitherMigrate (V0.CId (V0.CHash h)) = pure (V1.WalletId h)

instance Migrate V1.WalletId (V0.CId V0.Wal) where
    eitherMigrate (V1.WalletId h) = pure (V0.CId (V0.CHash h))

-- | Migrates to a V1 `SyncProgress` by computing the percentage as
-- coded here: https://github.com/input-output-hk/daedalus/blob/master/app/stores/NetworkStatusStore.js#L108
instance Migrate V0.SyncProgress V1.SyncProgress where
    eitherMigrate V0.SyncProgress{..} =
        let percentage = case _spNetworkCD of
                Nothing -> (0 :: Word8)
                Just nd | _spLocalCD >= nd -> 100
                Just nd -> round @Double $ (fromIntegral _spLocalCD / fromIntegral nd) * 100.0
        in pure $ V1.mkSyncProgress (fromIntegral percentage)


--
instance Migrate V0.CAccount V1.Account where
    eitherMigrate V0.CAccount{..} =
        V1.Account <$> eitherMigrate caId
                  -- ^ accId
                  <*> mapM eitherMigrate caAddresses
                  -- ^ accAddresses
                  <*> eitherMigrate caAmount
                  -- ^ accAmount
                  <*> pure (V0.caName caMeta)
                  -- ^ accName
                  <*> eitherMigrate caId
                  -- ^ accWalletId

--
-- Following instances are friendly borrowed from @martoon's PR https://github.com/input-output-hk/cardano-sl/pull/2008
-- TODO (jk): Use instances of his PR when it has been merged

-- in old API 'V0.AccountId' supposed to carry both wallet id and derivation index
instance Migrate (V1.WalletId, V1.AccountId) V0.AccountId where
    eitherMigrate (walId, accId) =
        V0.AccountId <$> eitherMigrate walId <*> pure accId

instance Migrate V0.AccountId (V1.WalletId, V1.AccountId) where
    eitherMigrate accId =
        (,) <$> eitherMigrate (V0.aiWId accId) <*> pure (V0.aiIndex accId)

instance Migrate V0.CAccountId V0.AccountId where
    eitherMigrate = first Errors.MigrationFailed . V0.decodeCType

--
-- #end of TODO (jk) ^
--

instance Migrate V0.CAccountId V1.AccountId where
    eitherMigrate cAccId = do
        oldAccountId :: V0.AccountId <- eitherMigrate cAccId
        (_, newAccountId) :: (V1.WalletId, V1.AccountId) <- eitherMigrate oldAccountId
        pure newAccountId

instance Migrate V0.CAccountId V1.WalletId where
    eitherMigrate cAccId = do
        oldAccountId :: V0.AccountId <- eitherMigrate cAccId
        (walletId, _) :: (V1.WalletId, V1.AccountId) <- eitherMigrate oldAccountId
        pure walletId

instance Migrate V0.CAddress Core.Address where
       eitherMigrate V0.CAddress {..} =
          either (\_ -> Left $ Errors.MigrationFailed "Error migrating V0.CAddress -> Core.Address failed.")
              Right $ decodeCTypeOrFail cadId

instance Migrate V1.AccountUpdate V0.CAccountMeta where
    eitherMigrate V1.AccountUpdate{..} =
        pure $ V0.CAccountMeta uaccName
