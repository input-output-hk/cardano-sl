{- | This is a temporary module to help migration @V0@ datatypes into @V1@ datatypes.
-}
{-# LANGUAGE TypeSynonymInstances #-}
module Cardano.Wallet.API.V1.Migration.Types (
      Migrate(..)
    , migrate
    ) where

import           Universum

import           Cardano.Wallet.API.V1.Errors as Errors
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Pos.Core.Common as Core
import qualified Pos.Util.Servant as V0
import qualified Pos.Wallet.Web.ClientTypes.Instances ()
import qualified Pos.Wallet.Web.ClientTypes.Types as V0

import qualified Control.Monad.Catch as Catch

-- | 'Migrate' encapsulates migration between types, when possible.
class Migrate from to where
    eitherMigrate :: from -> Either Errors.WalletError to

-- | "Run" the migration.
migrate :: ( Catch.MonadThrow m, Migrate from to ) => from -> m to
migrate from = case eitherMigrate from of
    Left e   -> Catch.throwM e
    Right to -> pure to

--
-- Instances
--


instance Migrate V0.CWallet V1.Wallet where
    eitherMigrate V0.CWallet{..} =
        V1.Wallet <$> eitherMigrate cwId
                  <*> pure (V0.cwName cwMeta)
                  <*> eitherMigrate cwAmount

-- NOTE: Migrate V1.Wallet V0.CWallet unable to do - not idempotent

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

instance Migrate Core.Coin V0.CCoin where
    eitherMigrate = pure . V0.encodeCType

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

-- NOTE: Migrate V1.SyncProgress V0.SyncProgress unable to do - not idempotent

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
          first (const $ Errors.MigrationFailed "Error migrating V0.CAddress -> Core.Address failed.")
              $ V0.decodeCType cadId
