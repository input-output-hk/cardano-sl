{- | This is a temporary module to help migration @V0@ datatypes into @V1@ datatypes.
-}
{-# LANGUAGE TypeSynonymInstances #-}
module Cardano.Wallet.API.V1.Migration.Types (
      Migrate(..)
    , migrate
    ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import           Data.Map (elems)
import           Data.Time.Clock.POSIX (POSIXTime)

import           Cardano.Wallet.API.V1.Errors as Errors
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Pos.Client.Txp.Util as V0
import qualified Pos.Core.Common as Core
import qualified Pos.Core.Txp as Core
import qualified Pos.Util.Servant as V0
import qualified Pos.Wallet.Web.ClientTypes.Instances ()
import qualified Pos.Wallet.Web.ClientTypes.Types as V0

import qualified Control.Monad.Catch as Catch

-- | 'Migrate' encapsulates migration between types, when possible.
class Migrate from to where
    eitherMigrate :: from -> Either Errors.WalletError to

-- | "Run" the migration.
migrate :: ( Migrate from to, Catch.MonadThrow m ) => from -> m to
migrate from = case eitherMigrate from of
    Left e   -> Catch.throwM e
    Right to -> pure to

--
-- Instances
--

instance Migrate from to => Migrate [from] [to] where
    eitherMigrate = mapM eitherMigrate

-- | Migration from list to NonEmpty, as suggested by @akegalj.
instance Migrate from to => Migrate [from] (NonEmpty to) where
  eitherMigrate a = NE.fromList <$> mapM eitherMigrate a

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

instance Migrate V1.AccountUpdate V0.CAccountMeta where
    eitherMigrate V1.AccountUpdate{..} =
        pure $ V0.CAccountMeta uaccName

instance Migrate V1.NewAccount V0.CAccountMeta where
    eitherMigrate V1.NewAccount{..} =
        pure $ V0.CAccountMeta naccName

instance Migrate (V1.WalletId, V1.NewAccount) V0.CAccountInit where
    eitherMigrate (wId, nAcc) = do
        newWId <- eitherMigrate wId
        accMeta <- eitherMigrate nAcc
        pure $ V0.CAccountInit accMeta newWId

instance Migrate V0.CAddress V1.WalletAddress where
    eitherMigrate V0.CAddress{..} = do
        addrId <- eitherMigrate cadId
        addrBalance <- eitherMigrate cadAmount
        let addrUsed = cadIsUsed
        let addrChangeAddress = cadIsChange
        return V1.WalletAddress{..}

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

-- in old API 'V0.AccountId' supposed to carry both wallet id and derivation index
instance Migrate (V1.WalletId, V1.AccountId) V0.AccountId where
    eitherMigrate (walId, accId) =
        V0.AccountId <$> eitherMigrate walId <*> pure accId

instance Migrate (V1.WalletId, V1.AccountId) V0.CAccountId where
    eitherMigrate (walId, accId) =
        V0.encodeCType <$> eitherMigrate (walId, accId)

instance Migrate V0.AccountId (V1.WalletId, V1.AccountId) where
    eitherMigrate accId =
        (,) <$> eitherMigrate (V0.aiWId accId) <*> pure (V0.aiIndex accId)

instance Migrate V0.CAccountId V0.AccountId where
    eitherMigrate = first Errors.MigrationFailed . V0.decodeCType

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

instance Migrate (V0.CId V0.Addr) Core.Address where
    eitherMigrate (V0.CId (V0.CHash h)) =
        first (const $ Errors.MigrationFailed "Error migrating (V0.CId V0.Addr) -> Core.Address failed.")
            . Core.decodeTextAddress $ h

instance Migrate (V0.CId V0.Addr, V0.CCoin) V1.PaymentDistribution where
    eitherMigrate (cIdAddr, cCoin) = do
        pdAddress <- eitherMigrate cIdAddr
        pdAmount  <- eitherMigrate cCoin
        pure $ V1.PaymentDistribution {..}

instance Migrate V1.PaymentDistribution (V0.CId V0.Addr, Core.Coin) where
    eitherMigrate V1.PaymentDistribution {..} =
        pure (V0.encodeCType pdAddress, pdAmount)

instance Migrate V0.CTxId V1.TxId where
    eitherMigrate (V0.CTxId (V0.CHash h)) = pure $ V1.TxId h

instance Migrate V0.CTx V1.Transaction where
    eitherMigrate V0.CTx{..} = do
        txId      <- eitherMigrate ctId
        let txConfirmations = ctConfirmations
        txAmount  <- eitherMigrate ctAmount
        txInputs  <- eitherMigrate ctInputs
        txOutputs <- eitherMigrate ctOutputs

        let txType = if ctIsLocal
            then V1.LocalTransaction
            else V1.ForeignTransaction

        let txDirection = if ctIsOutgoing
            then V1.OutgoingTransaction
            else V1.IncomingTransaction

        pure V1.Transaction{..}

-- | The migration instance for migrating history to a list of transactions
instance Migrate (Map Core.TxId (V0.CTx, POSIXTime), Word) [V1.Transaction] where
    eitherMigrate txsMapAndSize = do
        let txsMapValues = elems . fst $ txsMapAndSize
        mapM (eitherMigrate . fst) txsMapValues

instance Migrate V1.TransactionGroupingPolicy V0.InputSelectionPolicy where
    eitherMigrate policy =
        pure $ case policy of
                  V1.OptimiseForHighThroughputPolicy -> V0.OptimizeForHighThroughput
                  V1.OptimiseForSecurityPolicy       -> V0.OptimizeForSecurity
