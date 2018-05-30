{- | This is a temporary module to help migration @V0@ datatypes into @V1@ datatypes.
-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Wallet.API.V1.Migration.Types (
      Migrate(..)
    , migrate
    ) where

import           Universum

import qualified Control.Lens as Lens
import qualified Control.Monad.Catch as Catch
import           Data.Map (elems)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Time.Units (fromMicroseconds, toMicroseconds)
import           Data.Typeable (typeRep)
import           Formatting (sformat)

import           Cardano.Wallet.API.V1.Errors as Errors
import           Cardano.Wallet.API.V1.Types (V1 (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Pos.Client.Txp.Util as V0
import           Pos.Core (addressF)
import qualified Pos.Core.Common as Core
import qualified Pos.Core.Slotting as Core
import qualified Pos.Core.Txp as Core
import           Pos.Crypto (decodeHash)
import qualified Pos.Txp.Toil.Types as V0
import qualified Pos.Util.Servant as V0
import qualified Pos.Wallet.Web.ClientTypes.Instances ()
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.State.Storage as OldStorage
import           Pos.Wallet.Web.Tracking.Sync (calculateEstimatedRemainingTime)

-- | 'Migrate' encapsulates migration between types, when possible.
-- NOTE: This has @nothing@ to do with database migrations (see `safecopy`),
-- and the name clash is a historic accident. Hopefully once the new data layer
-- will be completed and the V0 API removed, we will be able to remove this
-- typeclass altogether.
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
instance (Migrate from to, Typeable from, Typeable to) => Migrate [from] (NonEmpty to) where
    eitherMigrate [] = Left . MigrationFailed $ mconcat
        [ "Failed migrating "
        , show (typeRep (Proxy @[from]))
        , " to a non empty list of "
        , show (typeRep (Proxy @to))
        , " because the list was empty."
        ]
    eitherMigrate (x:xs) = (:|) <$> eitherMigrate x <*> mapM eitherMigrate xs

instance Migrate (V0.CWallet, OldStorage.WalletInfo, Maybe Core.ChainDifficulty) V1.Wallet where
    eitherMigrate (V0.CWallet{..}, OldStorage.WalletInfo{..}, currentBlockchainHeight) =
        V1.Wallet <$> eitherMigrate cwId
                  <*> pure (V0.cwName cwMeta)
                  <*> eitherMigrate cwAmount
                  <*> pure cwHasPassphrase
                  <*> eitherMigrate cwPassphraseLU
                  <*> eitherMigrate _wiCreationTime
                  <*> eitherMigrate (V0.cwAssurance _wiMeta)
                  <*> eitherMigrate (_wiSyncState, _wiSyncStatistics, currentBlockchainHeight)

instance Migrate (OldStorage.WalletSyncState, OldStorage.SyncStatistics, Maybe Core.ChainDifficulty) V1.SyncState where
    eitherMigrate (wss, stats, currentBlockchainHeight) =
        case wss of
            OldStorage.NotSynced         -> V1.Restoring <$> eitherMigrate (stats, currentBlockchainHeight)
            OldStorage.RestoringFrom _ _ -> V1.Restoring <$> eitherMigrate (stats, currentBlockchainHeight)
            OldStorage.SyncedWith _      -> pure V1.Synced

instance Migrate (OldStorage.SyncStatistics, Maybe Core.ChainDifficulty) V1.SyncProgress where
    eitherMigrate (OldStorage.SyncStatistics{..}, currentBlockchainHeight) =
        let unknownCompletionTime = Core.Timestamp $ fromMicroseconds (fromIntegral (maxBound :: Int))
            percentage = case currentBlockchainHeight of
                Nothing -> 0.0
                Just nd | wspCurrentBlockchainDepth >= nd -> 100
                Just nd -> (fromIntegral wspCurrentBlockchainDepth / max 1.0 (fromIntegral nd)) * 100.0
            toMs (Core.Timestamp microsecs) =
              V1.mkEstimatedCompletionTime (round @Double $ (realToFrac (toMicroseconds microsecs) / 1000.0))
            tput (OldStorage.SyncThroughput blocks) = V1.mkSyncThroughput blocks
            remainingBlocks = fmap (\total -> total - wspCurrentBlockchainDepth) currentBlockchainHeight
        in V1.SyncProgress <$> pure (toMs (maybe unknownCompletionTime
                                                 (calculateEstimatedRemainingTime wspThroughput)
                                                 remainingBlocks))
                           <*> pure (tput wspThroughput)
                           <*> pure (V1.mkSyncPercentage (floor @Double $ percentage))

-- NOTE: Migrate V1.Wallet V0.CWallet unable to do - not idempotent

--
instance Migrate V0.CWalletAssurance V1.AssuranceLevel where
    eitherMigrate V0.CWAStrict = pure V1.StrictAssurance
    eitherMigrate V0.CWANormal = pure V1.NormalAssurance

instance Migrate V1.AssuranceLevel V0.CWalletAssurance where
    eitherMigrate V1.StrictAssurance = pure V0.CWAStrict
    eitherMigrate V1.NormalAssurance = pure V0.CWANormal

--
instance Migrate V0.CCoin (V1 Core.Coin) where
    eitherMigrate c =
        let err = Left . Errors.MigrationFailed . mappend "error migrating V0.CCoin -> Core.Coin, mkCoin failed: "
        in either err (pure . V1) (V0.decodeCType c)

instance Migrate (V1 Core.Coin) V0.CCoin where
    eitherMigrate (V1 c) = pure (V0.encodeCType c)

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
        let addrUsed = cadIsUsed
        let addrChangeAddress = cadIsChange
        return V1.WalletAddress{..}

-- | Migrates to a V1 `SyncProgress` by computing the percentage as
-- coded here: https://github.com/input-output-hk/daedalus/blob/master/app/stores/NetworkStatusStore.js#L108
instance Migrate V0.SyncProgress V1.SyncPercentage where
    eitherMigrate V0.SyncProgress{..} =
        let percentage = case _spNetworkCD of
                Nothing -> (0 :: Word8)
                Just nd | _spLocalCD >= nd -> 100
                Just nd -> floor @Double $ (fromIntegral _spLocalCD / max 1.0 (fromIntegral nd)) * 100.0
        in pure $ V1.mkSyncPercentage (fromIntegral percentage)

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
instance Migrate (V1.WalletId, V1.AccountIndex) V0.AccountId where
    eitherMigrate (walId, accIdx) =
        V0.AccountId <$> eitherMigrate walId <*> pure accIdx

instance Migrate V1.PaymentSource V0.AccountId where
    eitherMigrate V1.PaymentSource{..} = eitherMigrate (psWalletId, psAccountIndex)

instance Migrate (V1.WalletId, V1.AccountIndex) V0.CAccountId where
    eitherMigrate (walId, accId) =
        V0.encodeCType <$> eitherMigrate (walId, accId)

instance Migrate V1.PaymentSource V0.CAccountId where
    eitherMigrate V1.PaymentSource{..} = eitherMigrate (psWalletId, psAccountIndex)

instance Migrate V0.AccountId (V1.WalletId, V1.AccountIndex) where
    eitherMigrate accId =
        (,) <$> eitherMigrate (V0.aiWId accId) <*> pure (V0.aiIndex accId)

instance Migrate V0.CAccountId V0.AccountId where
    eitherMigrate = first Errors.MigrationFailed . V0.decodeCType

instance Migrate V0.CAccountId V1.AccountIndex where
    eitherMigrate cAccId = do
        oldAccountId :: V0.AccountId <- eitherMigrate cAccId
        (_, newAccountIndex) :: (V1.WalletId, V1.AccountIndex) <- eitherMigrate oldAccountId
        pure newAccountIndex

instance Migrate V0.CAccountId V1.WalletId where
    eitherMigrate cAccId = do
        oldAccountId :: V0.AccountId <- eitherMigrate cAccId
        (walletId, _) :: (V1.WalletId, V1.AccountIndex) <- eitherMigrate oldAccountId
        pure walletId

instance Migrate V0.CAddress (V1 Core.Address) where
       eitherMigrate V0.CAddress {..} =
           let err = Left . Errors.MigrationFailed . mappend "Error migrating V0.CAddress -> Core.Address failed: "
           in either err (pure . V1) (V0.decodeCType cadId)

instance Migrate (V0.CId V0.Addr) (V1 Core.Address) where
    eitherMigrate (V0.CId (V0.CHash h)) =
        let err = Left . Errors.MigrationFailed . mappend "Error migrating (V0.CId V0.Addr) -> Core.Address failed."
        in either err (pure . V1) (Core.decodeTextAddress h)

instance Migrate (V1 Core.Address) (V0.CId V0.Addr) where
    eitherMigrate (V1 address) =
      let h = sformat addressF address in
      pure $ (V0.CId (V0.CHash h))

instance Migrate (V0.CId V0.Addr, V0.CCoin) V1.PaymentDistribution where
    eitherMigrate (cIdAddr, cCoin) = do
        pdAddress <- eitherMigrate cIdAddr
        pdAmount  <- eitherMigrate cCoin
        pure $ V1.PaymentDistribution {..}

instance Migrate V1.PaymentDistribution (V0.CId V0.Addr, Core.Coin) where
    eitherMigrate V1.PaymentDistribution {..} =
        let (V1 amount) = pdAmount
            (V1 addr)   = pdAddress
            in pure (V0.encodeCType addr, amount)

instance Migrate (V0.CId V0.Addr, Core.Coin) V1.PaymentDistribution where
    eitherMigrate (cIdAddr, coin) = do
        pdAddress <- eitherMigrate cIdAddr
        pure $ V1.PaymentDistribution pdAddress (V1 coin)

instance Migrate V0.CTxId (V1 Core.TxId) where
    eitherMigrate (V0.CTxId (V0.CHash h)) =
        let err = Left . Errors.MigrationFailed . mappend "Error migrating a TxId: "
        in either err (pure . V1) (decodeHash h)

instance Migrate POSIXTime (V1 Core.Timestamp) where
    eitherMigrate time = pure . V1 $ view (Lens.from Core.timestampSeconds) time

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

        let V0.CTxMeta{..} = ctMeta
        txCreationTime <- eitherMigrate ctmDate
        txStatus <- eitherMigrate ctCondition

        pure V1.Transaction{..}

instance Migrate V0.CPtxCondition V1.TransactionStatus where
    eitherMigrate = Right . \case
        V0.CPtxCreating{} ->
            V1.Creating
        V0.CPtxApplying{} ->
            V1.Applying
        V0.CPtxInBlocks{} ->
            V1.InNewestBlocks
        V0.CPtxWontApply{} ->
            V1.WontApply
        V0.CPtxNotTracked ->
            V1.Persisted

-- | The migration instance for migrating history to a list of transactions
instance Migrate (Map Core.TxId (V0.CTx, POSIXTime)) [V1.Transaction] where
    eitherMigrate txsMap = mapM (eitherMigrate . fst) (elems txsMap)

instance Migrate (V1 V0.InputSelectionPolicy) V0.InputSelectionPolicy where
    eitherMigrate (V1 policy) = pure policy

instance Migrate V0.TxFee V1.EstimatedFees where
    eitherMigrate (V0.TxFee coin) = pure $ V1.EstimatedFees (V1 coin)

instance Migrate V1.EstimatedFees V0.TxFee where
    eitherMigrate V1.EstimatedFees{..} =
        let (V1 amount) = feeEstimatedAmount
            in pure $ V0.TxFee amount


-- | This type is really the same, since the unit can be ignored.
instance Migrate V1.WalletUpdate V0.CWalletMeta where
    eitherMigrate V1.WalletUpdate{..} = do
        migratedAssurance <- eitherMigrate uwalAssuranceLevel
        pure $ V0.CWalletMeta
            { cwName      = uwalName
            , cwAssurance = migratedAssurance
            , cwUnit      = 0
            }

instance Migrate V0.CWalletMeta V1.WalletUpdate where
    eitherMigrate V0.CWalletMeta{..} = do
        migratedAssurance <- eitherMigrate cwAssurance
        pure $ V1.WalletUpdate
            { uwalName              = cwName
            , uwalAssuranceLevel    = migratedAssurance
            }
