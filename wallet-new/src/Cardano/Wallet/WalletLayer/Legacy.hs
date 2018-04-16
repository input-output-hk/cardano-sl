{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.WalletLayer.Legacy
    ( MonadLegacyWallet
    , bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.Coerce (coerce)

import           Cardano.Wallet.WalletLayer.Error (WalletLayerError (..))
import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..), PassiveWalletLayer (..),
                                                   monadThrowToEither)

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))

import           Cardano.Wallet.API.V1.Migration (migrate)
import           Cardano.Wallet.API.V1.Migration.Types ()
import           Cardano.Wallet.API.V1.Types (Account (..), AccountIndex, AccountUpdate,
                                              AddressValidity (..), NewAccount (..),
                                              NewAddress (..), NewWallet (..), V1 (..), Wallet,
                                              WalletAddress (..), WalletId, WalletOperation (..),
                                              WalletUpdate)

import           Pos.Client.KeyStorage (MonadKeys)
import           Pos.Core (ChainDifficulty)
import           Pos.Crypto (PassPhrase, emptyPassphrase)

import           Pos.Wallet.Web.Tracking.Types (SyncQueue)

import           Pos.Util (HasLens', maybeThrow)
import           Pos.Wallet.Web.Account (GenSeed (..))
import           Pos.Wallet.Web.ClientTypes.Types (CWallet (..), CWalletInit (..), CWalletMeta (..))
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogicRead)
import qualified Pos.Wallet.Web.Methods.Logic as V0
import qualified Pos.Wallet.Web.Methods.Misc as V0
import           Pos.Wallet.Web.Methods.Restore (newWallet, restoreWalletFromSeed)
import           Pos.Wallet.Web.State.State (WalletDbReader, askWalletDB, askWalletSnapshot,
                                             getWalletAddresses, setWalletMeta)
import           Pos.Wallet.Web.State.Storage (getWalletInfo)


-- | Let's unify all the requirements for the legacy wallet.
type MonadLegacyWallet ctx m =
    ( WalletDbReader ctx m
    , HasLens' ctx SyncQueue
    , MonadUnliftIO m
    , MonadIO m
    , MonadThrow m
    , MonadWalletLogicRead ctx m
    , MonadKeys m
    )


-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall ctx m n a. (MonadMask n, MonadLegacyWallet ctx m)
    => (PassiveWalletLayer m -> n a) -> n a
bracketPassiveWallet =
    bracket
        (pure passiveWalletLayer)
        (\_ -> return ())


-- | Initialize the active wallet.
-- The active wallet is allowed all.
bracketActiveWallet
    :: forall ctx m n a. (MonadMask n, MonadLegacyWallet ctx m)
    => PassiveWalletLayer m
    -> WalletDiffusion
    -> (ActiveWalletLayer m -> n a) -> n a
bracketActiveWallet walletPassiveLayer walletDiffusion =
    bracket
      (return ActiveWalletLayer{..})
      (\_ -> return ())


passiveWalletLayer
    :: forall ctx m. MonadLegacyWallet ctx m
    => PassiveWalletLayer m
passiveWalletLayer = PassiveWalletLayer
    { _pwlCreateWallet      = monadThrowToEither ... pwlCreateWallet
    , _pwlGetWalletIds      = monadThrowToEither ... pwlGetWalletIds
    , _pwlGetWallet         = monadThrowToEither ... pwlGetWallet
    , _pwlUpdateWallet      = monadThrowToEither ... pwlUpdateWallet
    , _pwlDeleteWallet      = monadThrowToEither ... pwlDeleteWallet

    , _pwlCreateAccount     = monadThrowToEither ... pwlCreateAccount
    , _pwlGetAccounts       = monadThrowToEither ... pwlGetAccounts
    , _pwlGetAccount        = monadThrowToEither ... pwlGetAccount
    , _pwlUpdateAccount     = monadThrowToEither ... pwlUpdateAccount
    , _pwlDeleteAccount     = monadThrowToEither ... pwlDeleteAccount

    , _pwlCreateAddress     = monadThrowToEither ... pwlCreateAddress
    , _pwlGetAddresses      = monadThrowToEither ... pwlGetAddresses
    , _pwlIsAddressValid    = monadThrowToEither ... pwlIsAddressValid
    }

------------------------------------------------------------
-- Wallet
------------------------------------------------------------

pwlCreateWallet
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => NewWallet
    -> m Wallet
pwlCreateWallet NewWallet{..} = do

    let spendingPassword = fromMaybe mempty $ coerce newwalSpendingPassword
    let backupPhrase     = coerce newwalBackupPhrase

    initMeta    <- CWalletMeta  <$> pure newwalName
                                <*> migrate newwalAssuranceLevel
                                <*> pure 0

    let walletInit = CWalletInit initMeta backupPhrase

    wallet      <- newWalletHandler newwalOperation spendingPassword walletInit
    wId         <- migrate $ cwId wallet

    -- Get wallet or throw if missing.
    maybeThrow (WalletNotFound wId) =<< pwlGetWallet wId
  where
    -- | We have two functions which are very similar.
    newWalletHandler :: WalletOperation -> PassPhrase -> CWalletInit -> m CWallet
    newWalletHandler CreateWallet  = newWallet
    newWalletHandler RestoreWallet = restoreWalletFromSeed


pwlGetWalletIds
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => m [WalletId]
pwlGetWalletIds = do
    ws          <- askWalletSnapshot
    migrate $ getWalletAddresses ws


pwlGetWallet
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> m (Maybe Wallet)
pwlGetWallet wId = do
    ws          <- askWalletSnapshot

    cWId        <- migrate wId
    wallet      <- V0.getWallet cWId

    pure $ do
        walletInfo  <- getWalletInfo cWId ws
        migrate (wallet, walletInfo, Nothing @ChainDifficulty)


pwlUpdateWallet
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> WalletUpdate
    -> m Wallet
pwlUpdateWallet wId wUpdate = do
    walletDB    <- askWalletDB

    cWId        <- migrate wId
    cWMeta      <- migrate wUpdate

    -- Update the data
    setWalletMeta walletDB cWId cWMeta

    -- Get wallet or throw if missing.
    maybeThrow (WalletNotFound wId) =<< pwlGetWallet wId


pwlDeleteWallet
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> m ()
pwlDeleteWallet wId = do
    cWId        <- migrate wId
    _           <- V0.deleteWallet cWId
    pure ()


------------------------------------------------------------
-- Account
------------------------------------------------------------


pwlCreateAccount
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> NewAccount
    -> m Account
pwlCreateAccount wId newAcc@NewAccount{..} = do

    let spendingPassword = fromMaybe mempty . fmap coerce $ naccSpendingPassword

    accInit     <- migrate (wId, newAcc)
    cAccount    <- V0.newAccount RandomSeed spendingPassword accInit

    migrate cAccount


pwlGetAccounts
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> m [Account]
pwlGetAccounts wId = do
    cWId        <- migrate wId
    cAccounts   <- V0.getAccounts $ Just cWId
    migrate cAccounts


pwlGetAccount
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> AccountIndex
    -> m (Maybe Account)
pwlGetAccount wId aId = do
    accId       <- migrate (wId, aId)
    account     <- V0.getAccount accId
    Just <$> migrate account


pwlUpdateAccount
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> AccountIndex
    -> AccountUpdate
    -> m Account
pwlUpdateAccount wId accIdx accUpdate = do
    newAccId    <- migrate (wId, accIdx)
    accMeta     <- migrate accUpdate
    cAccount    <- V0.updateAccount newAccId accMeta
    migrate cAccount


pwlDeleteAccount
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> AccountIndex
    -> m ()
pwlDeleteAccount wId accIdx = do
    accId       <- migrate (wId, accIdx)
    _           <- V0.deleteAccount accId
    pure ()


------------------------------------------------------------
-- Address
------------------------------------------------------------


pwlCreateAddress
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => NewAddress
    -> m WalletAddress
pwlCreateAddress NewAddress{..} = do
    let password = fromMaybe emptyPassphrase $ coerce newaddrSpendingPassword

    accountId   <- migrate (newaddrWalletId, newaddrAccountIndex)
    newAddr     <- V0.newAddress RandomSeed password accountId

    migrate newAddr


pwlGetAddresses
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> Maybe AccountIndex
    -> m [WalletAddress]
pwlGetAddresses wId Nothing    = pwlGetWalletAddresses wId
pwlGetAddresses wId (Just aId) = pwlGetAccountAddresses wId aId


-- | We use this function if we get just the @WalletId@.
pwlGetWalletAddresses
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> m [WalletAddress]
pwlGetWalletAddresses wId = do
    accounts    <- pwlGetAccounts wId
    pure $ concatMap accAddresses accounts


-- | We use this function if we get both.
pwlGetAccountAddresses
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> AccountIndex
    -> m [WalletAddress]
pwlGetAccountAddresses wId aIdx = do
    account     <- pwlGetAccount wId aIdx
    accAddresses <$> maybeThrow
        (AccountNotFound wId aIdx)
        account


pwlIsAddressValid
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletAddress
    -> m AddressValidity
pwlIsAddressValid wAddr = do
    cAddress    <- migrate $ addrId wAddr
    isValid     <- V0.isValidAddress cAddress
    pure AddressValidity{..}


