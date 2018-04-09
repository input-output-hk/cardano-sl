{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds     #-}

module Cardano.Wallet.WalletLayer.Legacy
    ( MonadLegacyWallet
    , bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           Control.Monad.Catch (catchAll)
import           Data.Coerce (coerce)

import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..), PassiveWalletLayer (..))

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))

import           Cardano.Wallet.API.V1.Migration (migrate)
import           Cardano.Wallet.API.V1.Migration.Types ()
import           Cardano.Wallet.API.V1.Types (Account, AccountIndex, Address, NewWallet (..),
                                              V1 (..), Wallet, WalletId, WalletOperation (..),
                                              WalletUpdate)

import           Pos.Client.KeyStorage (MonadKeys)
import           Pos.Crypto (PassPhrase)
import           Pos.Wallet.Web.ClientTypes.Types (CWallet (..), CWalletInit (..), CWalletMeta (..))
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogicRead, deleteWallet, getAccount,
                                               getAccounts, getWallet)
import           Pos.Wallet.Web.Methods.Restore (newWallet, restoreWallet)
import           Pos.Wallet.Web.State.State (WalletDbReader, askWalletSnapshot, askWalletDB, getWalletAddresses, setWalletMeta)
import           Pos.Wallet.Web.State.Storage (getWalletInfo)


-- | Let's unify all the requirements for the legacy wallet.
type MonadLegacyWallet ctx m =
    ( WalletDbReader ctx m
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
  where
    passiveWalletLayer :: PassiveWalletLayer m
    passiveWalletLayer = PassiveWalletLayer
        { _pwlCreateWallet  = pwlCreateWallet
        , _pwlGetWalletIds  = pwlGetWalletIds
        , _pwlGetWallet     = pwlGetWallet
        , _pwlUpdateWallet  = pwlUpdateWallet
        , _pwlDeleteWallet  = pwlDeleteWallet

        , _pwlGetAccounts   = pwlGetAccounts
        , _pwlGetAccount    = pwlGetAccount

        , _pwlGetAddresses  = pwlGetAddresses
        }


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

------------------------------------------------------------
-- Wallet
------------------------------------------------------------

pwlCreateWallet
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => NewWallet
    -> m (Maybe Wallet)
pwlCreateWallet NewWallet{..} = do

    let spendingPassword = fromMaybe mempty $ coerce newwalSpendingPassword
    let backupPhrase     = coerce newwalBackupPhrase

    initMeta    <- CWalletMeta  <$> pure newwalName
                                <*> migrate newwalAssuranceLevel
                                <*> pure 0

    let walletInit = CWalletInit initMeta backupPhrase

    wallet      <- newWalletHandler newwalOperation spendingPassword walletInit
    wId         <- migrate $ cwId wallet

    pwlGetWallet wId
  where
    -- | We have two functions which are very similar.
    newWalletHandler :: WalletOperation -> PassPhrase -> CWalletInit -> m CWallet
    newWalletHandler CreateWallet  = newWallet
    newWalletHandler RestoreWallet = restoreWallet


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
    wallet      <- getWallet cWId

    pure $ do
        walletInfo  <- getWalletInfo cWId ws
        migrate (wallet, walletInfo)

pwlUpdateWallet
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> WalletUpdate
    -> m (Maybe Wallet)
pwlUpdateWallet wId wUpdate = do
    walletDB    <- askWalletDB

    cWId        <- migrate wId
    cWMeta      <- migrate wUpdate

    -- Update the data
    setWalletMeta walletDB cWId cWMeta

    pwlGetWallet wId

-- | Seems silly, but we do need some sort of feedback from
-- the DB.
pwlDeleteWallet
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> m Bool
pwlDeleteWallet wId = do
    cWId        <- migrate wId
    -- TODO(ks): It would be better to catch specific @Exception@.
    -- Maybe @try@?
    catchAll (fmap (const True) $ deleteWallet cWId) (const . pure $ False)

------------------------------------------------------------
-- Account
------------------------------------------------------------

pwlGetAccounts
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> m [Account]
pwlGetAccounts wId = do
    cWId        <- migrate wId
    cAccounts   <- getAccounts $ Just cWId
    migrate cAccounts

pwlGetAccount
    :: forall ctx m. (MonadLegacyWallet ctx m)
    => WalletId
    -> AccountIndex
    -> m (Maybe Account)
pwlGetAccount wId aId = do
    accId       <- migrate (wId, aId)
    account     <- getAccount accId
    fmap Just $ migrate account

------------------------------------------------------------
-- Address
------------------------------------------------------------

pwlGetAddresses :: WalletId -> m [Address]
pwlGetAddresses = error "Not implemented!"

