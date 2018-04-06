{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.WalletLayer.Legacy
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           Control.Monad.Catch (catchAll)

import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..), PassiveWalletLayer (..))

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))

import           Pos.Client.KeyStorage (MonadKeys)
import           Cardano.Wallet.API.V1.Migration (migrate)
import           Cardano.Wallet.API.V1.Migration.Types ()
import           Cardano.Wallet.API.V1.Types (Account, AccountIndex, Address, Wallet, WalletId)

import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogicRead, getAccount, getAccounts,
                                               getWallet, deleteWallet)
import           Pos.Wallet.Web.State.State (WalletDbReader, askWalletSnapshot, getWalletAddresses)
import           Pos.Wallet.Web.State.Storage (getWalletInfo)


-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall ctx m n a.
    ( MonadMask n
    , WalletDbReader ctx m
    , MonadIO m
    , MonadThrow m
    , MonadWalletLogicRead ctx m
    , MonadKeys m
    )
    => (PassiveWalletLayer m -> n a) -> n a
bracketPassiveWallet =
    bracket
        (pure passiveWalletLayer)
        (\_ -> return ())
  where

    pwlGetWalletIds :: m [WalletId]
    pwlGetWalletIds = do
        ws          <- askWalletSnapshot
        migrate $ getWalletAddresses ws

    pwlGetWallet :: WalletId -> m (Maybe Wallet)
    pwlGetWallet wId = do
        ws          <- askWalletSnapshot
        cWId        <- migrate wId
        wallet      <- getWallet cWId

        pure $ do
            walletInfo <- getWalletInfo cWId ws
            migrate (wallet, walletInfo)

    -- | Seems silly, but we do need some sort of feedback from
    -- the DB.
    pwlDeleteWallet :: WalletId -> m Bool
    pwlDeleteWallet wId = do
        cWId        <- migrate wId
        -- TODO(ks): It would be better to catch specific @Exception@.
        catchAll (fmap (const True) $ deleteWallet cWId) (const . pure $ False)

    pwlGetAccounts :: WalletId -> m [Account]
    pwlGetAccounts wId = do
        cWId        <- migrate wId
        cAccounts   <- getAccounts $ Just cWId
        migrate cAccounts

    pwlGetAccount  :: WalletId -> AccountIndex -> m (Maybe Account)
    pwlGetAccount wId aId = do
        accId       <- migrate (wId, aId)
        account     <- getAccount accId
        fmap Just $ migrate account

    pwlGetAddresses :: WalletId -> m [Address]
    pwlGetAddresses = error "Not implemented!"

    passiveWalletLayer :: PassiveWalletLayer m
    passiveWalletLayer = PassiveWalletLayer {..}


-- | Initialize the active wallet.
-- The active wallet is allowed all.
bracketActiveWallet
    :: forall ctx m n a.
    ( MonadMask n
    , WalletDbReader ctx m
    , MonadIO m
    , MonadThrow m
    , MonadWalletLogicRead ctx m
    , MonadKeys m
    )
    => PassiveWalletLayer m
    -> WalletDiffusion
    -> (ActiveWalletLayer m -> n a) -> n a
bracketActiveWallet walletPassiveLayer walletDiffusion =
    bracket
      (return ActiveWalletLayer{..})
      (\_ -> return ())

