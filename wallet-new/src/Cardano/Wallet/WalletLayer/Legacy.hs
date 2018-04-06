{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.WalletLayer.Legacy
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..), PassiveWalletLayer (..))

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))

import           Cardano.Wallet.API.V1.Migration (migrate)
import           Cardano.Wallet.API.V1.Migration.Types ()
import           Cardano.Wallet.API.V1.Types (Account, Address, Wallet, WalletId)

import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogicRead, getAccounts, getWallet)
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

    pwlGetAccounts :: WalletId -> m [Account]
    pwlGetAccounts wId = do
        cWId        <- migrate wId
        cAccounts   <- getAccounts $ Just cWId
        migrate cAccounts

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
    )
    => PassiveWalletLayer m
    -> WalletDiffusion
    -> (ActiveWalletLayer m -> n a) -> n a
bracketActiveWallet walletPassiveLayer walletDiffusion =
    bracket
      (return ActiveWalletLayer{..})
      (\_ -> return ())

