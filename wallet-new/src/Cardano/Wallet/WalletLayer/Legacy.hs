{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.WalletLayer.Legacy
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..), PassiveWalletLayer (..))

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))

import           Cardano.Wallet.API.V1.Migration (migrate)
import           Pos.Wallet.Web.State.State (WalletDbReader, askWalletSnapshot, getWalletAddresses,
                                             getWalletMeta)

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall ctx m n a. (MonadMask m, WalletDbReader ctx n, MonadIO n, MonadThrow n)
    => (PassiveWalletLayer n -> m a) -> m a
bracketPassiveWallet =
    bracket
        (pure passiveWalletLayer)
        (\_ -> return ())
  where
    passiveWalletLayer :: PassiveWalletLayer n
    passiveWalletLayer = PassiveWalletLayer
        { pwlGetWalletAddresses  = askWalletSnapshot >>= \ws -> migrate $ getWalletAddresses ws
        , pwlGetWalletMeta       = \cIdWal -> askWalletSnapshot >>= \ws -> pure $ getWalletMeta ws =<< (migrate cIdWal)
        }

-- | Initialize the active wallet.
-- The active wallet is allowed all.
bracketActiveWallet
    :: forall ctx m n a. (MonadMask m, WalletDbReader ctx n, MonadIO n, MonadThrow n)
    => WalletDiffusion
    -> (ActiveWalletLayer n -> m a) -> m a
bracketActiveWallet walletDiffusion =
    bracket
        (bracketPassiveWallet $ \walletPassiveLayer -> return ActiveWalletLayer {..})
        (\_ -> return ())
