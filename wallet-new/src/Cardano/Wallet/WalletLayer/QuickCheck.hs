{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.WalletLayer.QuickCheck
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..), PassiveWalletLayer (..))
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Orphans.Arbitrary () -- Arbitrary instances

import           Test.QuickCheck (Arbitrary, arbitrary, generate)

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (MonadMask m, MonadIO n)
    => (PassiveWalletLayer n -> m a) -> m a
bracketPassiveWallet =
    bracket
        (pure passiveWalletLayer)
        (\_ -> return ())
  where
    passiveWalletLayer :: PassiveWalletLayer n
    passiveWalletLayer = PassiveWalletLayer
        { _pwlCreateWallet      = \_     -> Right <$> liftedGen
        , _pwlGetWalletIds      =           Right <$> liftedGen
        , _pwlGetWallet         = \_     -> Right <$> liftedGen
        , _pwlUpdateWallet      = \_ _   -> Right <$> liftedGen
        , _pwlDeleteWallet      = \_     -> Right <$> liftedGen

        , _pwlCreateAccount     = \_ _   -> Right <$> liftedGen
        , _pwlGetAccounts       = \_     -> Right <$> liftedGen
        , _pwlGetAccount        = \_ _   -> Right <$> liftedGen
        , _pwlUpdateAccount     = \_ _ _ -> Right <$> liftedGen
        , _pwlDeleteAccount     = \_ _   -> Right <$> liftedGen

        , _pwlCreateAddress     = \_     -> Right <$> liftedGen
        , _pwlGetAddresses      = \_ _   -> Right <$> liftedGen
        , _pwlIsAddressValid    = \_     -> Right <$> liftedGen

        , _pwlCreateTx          = \_ _   -> Right <$> liftedGen
        , _pwlGetTxs            = \_ _ _ -> Right <$> liftedGen
        , _pwlEstimateFees      = \_     -> Right <$> liftedGen

        , _pwlGetSettings       =           Right <$> liftedGen

        , _pwlGetInfo           = \_     -> Right <$> liftedGen
        }

    -- | A utility function.
    liftedGen :: forall b. (MonadIO n, Arbitrary b) => n b
    liftedGen = liftIO . generate $ arbitrary

-- | Initialize the active wallet.
-- The active wallet is allowed all.
bracketActiveWallet
    :: forall m n a. (MonadMask m, MonadIO n)
    => PassiveWalletLayer n
    -> WalletDiffusion
    -> (ActiveWalletLayer n -> m a) -> m a
bracketActiveWallet walletPassiveLayer walletDiffusion =
    bracket
      (return ActiveWalletLayer{..})
      (\_ -> return ())

