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
        { _pwlCreateWallet   = \_     -> liftedGen
        , _pwlGetWalletIds   =           liftedGen
        , _pwlGetWallet      = \_     -> liftedGen
        , _pwlUpdateWallet   = \_ _   -> liftedGen
        , _pwlDeleteWallet   = \_     -> liftedGen

        , _pwlCreateAccount  = \_ _   -> liftedGen
        , _pwlGetAccounts    = \_     -> liftedGen
        , _pwlGetAccount     = \_ _   -> liftedGen
        , _pwlUpdateAccount  = \_ _ _ -> liftedGen
        , _pwlDeleteAccount  = \_ _   -> liftedGen

        , _pwlGetAddresses   = \_     -> liftedGen

        , _pwlApplyBlocks    = \_     -> liftedGen
        , _pwlRollbackBlocks = \_     -> liftedGen
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
bracketActiveWallet walletPassiveLayer _walletDiffusion =
    bracket
      (return ActiveWalletLayer{..})
      (\_ -> return ())
