{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.WalletLayer.QuickCheck
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Orphans.Arbitrary ()
import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..),
                     CreateAccountError (..), PassiveWalletLayer (..))

import qualified Cardano.Wallet.Kernel.Accounts as Kernel

import           Pos.Core ()
import           Test.QuickCheck (Arbitrary, arbitrary, generate, oneof)

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

        , _pwlCreateAddress  = \_     -> liftedGen
        , _pwlGetAddresses   = \_     -> liftedGen

        , _pwlApplyBlocks    = \_     -> liftedGen
        , _pwlRollbackBlocks = \_     -> liftedGen
       }

-- | A utility function.
liftedGen :: forall b n. (MonadIO n, Arbitrary b) => n b
liftedGen = liftIO . generate $ arbitrary

-- | Initialize the active wallet.
-- The active wallet is allowed all.
bracketActiveWallet
    :: forall m n a. (MonadMask m)
    => PassiveWalletLayer n
    -> WalletDiffusion
    -> (ActiveWalletLayer n -> m a) -> m a
bracketActiveWallet walletPassiveLayer _walletDiffusion =
    bracket
      (return activeWalletLayer)
      (\_ -> return ())
  where
    activeWalletLayer :: ActiveWalletLayer n
    activeWalletLayer = ActiveWalletLayer {
          walletPassiveLayer = walletPassiveLayer
        , pay          = \_ _ _ -> error "unimplemented"
        , estimateFees = \_ _ _ -> error "unimplemented"
        }


{-----------------------------------------------------------------------------
Orphan instances, in preparation for rehoming them.
------------------------------------------------------------------------------}

instance Arbitrary CreateAccountError where
    arbitrary = oneof [ CreateAccountError <$> arbitrary
                      , pure (CreateAccountWalletIdDecodingFailed "foobar")
                      , CreateAccountTimeLimitReached <$> arbitrary
                      ]

instance Arbitrary Kernel.CreateAccountError where
    arbitrary = oneof [ Kernel.CreateAccountUnknownHdRoot <$> arbitrary
                      , Kernel.CreateAccountHdRndAccountSpaceSaturated <$> arbitrary
                      ]
