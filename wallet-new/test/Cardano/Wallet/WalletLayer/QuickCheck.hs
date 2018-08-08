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
                     CreateAccountError (..), DeleteAccountError (..),
                     DeleteWalletError (..), GetAccountError (..),
                     GetAccountsError (..), GetWalletError (..),
                     PassiveWalletLayer (..), UpdateAccountError (..),
                     UpdateWalletError (..), UpdateWalletPasswordError (..))

import           Cardano.Wallet.API.V1.Types (V1 (..))
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
        { _pwlCreateWallet         = \_     -> liftedGen
        , _pwlGetWallets           =           liftedGen
        , _pwlGetWallet            = \_     -> liftedGen
        , _pwlUpdateWallet         = \_ _   -> liftedGen
        , _pwlUpdateWalletPassword = \_ _   -> liftedGen
        , _pwlDeleteWallet         = \_     -> liftedGen

        , _pwlCreateAccount        = \_ _   -> liftedGen
        , _pwlGetAccounts          = \_     -> liftedGen
        , _pwlGetAccount           = \_ _   -> liftedGen
        , _pwlUpdateAccount        = \_ _ _ -> liftedGen
        , _pwlDeleteAccount        = \_ _   -> liftedGen

        , _pwlCreateAddress        = \_     -> liftedGen
        , _pwlGetAddresses         = \_     -> liftedGen

        , _pwlApplyBlocks          = \_     -> liftedGen
        , _pwlRollbackBlocks       = \_     -> liftedGen
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
 Note that most of them are bonkers -- they were put here just for the sake
 of the code to compile, as we are not actually using this particular layer
 anywhere.
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

instance Arbitrary GetAccountError where
    arbitrary = oneof [ GetAccountError . V1 <$> arbitrary
                      , GetAccountWalletIdDecodingFailed <$> arbitrary
                      ]

instance Arbitrary GetAccountsError where
    arbitrary = oneof [ GetAccountsError <$> arbitrary
                      , GetAccountsWalletIdDecodingFailed <$> arbitrary
                      ]

instance Arbitrary UpdateAccountError where
    arbitrary = oneof [ UpdateAccountError . V1 <$> arbitrary
                      , UpdateAccountWalletIdDecodingFailed <$> arbitrary
                      ]

instance Arbitrary DeleteAccountError where
    arbitrary = oneof [ DeleteAccountError . V1 <$> arbitrary
                      , DeleteAccountWalletIdDecodingFailed <$> arbitrary
                      ]

instance Arbitrary GetWalletError where
    arbitrary = oneof [ GetWalletWalletIdDecodingFailed <$> arbitrary
                      , GetWalletError . V1 <$> arbitrary
                      ]

instance Arbitrary UpdateWalletPasswordError where
    arbitrary = oneof [ UpdateWalletPasswordError <$> arbitrary
                      , UpdateWalletPasswordWalletIdDecodingFailed <$> arbitrary
                      ]

instance Arbitrary DeleteWalletError where
    arbitrary = oneof [ DeleteWalletWalletIdDecodingFailed <$> arbitrary ]

instance Arbitrary UpdateWalletError where
    arbitrary = oneof [ UpdateWalletWalletIdDecodingFailed <$> arbitrary ]
