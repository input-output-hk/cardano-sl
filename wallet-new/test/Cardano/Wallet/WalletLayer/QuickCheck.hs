{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.WalletLayer.QuickCheck
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer (..),
                     DeleteAccountError (..), DeleteWalletError (..),
                     GetAccountError (..), GetAccountsError (..),
                     GetUtxosError (..), GetWalletError (..),
                     ImportWalletError (..), PassiveWalletLayer (..),
                     UpdateAccountError (..), UpdateWalletError (..),
                     UpdateWalletPasswordError (..), ValidateAddressError (..))

import           Pos.Chain.Update (ConfirmedProposalState)
import           Pos.Core ()
import           Test.Pos.Chain.Txp.Arbitrary ()
import           Test.QuickCheck (Arbitrary (..), arbitrary, generate, oneof)


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
        { createWallet         = \_     -> liftedGen
        , getWallets           =           liftedGen
        , getWallet            = \_     -> liftedGen
        , updateWallet         = \_ _   -> liftedGen
        , updateWalletPassword = \_ _   -> liftedGen
        , deleteWallet         = \_     -> liftedGen
        , getUtxos             = \_     -> liftedGen

        , createAccount        = \_ _   -> liftedGen
        , getAccounts          = \_     -> liftedGen
        , getAccount           = \_ _   -> liftedGen
        , getAccountBalance    = \_ _   -> liftedGen
        , getAccountAddresses  = \_ _ _ _ -> liftedGen
        , updateAccount        = \_ _ _ -> liftedGen
        , deleteAccount        = \_ _   -> liftedGen

        , createAddress        = \_     -> liftedGen
        , getAddresses         = \_     -> liftedGen
        , validateAddress      = \_     -> liftedGen

        , getTransactions      = \_ _ _ _ _ _ -> liftedGen
        , getTxFromMeta        = \_ -> liftedGen

        , applyBlocks          = \_     -> liftedGen
        , rollbackBlocks       = \_     -> liftedGen

        , getNodeSettings      = liftedGen

        , nextUpdate           = liftedGen
        , applyUpdate          = liftedGen
        , postponeUpdate       = liftedGen
        , resetWalletState     = liftedGen
        , importWallet         = \_ -> liftedGen

        , waitForUpdate        = liftedGen
        , addUpdate            = \_ -> liftedGen
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
        , pay               = \_ _ _ -> error "unimplemented"
        , estimateFees      = \_ _ _ -> error "unimplemented"
        , createUnsignedTx  = \_ _ _ -> error "unimplemented"
        , submitSignedTx    = \_     -> error "unimplemented"
        , redeemAda         = \_     -> error "unimplemented"
        , getNodeInfo       =           error "unimplemented"
        }


{-----------------------------------------------------------------------------
 Orphan instances, in preparation for rehoming them.
 Note that most of them are bonkers -- they were put here just for the sake
 of the code to compile, as we are not actually using this particular layer
 anywhere.
------------------------------------------------------------------------------}

instance Arbitrary GetAccountError where
    arbitrary = oneof [ GetAccountError <$> arbitrary
                      , GetAccountWalletIdDecodingFailed <$> arbitrary
                      ]

instance Arbitrary GetAccountsError where
    arbitrary = oneof [ GetAccountsError <$> arbitrary
                      , GetAccountsWalletIdDecodingFailed <$> arbitrary
                      ]

instance Arbitrary UpdateAccountError where
    arbitrary = oneof [ UpdateAccountError <$> arbitrary
                      , UpdateAccountWalletIdDecodingFailed <$> arbitrary
                      ]

instance Arbitrary DeleteAccountError where
    arbitrary = oneof [ DeleteAccountError <$> arbitrary
                      , DeleteAccountWalletIdDecodingFailed <$> arbitrary
                      ]

instance Arbitrary GetWalletError where
    arbitrary = oneof [ GetWalletWalletIdDecodingFailed <$> arbitrary
                      , GetWalletError <$> arbitrary
                      ]

instance Arbitrary GetUtxosError where
    arbitrary = oneof [ pure (GetUtxosWalletIdDecodingFailed "foobar")
                      , GetUtxosGetAccountsError <$> arbitrary
                      , GetUtxosCurrentAvailableUtxoError <$> arbitrary
                      ]

instance Arbitrary UpdateWalletPasswordError where
    arbitrary = oneof [ UpdateWalletPasswordError <$> arbitrary
                      , UpdateWalletPasswordWalletIdDecodingFailed <$> arbitrary
                      ]

instance Arbitrary DeleteWalletError where
    arbitrary = oneof [ DeleteWalletWalletIdDecodingFailed <$> arbitrary ]

instance Arbitrary UpdateWalletError where
    arbitrary = oneof [ UpdateWalletWalletIdDecodingFailed <$> arbitrary ]

instance Arbitrary ValidateAddressError where
    arbitrary = oneof [ ValidateAddressDecodingFailed <$> arbitrary
                      ]

instance Arbitrary ImportWalletError where
    arbitrary = oneof [ ImportWalletFileNotFound <$> arbitrary
                      , ImportWalletNoWalletFoundInBackup <$> arbitrary
                      , ImportWalletCreationFailed <$> arbitrary
                      ]

-- This is obviously not a valid 'Arbitrary' instance, but one will be provided
-- when we will be start using this 'WalletLayer' implementation. Note how the
-- core layer already provides one, it's just a matter of exposing it to other
-- components and use it.
instance Arbitrary ConfirmedProposalState where
    arbitrary = oneof []
