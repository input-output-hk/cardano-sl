module Cardano.Wallet.WalletLayer
    ( -- * Kernel
      bracketKernelPassiveWallet
    , bracketKernelActiveWallet
    -- * Legacy
    , bracketLegacyPassiveWallet
    , bracketLegacyActiveWallet
    -- * QuickCheck
    , bracketQuickCheckPassiveWallet
    , bracketQuickCheckActiveWallet
    -- * We re-export the types since we want all the dependencies
    -- in this module, other modules shouldn't be touched.
    , module Cardano.Wallet.WalletLayer.Types
    ) where

import           Universum

import           System.Wlog (Severity)

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))

import           Cardano.Wallet.Kernel (PassiveWallet)
import qualified Cardano.Wallet.WalletLayer.Kernel as Kernel
import qualified Cardano.Wallet.WalletLayer.Legacy as Legacy
import qualified Cardano.Wallet.WalletLayer.QuickCheck as QuickCheck
import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..),
                     PassiveWalletLayer (..), applyBlocks, rollbackBlocks)

------------------------------------------------------------
-- Kernel
------------------------------------------------------------
bracketKernelPassiveWallet
    :: forall m n a. (MonadIO m, MonadIO n, MonadMask n)
    => (Severity -> Text -> IO ())
    -> (PassiveWalletLayer m -> PassiveWallet -> n a) -> n a
bracketKernelPassiveWallet = Kernel.bracketPassiveWallet

bracketKernelActiveWallet
    :: forall m n a. (MonadIO n, MonadMask n)
    => PassiveWalletLayer m
    -> PassiveWallet
    -> WalletDiffusion -> (ActiveWalletLayer m -> n a) -> n a
bracketKernelActiveWallet  = Kernel.bracketActiveWallet

------------------------------------------------------------
-- Legacy
------------------------------------------------------------

bracketLegacyPassiveWallet
    :: forall ctx m n a. (MonadMask n, Legacy.MonadLegacyWallet ctx m)
    => (PassiveWalletLayer m -> n a) -> n a
bracketLegacyPassiveWallet = Legacy.bracketPassiveWallet

bracketLegacyActiveWallet
    :: forall m n a. (MonadMask n)
    => PassiveWalletLayer m -> WalletDiffusion -> (ActiveWalletLayer m -> n a) -> n a
bracketLegacyActiveWallet  = Legacy.bracketActiveWallet

------------------------------------------------------------
-- QuickCheck
------------------------------------------------------------

bracketQuickCheckPassiveWallet
    :: forall m n a. (MonadMask n, MonadIO m)
    => (PassiveWalletLayer m -> n a) -> n a
bracketQuickCheckPassiveWallet = QuickCheck.bracketPassiveWallet

bracketQuickCheckActiveWallet
    :: forall m n a. (MonadMask n)
    => PassiveWalletLayer m -> WalletDiffusion -> (ActiveWalletLayer m -> n a) -> n a
bracketQuickCheckActiveWallet  = QuickCheck.bracketActiveWallet
