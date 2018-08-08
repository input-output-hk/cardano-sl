module Cardano.Wallet.WalletLayer
    ( -- * Kernel
      bracketKernelPassiveWallet
    , bracketKernelActiveWallet
    -- * Legacy
    , bracketLegacyPassiveWallet
    , bracketLegacyActiveWallet
    -- * We re-export the types since we want all the dependencies
    -- in this module, other modules shouldn't be touched.
    , module Types
    ) where

import           Universum

import           System.Wlog (Severity)

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))

import           Cardano.Wallet.Kernel (ActiveWallet, PassiveWallet)
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import qualified Cardano.Wallet.WalletLayer.Kernel as Kernel
import qualified Cardano.Wallet.WalletLayer.Legacy as Legacy
import           Cardano.Wallet.WalletLayer.Types as Types
import           Pos.Core.Configuration (ProtocolMagic)

------------------------------------------------------------
-- Kernel
------------------------------------------------------------
bracketKernelPassiveWallet
    :: forall m n a. (MonadIO m, MonadIO n, MonadMask n)
    => (Severity -> Text -> IO ())
    -> Keystore
    -> NodeStateAdaptor IO
    -> (PassiveWalletLayer m -> PassiveWallet -> n a) -> n a
bracketKernelPassiveWallet = Kernel.bracketPassiveWallet

bracketKernelActiveWallet
    :: forall m n a. (MonadIO n, MonadMask n, MonadIO m)
    => ProtocolMagic
    -> PassiveWalletLayer m
    -> PassiveWallet
    -> WalletDiffusion
    -> (ActiveWalletLayer m -> ActiveWallet -> n a) -> n a
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
