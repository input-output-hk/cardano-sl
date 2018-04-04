module Cardano.Wallet.WalletLayer
    ( WalletBracketable (..)
    , WalletTypeKernel (..)
    , WalletTypeLegacy (..)
    , WalletTypeQuickCheck (..)
    -- * We re-export the types since we want all the dependencies
    -- in this module, other module shouldn't be touched.
    , module Cardano.Wallet.WalletLayer.Types
    ) where

import           Universum

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))

import qualified Cardano.Wallet.WalletLayer.Kernel as Kernel
import qualified Cardano.Wallet.WalletLayer.Legacy as Legacy
import qualified Cardano.Wallet.WalletLayer.QuickCheck as QuickCheck
import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..), PassiveWalletLayer (..))

import           Pos.Wallet.Web.State.State (WalletDbReader)


-----------------------------------------------------------
-- Data types
-----------------------------------------------------------

data WalletTypeKernel
    = Kernel
    deriving (Eq, Show)

data WalletTypeLegacy
    = Legacy
    deriving (Eq, Show)

data WalletTypeQuickCheck
    = QuickCheck
    deriving (Eq, Show)

-----------------------------------------------------------
-- Class and instances
-----------------------------------------------------------

-- | The typeclass that unifies all the underlying wallets.
class WalletBracketable b m where

    bracketPassiveWallet
        :: forall n a. (MonadMask n)
        => b
        -> (PassiveWalletLayer m -> n a) -> n a

    bracketActiveWallet
        :: forall n a. (MonadMask n)
        => b
        -> PassiveWalletLayer m
        -> WalletDiffusion
        -> (ActiveWalletLayer m -> n a) -> n a


instance (Monad m) => WalletBracketable WalletTypeKernel m where
    bracketPassiveWallet _ = Kernel.bracketPassiveWallet
    bracketActiveWallet  _ = Kernel.bracketActiveWallet

instance (WalletDbReader ctx m, MonadIO m, MonadThrow m) => WalletBracketable WalletTypeLegacy m where
    bracketPassiveWallet _ = Legacy.bracketPassiveWallet
    bracketActiveWallet  _ = Legacy.bracketActiveWallet

instance (MonadIO m) => WalletBracketable WalletTypeQuickCheck m where
    bracketPassiveWallet _ = QuickCheck.bracketPassiveWallet
    bracketActiveWallet  _ = QuickCheck.bracketActiveWallet

