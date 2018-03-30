module Cardano.Wallet.WalletLayer
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    , bracketActiveWalletLayer
    , init
    ) where

import           Universum

import           System.Wlog (Severity (..))

import           Cardano.Wallet.API.V1.Types (WalletId)
import           Pos.Wallet.Web.ClientTypes (CWalletMeta)

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))


-- | The wallet data layer.
data PassiveWalletLayer m = PassiveWalletLayer
    { pwlGetWalletAddresses :: m [WalletId]
    , pwlGetWalletMeta      :: WalletId -> m (Maybe CWalletMeta)

    -- | We require this to be visible from "the outside".
    , pwlWalletLogMessage   :: Severity -> Text -> IO ()
    }

--makeLenses ''PassiveWalletLayer

-- | Initialize the wallet
init :: forall m. PassiveWalletLayer m -> IO ()
init PassiveWalletLayer{..} = do
    pwlWalletLogMessage Info "Wallet kernel initialized"


-- | Active wallet layer
--
-- An active wallet layer. See @ActiveWallet@.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet layer
      walletPassiveLayer :: PassiveWalletLayer m

      -- | The wallet diffusion layer
    , walletDiffusion    :: WalletDiffusion
    }

-- | Initialize the active wallet
bracketActiveWalletLayer
    :: forall m n a. (MonadMask m, Monad n)
    => PassiveWalletLayer n
    -> WalletDiffusion
    -> (ActiveWalletLayer n -> m a) -> m a
bracketActiveWalletLayer walletPassiveLayer walletDiffusion =
    bracket
      (return ActiveWalletLayer{..})
      (\_ -> return ())

