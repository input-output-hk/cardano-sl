module Cardano.Wallet.WalletLayer.Kernel
    ( bracketKernelWallet
    ) where

import           Universum
import           System.Wlog (Severity (..))

import           Cardano.Wallet.WalletLayer (PassiveWalletLayer (..))

import           Cardano.Wallet.Kernel (bracketPassiveWallet)


bracketKernelWallet
    :: forall m n a. (MonadMask m, Monad n)
    => (Severity -> Text -> IO ())
    -> (PassiveWalletLayer n -> m a) -> m a
bracketKernelWallet walletLogMessage =
    bracket
        (bracketPassiveWallet walletLogMessage bracketNewWallet)
        (\_ -> return ())
  where
    -- | TODO(ks): Currently not implemented!
    bracketNewWallet _wallet =
        pure $ PassiveWalletLayer
            { pwlGetWalletAddresses  = error "Not implemented!"
            , pwlGetWalletMeta       = error "Not implemented!"

            , pwlWalletLogMessage    = walletLogMessage
            }

