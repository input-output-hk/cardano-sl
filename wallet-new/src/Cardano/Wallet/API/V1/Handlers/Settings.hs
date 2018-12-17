module Cardano.Wallet.API.V1.Handlers.Settings (handlers) where

import           Universum

import           Servant

import           Cardano.Wallet.API.Response (WalletResponse, single)
import qualified Cardano.Wallet.API.V1.Settings as Settings
import           Cardano.Wallet.API.V1.Types (NodeSettings)
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

handlers :: PassiveWalletLayer IO -> ServerT Settings.API Handler
handlers = getNodeSettings

-- | Retrieve the static settings for this node
getNodeSettings :: PassiveWalletLayer IO
                -> Handler (WalletResponse NodeSettings)
getNodeSettings w = liftIO $ single <$> WalletLayer.getNodeSettings w
