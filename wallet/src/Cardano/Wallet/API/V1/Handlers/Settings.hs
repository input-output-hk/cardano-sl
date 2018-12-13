module Cardano.Wallet.API.V1.Handlers.Settings (handlers) where

import           Universum

import           Servant

import           Cardano.Wallet.API.Response (APIResponse, single)
import           Cardano.Wallet.API.V1.Types (NodeSettings)
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

import qualified Pos.Node.API as Node

handlers :: PassiveWalletLayer IO -> ServerT Node.SettingsAPI Handler
handlers = getNodeSettings

-- | Retrieve the static settings for this node
getNodeSettings :: PassiveWalletLayer IO
                -> Handler (APIResponse NodeSettings)
getNodeSettings w = liftIO $ single <$> WalletLayer.getNodeSettings w
