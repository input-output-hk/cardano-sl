module Cardano.Wallet.API.V1.Handlers.Info (handlers) where

import           Universum

import           Servant

import           Cardano.Wallet.API.Response (WalletResponse, single)
import qualified Cardano.Wallet.API.V1.Info as Info
import           Cardano.Wallet.API.V1.Types (NodeInfo)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

handlers :: ActiveWalletLayer IO -> ServerT Info.API Handler
handlers = getNodeInfo

getNodeInfo :: ActiveWalletLayer IO -> Handler (WalletResponse NodeInfo)
getNodeInfo w = liftIO $ single <$> WalletLayer.getNodeInfo w
