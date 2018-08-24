module Cardano.Wallet.API.Internal.Handlers (handlers) where

import           Universum

import           Servant

import           Pos.Core.Update (SoftwareVersion)

import qualified Cardano.Wallet.API.Internal as Internal
import           Cardano.Wallet.API.V1.Types (V1)
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

handlers :: PassiveWalletLayer IO -> ServerT Internal.API Handler
handlers w = nextUpdate       w
        :<|> applyUpdate      w
        :<|> postponeUpdate   w
        :<|> resetWalletState w

nextUpdate :: PassiveWalletLayer IO -> Handler (V1 SoftwareVersion)
nextUpdate w = do
    mUpd <- liftIO $ WalletLayer.nextUpdate w
    case mUpd of
      Just upd -> return upd
      Nothing  -> throwError err404

applyUpdate :: PassiveWalletLayer IO -> Handler NoContent
applyUpdate w = liftIO (WalletLayer.applyUpdate w) >> return NoContent

postponeUpdate :: PassiveWalletLayer IO -> Handler NoContent
postponeUpdate w = liftIO (WalletLayer.postponeUpdate w) >> return NoContent

resetWalletState :: PassiveWalletLayer IO -> Handler NoContent
resetWalletState w = liftIO (WalletLayer.resetWalletState w) >> return NoContent
