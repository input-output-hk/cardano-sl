-- | Run 'GetWallets' client.

module Client.Pos.Wallet.Web.Endpoint.GetWallets
    ( getWalletsIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api (getWallets)
import           Client.Pos.Wallet.Web.Run (runEndpointClient)
import           Bench.Pos.Wallet.Types    (WalletsConfig (..))

-- | Run 'GetWallets' client. As a result we will get a list of wallets.
getWalletsIO :: WalletsConfig -> IO ()
getWalletsIO WalletsConfig {..} =
    runEndpointClient getWallets >>= \case
        Left problem -> putText $ "Cannot obtain wallets information: " <> problem
        Right walletsInfo -> print walletsInfo     -- :: [CWallet]
