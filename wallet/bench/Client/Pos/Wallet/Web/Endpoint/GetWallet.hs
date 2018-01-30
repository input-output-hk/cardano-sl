-- | Run 'GetWallets' client.

module Client.Pos.Wallet.Web.Endpoint.GetWallet
    ( getWalletIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api  (getWallet)
import           Client.Pos.Wallet.Web.Run  (runEndpointClient)
import           Bench.Pos.Wallet.Types     (CompleteConfig (..))
import           Bench.Pos.Wallet.Random    (pickRandomWalletIdFrom)

-- | Run 'GetWallet' client. As a result we will get a particular wallet.
getWalletIO :: CompleteConfig -> IO ()
getWalletIO conf@CompleteConfig {..} = do
    walletId <- pickRandomWalletIdFrom walletsConfig
    runEndpointClient conf (getWallet walletId) >>= \case
        Left problem -> putText $ "Cannot obtain wallet information: " <> problem
        Right _ -> return ()
