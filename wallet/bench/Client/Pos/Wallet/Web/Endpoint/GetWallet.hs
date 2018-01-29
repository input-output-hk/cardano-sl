-- | Run 'GetWallets' client.

module Client.Pos.Wallet.Web.Endpoint.GetWallet
    ( getWalletIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api  (getWallet)
import           Client.Pos.Wallet.Web.Run  (runEndpointClient)
import           Bench.Pos.Wallet.Types     (WalletsConfig (..))

import           Pos.Wallet.Web.ClientTypes (CId (..), CHash (..))

-- | Run 'GetWallet' client. As a result we will get a particular wallet.
getWalletIO :: WalletsConfig -> IO ()
getWalletIO WalletsConfig {..} =
    runEndpointClient (getWallet walletId) >>= \case
        Left problem -> putText $ "Cannot obtain wallet information: " <> problem
        Right wallet -> print wallet     -- :: CWallet
  where
    walletId = CId (CHash "")
    -- walletId = [CId (CHash anId) | Wallet anId _ <- wallets]
