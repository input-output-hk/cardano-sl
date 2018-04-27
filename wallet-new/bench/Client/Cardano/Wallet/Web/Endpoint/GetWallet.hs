-- | Run 'GetWallets' client.

module Client.Cardano.Wallet.Web.Endpoint.GetWallet
    ( getWalletIO
    ) where

import           Universum

import           Client.Cardano.Wallet.Web.Api     (getWallet)
import           Client.Cardano.Wallet.Web.Run     (runEndpointClient)
import           Client.Cardano.Wallet.Web.Analyze (analyzeResponseIfNeeded, checkResponse)
import           Bench.Cardano.Wallet.Types        (BenchEndpoint (..), CompleteConfig (..),
                                                    Wallet (..), WalletsConfig (..),
                                                    Response, ResponseReport (..))
import           Bench.Cardano.Wallet.Random       (pickRandomElementFrom)

import           Pos.Wallet.Web.ClientTypes        (CWallet (..), CId (..), CHash (..), Wal)

-- | Run 'GetWallet' client. As a result we will get a particular wallet.
getWalletIO :: CompleteConfig -> IO ()
getWalletIO conf@CompleteConfig {..} = do
    Wallet anId _ <- pickRandomElementFrom $ wallets walletsConfig
    response <- runEndpointClient conf $ getWallet anId
    analyzeResponseIfNeeded GetWalletBench conf $ analyze response anId

-- | Analyze response with wallet information.
analyze
    :: Response CWallet
    -> CId Wal
    -> ResponseReport
analyze response (CId (CHash anId)) =
    checkResponse response
                  ("Cannot get a wallet '" <> anId <> "'")
                  $ \wallet -> ResponseReport $ show wallet
