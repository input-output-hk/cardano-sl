-- | Run 'GetWallets' client.

module Client.Pos.Wallet.Web.Endpoint.GetWallet
    ( getWalletIO
    ) where

import           Universum

import qualified Data.Text.IO                      as TIO

import           Client.Pos.Wallet.Web.Api         (getWallet)
import           Client.Pos.Wallet.Web.Run         (runEndpointClient)
import           Bench.Pos.Wallet.Config.Endpoints (extractEndpointConfigFor)
import           Bench.Pos.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
                                                    EndpointConfig (..), Wallet (..),
                                                    WalletsConfig (..),
                                                    Response, ResponseReport (..))
import           Bench.Pos.Wallet.Random           (pickRandomElementFrom)

import           Pos.Wallet.Web.ClientTypes        (CWallet (..), CId (..), Wal)

-- | Run 'GetWallet' client. As a result we will get a particular wallet.
getWalletIO :: CompleteConfig -> IO ()
getWalletIO conf@CompleteConfig {..} = do
    Wallet anId _ <- pickRandomElementFrom $ wallets walletsConfig
    response <- runEndpointClient conf (getWallet anId)
    when needResponseAnalysis $ do
        let ResponseReport report = analyze response anId
        case extractEndpointConfigFor GetWalletBench conf of
            Nothing -> return ()
            Just (EndpointConfig {..}) -> TIO.appendFile pathToResponseReports report
    return ()

-- | Analyze response with wallet information.
analyze
    :: Response CWallet
    -> CId Wal
    -> ResponseReport
analyze response walletId =
    case response of
        Left problem ->
            ResponseReport $
                "Cannot get a wallet: " <> problem
        Right (Left walletError) ->
            ResponseReport $
                "Server returned an error: " <> pretty walletError
        Right (Right wallet) -> do
            ResponseReport $
                show wallet
