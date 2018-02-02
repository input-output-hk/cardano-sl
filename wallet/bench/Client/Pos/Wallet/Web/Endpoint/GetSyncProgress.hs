-- | Run 'GetSyncProgress' client.

module Client.Pos.Wallet.Web.Endpoint.GetSyncProgress
    ( getSyncProgressIO
    ) where

import           Universum

import qualified Data.Text.IO                      as TIO

import           Client.Pos.Wallet.Web.Api         (getSyncProgress)
import           Client.Pos.Wallet.Web.Run         (runEndpointClient)
import           Bench.Pos.Wallet.Config.Endpoints (extractEndpointConfigFor)
import           Bench.Pos.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
                                                    EndpointConfig (..),
                                                    Response, ResponseReport (..))

import           Pos.Wallet.Web.ClientTypes        (SyncProgress (..))

-- | Run 'GetSyncProgress' client. As a result we will get a list of wallets.
getSyncProgressIO :: CompleteConfig -> IO ()
getSyncProgressIO conf@CompleteConfig {..} = do
    response <- runEndpointClient conf getSyncProgress
    when needResponseAnalysis $ do
        let ResponseReport report = analyze response
        case extractEndpointConfigFor GetSyncProgressBench conf of
            Nothing -> return ()
            Just (EndpointConfig {..}) -> TIO.appendFile pathToResponseReports report
    return ()

-- | Analyze response with sync progress.
analyze
    :: Response SyncProgress
    -> ResponseReport
analyze response =
    case response of
        Left problem ->
            ResponseReport $
                "Cannot get sync progress: " <> problem
        Right (Left walletError) ->
            ResponseReport $
                "Server returned an error: " <> pretty walletError
        Right (Right syncProgress) -> do
            ResponseReport $
                show syncProgress
