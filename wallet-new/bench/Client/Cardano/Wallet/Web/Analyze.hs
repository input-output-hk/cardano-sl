-- | Aux functions for analyzing responses.

module Client.Cardano.Wallet.Web.Analyze
    ( analyzeResponseIfNeeded
    , checkResponse
    ) where

import           Universum

import           Bench.Cardano.Wallet.Config.Endpoints (extractEndpointConfigFor)
import           Bench.Cardano.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
                                                        EndpointConfig (..), Response,
                                                        ResponseReport (..))

-- | Sometimes we don't want to analyze response from the server.
analyzeResponseIfNeeded
    :: BenchEndpoint
    -> CompleteConfig
    -> ResponseReport
    -> IO ()
analyzeResponseIfNeeded endpoint
                        conf@CompleteConfig {..}
                        (ResponseReport report) = when needResponseAnalysis $
    case extractEndpointConfigFor endpoint conf of
        Nothing                    -> return ()
        Just (EndpointConfig {..}) -> appendFile pathToResponseReports $ report <> "\n"

-- | Check if response is successful or not.
checkResponse
    :: Response dataWeNeed
    -> Text
    -> (dataWeNeed -> ResponseReport)
    -> ResponseReport
checkResponse response messageIfFail actionIfSuccess =
    case response of
        Left problem             -> ResponseReport $ messageIfFail <> ": " <> problem
        Right (Left walletError) -> ResponseReport $ "Server returned an error: " <> pretty walletError
        Right (Right dataWeNeed) -> actionIfSuccess dataWeNeed
