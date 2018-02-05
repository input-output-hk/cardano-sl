-- | Run 'GetWallets' client.

module Client.Pos.Wallet.Web.Endpoint.IsValidAddress
    ( isValidAddressIO
    ) where

import           Universum

import qualified Data.Text.IO                      as TIO

import           Client.Pos.Wallet.Web.Api         (isValidAddress)
import           Client.Pos.Wallet.Web.Run         (runEndpointClient)
import           Bench.Pos.Wallet.Config.Endpoints (extractEndpointConfigFor)
import           Bench.Pos.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
                                                    EndpointConfig (..), Wallet (..),
                                                    WalletAccount (..), WalletsConfig (..),
                                                    Response, ResponseReport (..))
import           Bench.Pos.Wallet.Random           (pickRandomElementFrom)

import           Pos.Wallet.Web.ClientTypes        (Addr, CId (..), CHash (..))

-- | Run 'IsValidAddress' client. As a result we will get a True if address if valid.
isValidAddressIO :: CompleteConfig -> IO ()
isValidAddressIO conf@CompleteConfig {..} = do
    wallet  <- pickRandomElementFrom $ wallets walletsConfig
    account <- pickRandomElementFrom $ accounts wallet
    address <- pickRandomElementFrom $ addresses account
    response <- runEndpointClient conf $ isValidAddress address
    when needResponseAnalysis $ do
        let ResponseReport report = analyze response address
        case extractEndpointConfigFor IsValidAddressBench conf of
            Nothing -> return ()
            Just (EndpointConfig {..}) -> TIO.appendFile pathToResponseReports $ report <> "\n"
    return ()

-- | Analyze response with information about address.
analyze
    :: Response Bool
    -> CId Addr
    -> ResponseReport
analyze response (CId (CHash anAddress)) =
    case response of
        Left problem ->
            ResponseReport $
                "Cannot check an address '" <> anAddress <> "': " <> problem
        Right (Left walletError) ->
            ResponseReport $
                "Server returned an error for address '" <> anAddress <> "': " <> pretty walletError
        Right (Right isValid) ->
            ResponseReport $ "Address '" <> anAddress <> "' is " <> if isValid then "valid" else "invalid"
