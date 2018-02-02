-- | Function for running a client, for @NewAddress@.

module Client.Pos.Wallet.Web.Endpoint.NewAddress
    ( newAddressIO
    ) where

import           Universum

import qualified Data.Text.IO                      as TIO

import           Client.Pos.Wallet.Web.Api         (newAddress)
import           Client.Pos.Wallet.Web.Run         (runEndpointClient)
import           Bench.Pos.Wallet.Config.Endpoints (extractEndpointConfigFor)
import           Bench.Pos.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
                                                    EndpointConfig (..), Wallet (..),
                                                    WalletAccount (..), WalletsConfig (..),
                                                    Response, ResponseReport (..))
import           Bench.Pos.Wallet.Random           (pickRandomElementFrom)
import           Pos.Wallet.Web.ClientTypes        (CAccountId (..), CAddress (..)) 

-- | Run 'NewAddress' client. As a result we will get
-- a newly created address in the given account.
newAddressIO :: CompleteConfig -> IO ()
newAddressIO conf@CompleteConfig {..} = do
    let passPhrase = Nothing
    wallet  <- pickRandomElementFrom $ wallets walletsConfig
    account <- pickRandomElementFrom $ accounts wallet
    response <- runEndpointClient conf $ newAddress passPhrase (accountId account)
    when needResponseAnalysis $ do
        let ResponseReport report = analyze response account
        case extractEndpointConfigFor NewAddressBench conf of
            Nothing -> return ()
            Just (EndpointConfig {..}) -> TIO.appendFile pathToResponseReports report
    return ()

-- | Analyze response with new address.
analyze
    :: Response CAddress
    -> WalletAccount
    -> ResponseReport
analyze response (WalletAccount (CAccountId accId) _) =
    case response of
        Left problem ->
            ResponseReport $
                "Cannot create new address in account '" <> accId <> "': " <> problem
        Right (Left walletError) ->
            ResponseReport $
                "Server returned an error: " <> pretty walletError
        Right (Right createdAddress) -> do
            ResponseReport $
                show createdAddress
