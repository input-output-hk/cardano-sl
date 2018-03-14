-- | Function for running a client, for @NewAddress@.

module Client.Cardano.Wallet.Web.Endpoint.NewAddress
    ( newAddressIO
    ) where

import           Universum

import           Client.Cardano.Wallet.Web.Api     (newAddress)
import           Client.Cardano.Wallet.Web.Run     (runEndpointClient)
import           Client.Cardano.Wallet.Web.Analyze (analyzeResponseIfNeeded, checkResponse)
import           Bench.Cardano.Wallet.Types        (BenchEndpoint (..), CompleteConfig (..),
                                                    Wallet (..), WalletAccount (..),
                                                    WalletsConfig (..), Response,
                                                    ResponseReport (..))
import           Bench.Cardano.Wallet.Random       (pickRandomElementFrom)
import           Pos.Wallet.Web.ClientTypes        (CAccountId (..), CAddress (..))

-- | Run 'NewAddress' client. As a result we will get
-- a newly created address in the given account.
newAddressIO :: CompleteConfig -> IO ()
newAddressIO conf@CompleteConfig {..} = do
    let passPhrase = Nothing
    wallet  <- pickRandomElementFrom $ wallets walletsConfig
    account <- pickRandomElementFrom $ accounts wallet
    response <- runEndpointClient conf $ newAddress passPhrase (accountId account)
    analyzeResponseIfNeeded NewAddressBench conf $ analyze response account

-- | Analyze response with new address.
analyze
    :: Response CAddress
    -> WalletAccount
    -> ResponseReport
analyze response (WalletAccount (CAccountId accId) _) =
    checkResponse response
                  ("Cannot create new address in account '" <> accId <> "'")
                  $ \createdAddress -> ResponseReport $ show createdAddress
