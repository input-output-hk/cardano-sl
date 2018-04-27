-- | Run 'GetWallets' client.

module Client.Cardano.Wallet.Web.Endpoint.IsValidAddress
    ( isValidAddressIO
    ) where

import           Universum

import           Client.Cardano.Wallet.Web.Api     (isValidAddress)
import           Client.Cardano.Wallet.Web.Run     (runEndpointClient)
import           Client.Cardano.Wallet.Web.Analyze (analyzeResponseIfNeeded, checkResponse)
import           Bench.Cardano.Wallet.Types        (BenchEndpoint (..), CompleteConfig (..),
                                                    Wallet (..), WalletAccount (..),
                                                    WalletsConfig (..), Response,
                                                    ResponseReport (..))
import           Bench.Cardano.Wallet.Random       (pickRandomElementFrom)

import           Pos.Wallet.Web.ClientTypes        (Addr, CId (..), CHash (..))

-- | Run 'IsValidAddress' client. As a result we will get a True if address if valid.
isValidAddressIO :: CompleteConfig -> IO ()
isValidAddressIO conf@CompleteConfig {..} = do
    wallet  <- pickRandomElementFrom $ wallets walletsConfig
    account <- pickRandomElementFrom $ accounts wallet
    address <- pickRandomElementFrom $ addresses account
    response <- runEndpointClient conf $ isValidAddress address
    analyzeResponseIfNeeded IsValidAddressBench conf $ analyze response address

-- | Analyze response with information about address.
analyze
    :: Response Bool
    -> CId Addr
    -> ResponseReport
analyze response (CId (CHash anAddress)) =
    checkResponse response
                  ("Cannot check an address '" <> anAddress <> "'")
                  $ \isValid -> ResponseReport $ "Address '" <> anAddress <> "' is " <>
                                    if isValid then "valid" else "invalid"
