-- | Run 'GetWallets' client.

module Client.Pos.Wallet.Web.Endpoint.IsValidAddress
    ( isValidAddressIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api         (isValidAddress)
import           Client.Pos.Wallet.Web.Run         (runEndpointClient)
import           Client.Pos.Wallet.Web.Analyze     (analyzeResponseIfNeeded, checkResponse)
import           Bench.Pos.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
                                                    Wallet (..), WalletAccount (..),
                                                    WalletsConfig (..), Response,
                                                    ResponseReport (..))
import           Bench.Pos.Wallet.Random           (pickRandomElementFrom)

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
