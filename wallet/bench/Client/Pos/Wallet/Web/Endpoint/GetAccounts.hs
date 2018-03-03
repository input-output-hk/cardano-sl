-- | Run 'GetAccounts' client.

module Client.Pos.Wallet.Web.Endpoint.GetAccounts
    ( getAccountsIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api         (getAccounts)
import           Client.Pos.Wallet.Web.Run         (runEndpointClient)
import           Client.Pos.Wallet.Web.Analyze     (analyzeResponseIfNeeded, checkResponse)
import           Bench.Pos.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
                                                    Response, ResponseReport (..))

import           Pos.Wallet.Web.ClientTypes        (CAccount (..))

-- | Run 'GetAccounts' client. As a result we will get a list of accounts.
getAccountsIO :: CompleteConfig -> IO ()
getAccountsIO conf@CompleteConfig {..} = do
    let walletId = Nothing -- In this case all accounts in all wallets will be selected.
    -- TODO: Probably we have to select one random wallet and get accounts from it only.
    response <- runEndpointClient conf $ getAccounts walletId
    analyzeResponseIfNeeded GetAccountsBench conf $ analyze response

-- | Analyze response with list of accounts.
analyze
    :: Response [CAccount]
    -> ResponseReport
analyze response =
    checkResponse response
                  "Cannot get list of accounts"
                  $ \accounts -> ResponseReport $ show accounts
