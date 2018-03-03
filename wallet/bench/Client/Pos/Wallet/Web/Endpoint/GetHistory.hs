-- | Function for running a client, for @GetHistory@.

module Client.Pos.Wallet.Web.Endpoint.GetHistory
    ( getHistoryIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api         (getHistory)
import           Client.Pos.Wallet.Web.Run         (runEndpointClient)
import           Client.Pos.Wallet.Web.Analyze     (analyzeResponseIfNeeded, checkResponse)
import           Bench.Pos.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
                                                    Wallet (..), WalletAccount (..),
                                                    WalletsConfig (..), Response,
                                                    ResponseReport (..))
import           Bench.Pos.Wallet.Random           (pickRandomElementFrom)

import           Pos.Wallet.Web.ClientTypes        (Addr, CHash (..), CId (..), CTx (..))

-- | Run 'GetHistory' client. As a result we will get
-- a list of transactions and size of a full history.
getHistoryIO :: CompleteConfig -> IO ()
getHistoryIO conf@CompleteConfig {..} = do
    wallet  <- pickRandomElementFrom $ wallets walletsConfig
    account <- pickRandomElementFrom $ accounts wallet
    address <- pickRandomElementFrom $ addresses account
    let offset = Nothing -- Default value of offset will be used.
        limit  = Nothing -- Default value of limit will be used.
    -- There's an error "Please do not specify both walletId and accountId at the same time"
    -- on the server side, so we should specify just walletId.
    response <- runEndpointClient conf $ getHistory (Just $ walletId wallet)
                                                    Nothing
                                                    (Just address)
                                                    offset
                                                    limit
    analyzeResponseIfNeeded GetHistoryBench conf $ analyze response wallet address

-- | Analyze response with transactions history on
-- particular wallet/account/address.
analyze
    :: Response ([CTx], Word)
    -> Wallet
    -> CId Addr
    -> ResponseReport
analyze response
        (Wallet (CId (CHash walletId)) _)
        (CId (CHash addr)) =
    checkResponse response
                  ("Cannot get history for wallet '" <> walletId <> "', address '" <> addr <> "'")
                  $ \(transactions, _) -> ResponseReport $ show transactions
