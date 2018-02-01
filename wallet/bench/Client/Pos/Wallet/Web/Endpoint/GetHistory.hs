-- | Function for running a client, for @GetHistory@.

module Client.Pos.Wallet.Web.Endpoint.GetHistory
    ( getHistoryIO
    ) where

import           Universum

import qualified Data.Text.IO                      as TIO

import           Client.Pos.Wallet.Web.Api         (getHistory)
import           Client.Pos.Wallet.Web.Run         (runEndpointClient)
import           Bench.Pos.Wallet.Config.Endpoints (extractEndpointConfigFor)
import           Bench.Pos.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
                                                    EndpointConfig (..), Wallet (..),
                                                    WalletAccount (..), WalletsConfig (..),
                                                    Response, ResponseReport (..))
import           Bench.Pos.Wallet.Random           (pickRandomElementFrom)

import           Pos.Wallet.Web.ClientTypes        (Addr, CId (..), CTx (..))

-- | Run 'GetHistory' client. As a result we will get
-- a list of transactions and size of a full history.
getHistoryIO :: CompleteConfig -> IO ()
getHistoryIO conf@CompleteConfig {..} = do
    wallet  <- pickRandomElementFrom $ wallets walletsConfig
    account <- pickRandomElementFrom $ accounts wallet
    address <- pickRandomElementFrom $ addresses account
    let offset = Nothing -- Default value of offset will be used.
        limit  = Nothing -- Default value of limit will be used.
    response <- runEndpointClient conf $ getHistory (Just $ walletId wallet)
                                                    (Just $ accountId account)
                                                    (Just address)
                                                    offset
                                                    limit
    when needResponseAnalysis $ do
        let ResponseReport report = analyze response wallet account address
        case extractEndpointConfigFor GetHistoryBench conf of
            Nothing -> return ()
            Just (EndpointConfig {..}) -> TIO.appendFile pathToResponseReports report
    return ()

-- | Analyze response with transactions history on
-- particular wallet/account/address.
analyze
    :: Response ([CTx], Word)
    -> Wallet
    -> WalletAccount
    -> CId Addr
    -> ResponseReport
analyze response
        Wallet {..}
        WalletAccount {..}
        _ =
    case response of
        Left problem ->
            ResponseReport $
                "Cannot get history for wallet '" <> "" <> "' : " <> problem
        Right (Left walletError) ->
            ResponseReport $
                "Server returned an error: " <> pretty walletError
        Right (Right (transactions, _)) -> do
            ResponseReport $
                show transactions
