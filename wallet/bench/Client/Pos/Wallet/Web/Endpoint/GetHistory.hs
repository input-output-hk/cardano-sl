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
    when needResponseAnalysis $ do
        let ResponseReport report = analyze response wallet address
        case extractEndpointConfigFor GetHistoryBench conf of
            Nothing -> return ()
            Just (EndpointConfig {..}) -> TIO.appendFile pathToResponseReports report
    return ()

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
    case response of
        Left problem ->
            ResponseReport $
                "Cannot get history for wallet '" <> walletId <> "', address '" <> addr <> "': " <> problem
        Right (Left walletError) ->
            ResponseReport $
                "Server returned an error, for wallet '" <> walletId <> "', address '" <> addr <> "': " <> pretty walletError
        Right (Right (transactions, _)) -> do
            ResponseReport $
                show transactions
