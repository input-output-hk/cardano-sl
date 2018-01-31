-- | Function for running a client, for @GetHistory@.

module Client.Pos.Wallet.Web.Endpoint.GetHistory
    ( getHistoryIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api  (getHistory)
import           Client.Pos.Wallet.Web.Run  (runEndpointClient)
import           Bench.Pos.Wallet.Types     (CompleteConfig (..), Wallet (..),
                                             WalletAccount (..), WalletsConfig (..))
import           Bench.Pos.Wallet.Random    (pickRandomElementFrom)

-- | Run 'GetHistory' client. As a result we will get
-- a list of transactions and size of a full history.
getHistoryIO :: CompleteConfig -> IO ()
getHistoryIO conf@CompleteConfig {..} = do
    wallet  <- pickRandomElementFrom $ wallets walletsConfig
    account <- pickRandomElementFrom $ accounts wallet
    address <- pickRandomElementFrom $ addresses account
    let offset = Nothing -- Default value of offset will be used.
        limit  = Nothing -- Default value of limit will be used.
    runEndpointClient conf (getHistory (Just $ walletId wallet)
                                       (Just $ accountId account)
                                       (Just address)
                                       offset
                                       limit) >>= \case
        Left problem ->
            putText $ "Cannot get a history: " <> problem
        Right (Left failure) ->
            putText $ "Server returned an error: " <> pretty failure
        Right (Right ({-transactions-}_, _)) ->
            return () -- :: ([CTx], Word)
