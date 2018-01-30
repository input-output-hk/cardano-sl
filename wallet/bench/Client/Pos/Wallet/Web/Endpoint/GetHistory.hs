-- | Function for running a client, for @GetHistory@.

module Client.Pos.Wallet.Web.Endpoint.GetHistory
    ( getHistoryIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api  (getHistory)
import           Client.Pos.Wallet.Web.Run  (runEndpointClient)
import           Bench.Pos.Wallet.Types     (CompleteConfig (..))

import           Pos.Wallet.Web.ClientTypes (CAccountId (..), CHash (..), CId (..),
                                             ScrollLimit (..), ScrollOffset (..))

-- | Run 'GetHistory' client. As a result we will get
-- a list of transactions and size of a full history.
getHistoryIO :: CompleteConfig -> IO ()
getHistoryIO conf@CompleteConfig {..} =
    let wallet    = CId (CHash "")
        accountId = CAccountId ""
        address   = CId (CHash "")
        offset    = ScrollOffset 1
        limit     = ScrollLimit 2
    in
    runEndpointClient conf (getHistory (Just wallet)
                                       (Just accountId)
                                       (Just address)
                                       (Just offset)
                                       (Just limit)) >>= \case
        Left problem ->
            putText $ "Cannot get a history: " <> problem
        Right (Left failure) ->
            putText $ "Server returned an error: " <> pretty failure
        Right (Right (transactions, _)) ->
            print transactions -- :: ([CTx], Word)
