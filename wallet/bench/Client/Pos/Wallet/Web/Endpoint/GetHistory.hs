-- | TODO: real values for the client function.

module Client.Pos.Wallet.Web.Endpoint.GetHistory
    ( getHistoryIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api  (getHistory)
import           Client.Pos.Wallet.Web.Run  (runEndpointClient)

-- import           Pos.Wallet.Web.ClientTypes (CAccountId (..),
--                                              CId (..), CHash (..)) -- , CPassPhrase, CTx)

-- | Run 'GetHistory' client. As a result we get
-- a list of transactions and size of full history.
getHistoryIO :: IO ()
getHistoryIO =
    let wallet    = Nothing -- Maybe (CId Wal)
        accountId = Nothing -- Maybe CAccountId
        address   = Nothing -- Maybe (CId Addr)
        offset    = Nothing -- Maybe ScrollOffset
        limit     = Nothing -- Maybe ScrollLimit
    in
    runEndpointClient (getHistory wallet accountId address offset limit) >>= \case
        Left problem -> putText $ "Cannot get a history: " <> problem
        Right (transactions, _) -> print transactions -- :: ([CTx], Word)
