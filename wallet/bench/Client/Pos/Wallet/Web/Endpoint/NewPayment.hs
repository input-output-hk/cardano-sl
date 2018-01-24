-- | Function for running a client, for @NewPayment@.

module Client.Pos.Wallet.Web.Endpoint.NewPayment
    ( newPaymentIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api  (newPayment)
import           Client.Pos.Wallet.Web.Run  (runEndpointClient)

import           Pos.Client.Txp.Util        (InputSelectionPolicy (..))
import           Pos.Core.Types             (mkCoin)
import           Pos.Wallet.Web.ClientTypes (CAccountId (..),
                                             CId (..), CHash (..), CPassPhrase (..))

-- | Run 'NewPayment' client. As a result
-- we will get a newly created transaction.
newPaymentIO :: IO ()
newPaymentIO =
    let passPhrase = CPassPhrase ""
        accountId  = CAccountId ""
        address    = CId (CHash "")
        coin       = mkCoin 100
        policy     = OptimizeForSecurity
    in
    runEndpointClient (newPayment (Just passPhrase)
                                  accountId
                                  address
                                  coin
                                  (Just policy)) >>= \case
        Left problem -> putText $ "Cannot create new payment: " <> problem
        Right newTx  -> print newTx -- :: CTx
