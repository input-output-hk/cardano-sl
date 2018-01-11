-- | .

module Client.Pos.Wallet.Web.Endpoint.NewPayment
    ( newPaymentIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api  (newPayment)
import           Client.Pos.Wallet.Web.Run  (runEndpointClient)

import           Pos.Core.Types             (mkCoin)
import           Pos.Wallet.Web.ClientTypes (CAccountId (..),
                                             CId (..), CHash (..)) -- , CPassPhrase, CTx)

-- |
newPaymentIO :: IO ()
newPaymentIO =
    let passPhrase = Nothing            -- :: Maybe CPassPhrase
        accountId  = CAccountId ""      -- :: CAccountId
        address    = CId (CHash "")     -- :: CId Addr
        coin       = mkCoin (100 :: Word64) -- :: Coin
        policy     = Nothing            -- :: Maybe InputSelectionPolicy
    in
    runEndpointClient (newPayment passPhrase accountId address coin policy) >>= \case
        Left problem -> putStrLn $ "Cannot create new payment: " <> problem
        Right newTx  -> print newTx     -- :: CTx
