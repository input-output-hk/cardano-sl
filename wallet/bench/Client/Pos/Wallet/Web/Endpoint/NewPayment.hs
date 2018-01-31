-- | Function for running a client, for @NewPayment@.

module Client.Pos.Wallet.Web.Endpoint.NewPayment
    ( newPaymentIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api (newPayment)
import           Client.Pos.Wallet.Web.Run (runEndpointClient)
import           Bench.Pos.Wallet.Types    (CompleteConfig (..), Wallet (..),
                                            WalletAccount (..), WalletsConfig (..))
import           Bench.Pos.Wallet.Random   (pickRandomElementFrom,
                                            pickTwoRandomElementsFrom,
                                            pickRandomValueBetween)

import           Pos.Client.Txp.Util       (InputSelectionPolicy (..))
import           Pos.Core.Types            (mkCoin)

-- | Run 'NewPayment' client. As a result
-- we will get a newly created transaction.
newPaymentIO :: CompleteConfig -> IO ()
newPaymentIO conf@CompleteConfig {..} = do
    -- In real life we send money from one wallet to another.
    -- So we want be sure that 'fromWallet' and 'toWallet' are
    -- different wallets.
    (fromWallet,
     toWallet)  <- pickTwoRandomElementsFrom $ wallets walletsConfig
    fromAccount <- pickRandomElementFrom $ accounts fromWallet
    toAccount   <- pickRandomElementFrom $ accounts toWallet
    toAddress   <- pickRandomElementFrom $ addresses toAccount
    amount      <- pickRandomValueBetween (minAmount, maxAmount)
    let passPhrase = Nothing
        policy     = OptimizeForSecurity
    runEndpointClient conf (newPayment passPhrase
                                       (accountId fromAccount)
                                       toAddress
                                       (mkCoin amount)
                                       (Just policy)) >>= \case
        Left problem -> putText $ "Cannot create new payment: " <> problem
        Right _ -> return () -- :: CTx
  where
    minAmount, maxAmount :: Word64
    minAmount = 1
    maxAmount = 10
