-- | Function for running a client, for @NewPayment@.

module Client.Pos.Wallet.Web.Endpoint.NewPayment
    ( newPaymentIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api (newPayment)
import           Client.Pos.Wallet.Web.Run (runEndpointClient)
import           Bench.Pos.Wallet.Types    (CompleteConfig (..), WalletAccount (..))
import           Bench.Pos.Wallet.Random   (pickRandomWalletFrom,
                                            pickRandomAccountIn,
                                            pickRandomAddressIn,
                                            pickRandomValueBetween)

import           Pos.Client.Txp.Util       (InputSelectionPolicy (..))
import           Pos.Core.Types            (mkCoin)

-- | Run 'NewPayment' client. As a result
-- we will get a newly created transaction.
newPaymentIO :: CompleteConfig -> IO ()
newPaymentIO conf@CompleteConfig {..} = do
    -- TODO: Fix random values, it should be different wallets (?).
    fromWallet  <- pickRandomWalletFrom walletsConfig
    fromAccount <- pickRandomAccountIn fromWallet

    toWallet    <- pickRandomWalletFrom walletsConfig
    toAccount   <- pickRandomAccountIn toWallet
    toAddress   <- pickRandomAddressIn toAccount

    amount      <- pickRandomValueBetween (minAmount, maxAmount)
    let passPhrase    = Nothing
        fromAccountId = accountId fromAccount
        policy        = OptimizeForSecurity
    runEndpointClient conf (newPayment passPhrase
                                       fromAccountId
                                       toAddress
                                       (mkCoin amount)
                                       (Just policy)) >>= \case
        Left problem -> putText $ "Cannot create new payment: " <> problem
        Right _ -> return () -- :: CTx
  where
    minAmount, maxAmount :: Word64
    minAmount = 1
    maxAmount = 100
