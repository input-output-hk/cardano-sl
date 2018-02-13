-- | Function for running a client, for @NewPayment@.

module Client.Pos.Wallet.Web.Endpoint.NewPayment
    ( newPaymentIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api         (newPayment)
import           Client.Pos.Wallet.Web.Run         (runEndpointClient)
import           Client.Pos.Wallet.Web.Analyze     (analyzeResponseIfNeeded, checkResponse)
import           Bench.Pos.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
                                                    Wallet (..), WalletAccount (..),
                                                    WalletsConfig (..), Response,
                                                    ResponseReport (..))
import           Bench.Pos.Wallet.Random           (pickRandomElementFrom,
                                                    -- pickTwoRandomElementsFrom,
                                                    pickRandomValueBetween)
import           Pos.Wallet.Web.ClientTypes        (Addr, CId (..), CTx (..))
import           Pos.Client.Txp.Util               (InputSelectionPolicy (..))
import           Pos.Core.Types                    (mkCoin)

-- | Run 'NewPayment' client. As a result
-- we will get a newly created transaction.
newPaymentIO :: CompleteConfig -> IO ()
newPaymentIO conf@CompleteConfig {..} = do
    let passPhrase = Nothing
        policy     = OptimizeForSecurity
        minAmount  = 300000 :: Word64 -- In Lovelaces
        maxAmount  = 900000 :: Word64 -- In Lovelaces
    -- Wallets 'fromWallet' and 'toWallet' can be the same one,
    -- it's valid situation.
    -- (fromWallet,
    --  toWallet) <- pickTwoRandomElementsFrom $ wallets walletsConfig
    fromWallet  <- pickRandomElementFrom $ wallets walletsConfig
    toWallet    <- pickRandomElementFrom $ wallets walletsConfig
    fromAccount <- pickRandomElementFrom $ accounts fromWallet
    toAccount   <- pickRandomElementFrom $ accounts toWallet
    toAddress   <- pickRandomElementFrom $ addresses toAccount
    amount      <- pickRandomValueBetween (minAmount, maxAmount)
    response <- runEndpointClient conf $ newPayment passPhrase
                                                    (accountId fromAccount)
                                                    toAddress
                                                    (mkCoin amount)
                                                    (Just policy)
    analyzeResponseIfNeeded NewPaymentBench conf $ analyze response
                                                           fromAccount
                                                           toAddress
                                                           amount

-- | Analyze response with new transaction.
analyze
    :: Response CTx
    -> WalletAccount
    -> CId Addr
    -> Word64
    -> ResponseReport
analyze response WalletAccount {..} _ _ =
    checkResponse response
                  "Cannot create new payment"
                  $ \newTransaction -> ResponseReport $ show newTransaction
