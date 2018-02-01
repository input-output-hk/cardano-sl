-- | Function for running a client, for @NewPayment@.

module Client.Pos.Wallet.Web.Endpoint.NewPayment
    ( newPaymentIO
    ) where

import           Universum

import qualified Data.Text.IO                      as TIO

import           Client.Pos.Wallet.Web.Api         (newPayment)
import           Client.Pos.Wallet.Web.Run         (runEndpointClient)
import           Bench.Pos.Wallet.Config.Endpoints (extractEndpointConfigFor)
import           Bench.Pos.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
                                                    EndpointConfig (..), Wallet (..),
                                                    WalletAccount (..), WalletsConfig (..),
                                                    Response, ResponseReport (..))
import           Bench.Pos.Wallet.Random           (pickRandomElementFrom,
                                                    pickTwoRandomElementsFrom,
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
        minAmount  = 1000000  :: Word64 -- In Lovelaces
        maxAmount  = 10000000 :: Word64 -- In Lovelaces
    -- In real life we send money from one wallet to another.
    -- So we want be sure that 'fromWallet' and 'toWallet' are
    -- different wallets.
    (fromWallet,
     toWallet)  <- pickTwoRandomElementsFrom $ wallets walletsConfig
    fromAccount <- pickRandomElementFrom $ accounts fromWallet
    toAccount   <- pickRandomElementFrom $ accounts toWallet
    toAddress   <- pickRandomElementFrom $ addresses toAccount
    amount      <- pickRandomValueBetween (minAmount, maxAmount)
    response <- runEndpointClient conf $ newPayment passPhrase
                                                    (accountId fromAccount)
                                                    toAddress
                                                    (mkCoin amount)
                                                    (Just policy)
    when needResponseAnalysis $ do
        let ResponseReport report = analyze response fromAccount toAddress amount
        case extractEndpointConfigFor NewPaymentBench conf of
            Nothing -> return ()
            Just (EndpointConfig {..}) -> TIO.appendFile pathToResponseReports report
    return ()

-- | Analyze response with new transaction.
analyze
    :: Response CTx
    -> WalletAccount
    -> CId Addr
    -> Word64
    -> ResponseReport
analyze response
        WalletAccount {..}
        _
        _ =
    case response of
        Left problem ->
            ResponseReport $
                "Cannot create new payment: " <> problem
        Right (Left walletError) ->
            ResponseReport $
                "Server returned an error: " <> pretty walletError
        Right (Right newTransaction) -> do
            ResponseReport $
                show newTransaction
