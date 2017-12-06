module Cardano.Wallet.API.V1.Handlers.Transactions where

import           Universum

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Transactions as Transactions
import           Cardano.Wallet.API.V1.Types

import           Servant
import           Test.QuickCheck (arbitrary, generate)

handlers :: Server Transactions.API
handlers =   newTransaction
        :<|> allTransactions
        :<|> estimateFees

newTransaction :: Payment -> Handler (WalletResponse Transaction)
newTransaction _ = single <$> (liftIO $ generate arbitrary)

allTransactions :: RequestParams -> Handler (WalletResponse [Transaction])
allTransactions _ = single <$> (liftIO $ generate arbitrary)

estimateFees :: Payment -> Handler (WalletResponse EstimatedFees)
estimateFees _ = single <$> (liftIO $ generate arbitrary)
