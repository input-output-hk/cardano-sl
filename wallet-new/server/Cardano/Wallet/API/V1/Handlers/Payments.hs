module Cardano.Wallet.API.V1.Handlers.Payments where

import           Universum

import qualified Cardano.Wallet.API.V1.Payments as Payments
import           Cardano.Wallet.API.V1.Types

import           Servant
import           Test.QuickCheck                (arbitrary, generate)

handlers :: Server Payments.API
handlers =   newPayment
        :<|> estimateFees

newPayment :: Payment -> Handler Transaction
newPayment _ = liftIO $ generate arbitrary

estimateFees :: Payment -> Handler EstimatedFees
estimateFees _ = liftIO $ generate arbitrary
