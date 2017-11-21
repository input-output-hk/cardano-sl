module Cardano.Wallet.API.V1.Handlers.Updates where

import           Universum

import           Cardano.Wallet.API.V1.Types
import qualified Cardano.Wallet.API.V1.Updates as Updates

import           Servant
import           Test.QuickCheck (arbitrary, generate)

handlers :: Server Updates.API
handlers =   nextUpdate
        :<|> applyUpdate

nextUpdate :: Handler WalletUpdate
nextUpdate = liftIO $ generate arbitrary

applyUpdate :: Handler WalletUpdate
applyUpdate = liftIO $ generate arbitrary
