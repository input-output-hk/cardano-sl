module Cardano.Wallet.API.V1.LegacyHandlers.Updates where

import           Universum

import           Cardano.Wallet.API.Response (WalletResponse, single)
import           Cardano.Wallet.API.V1.Types
import qualified Cardano.Wallet.API.V1.Updates as Updates

import           Servant
import           Test.QuickCheck (arbitrary, generate)

handlers :: Server Updates.API
handlers =   nextUpdate
        :<|> applyUpdate

nextUpdate :: Handler (WalletResponse WalletUpdate)
nextUpdate = single <$> (liftIO $ generate arbitrary)

applyUpdate :: Handler (WalletResponse WalletUpdate)
applyUpdate = single <$> (liftIO $ generate arbitrary)
