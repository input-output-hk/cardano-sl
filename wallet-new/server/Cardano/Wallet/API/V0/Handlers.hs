module Cardano.Wallet.API.V0.Handlers where

import           Universum

import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V0    as V0

import           Servant

handlers :: Server V0.API
handlers = apiVersion

apiVersion :: Handler WalletVersion
apiVersion = return (WalletVersion V0 "6f1131adca2f0bc6d24c9181cabd2b9e0704fd79")
