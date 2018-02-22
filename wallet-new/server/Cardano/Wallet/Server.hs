module Cardano.Wallet.Server
    ( walletServer
    ) where

import           Servant
import           Universum

import           Cardano.Wallet.API
import qualified Cardano.Wallet.API.Development.Handlers as Dev
import qualified Cardano.Wallet.API.V0 as V0
import qualified Cardano.Wallet.API.V1.Handlers as V1
import           Cardano.Wallet.Kernel
import           Cardano.Wallet.Server.CLI (RunMode (..))


-- | Serve the REST interface to the wallet
--
-- NOTE: Unlike the legacy server, the handlers will not run in a special
-- Cardano monad because they just interfact with the Wallet object.
walletServer :: ActiveWallet
             -> RunMode
             -> Server WalletAPI
walletServer w runMode = externalAPI :<|> internalAPI
  where
    externalAPI = v0Handler :<|> V1.handlers w
    internalAPI = Dev.handlers runMode

-- | Return
--
-- TODO: Not sure if we want to support the V0 API with the new wallet.
-- For now I'm assuming we're not going to.
--
-- TODO: It'd be nicer to not through an exception here, but servant doesn't
-- make this very easy at the moment.
v0Handler :: Server V0.API
v0Handler = error "V0 API no longer supported"
