module Cardano.Wallet.Server (walletServer) where

import Universum
import Servant

import Cardano.Wallet.API
import Cardano.Wallet.Kernel
import qualified Cardano.Wallet.API.V0          as V0
import qualified Cardano.Wallet.API.V1.Handlers as V1

-- | Serve the REST interface to the wallet
--
-- NOTE: Unlike the legacy server, the handlers will not run in a special
-- Cardano monad because they just interfact with the Wallet object.
walletServer :: ActiveWallet -> Server WalletAPI
walletServer w = v0Handler :<|> V1.handlers w

-- | Return
--
-- TODO: Not sure if we want to support the V0 API with the new wallet.
-- For now I'm assuming we're not going to.
--
-- TODO: It'd be nicer to not through an exception here, but servant doesn't
-- make this very easy at the moment.
v0Handler :: Server V0.API
v0Handler = error "V0 API no longer supported"
