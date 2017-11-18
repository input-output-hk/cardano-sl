{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.Server where

import           Cardano.Wallet.API
import           Cardano.Wallet.API.V1.Migration as Migration

import qualified Cardano.Wallet.API.V0.Handlers as V0
import qualified Cardano.Wallet.API.V1.Handlers as V1

import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Servant

-- | This function has the tricky task of plumbing different versions of the API,
-- with potentially different monadic stacks into a uniform @Server@ we can use
-- with Servant.
-- You see below two calls to 'V0.handlers', and this is because we support (at the
-- time of writing) two ways of accessing the V0 API, which are either by hitting
-- `/api/..` or by hitting `/api/v0/...`.

walletServer :: ( Migration.HasConfigurations
                , Migration.HasCompileInfo
                )
             => (forall a. WalletWebMode a -> Handler a)
             -> Server WalletAPI
walletServer natV0 = V0.handlers natV0
                :<|> V0.handlers natV0
                :<|> V1.handlers natV0
