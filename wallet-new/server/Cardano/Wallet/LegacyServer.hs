{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.LegacyServer where

import           Cardano.Wallet.API
import           Cardano.Wallet.API.V1.Migration as Migration

import qualified Cardano.Wallet.API.V0.Handlers       as V0
import qualified Cardano.Wallet.API.V1.LegacyHandlers as V1

import           Pos.Diffusion.Types (Diffusion (..))
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Servant

-- | This function has the tricky task of plumbing different versions of the API,
-- with potentially different monadic stacks into a uniform @Server@ we can use
-- with Servant.
walletServer :: ( Migration.HasConfigurations
                , Migration.HasCompileInfo
                )
             => (forall a. WalletWebMode a -> Handler a)
             -> Diffusion WalletWebMode
             -> Server WalletAPI
walletServer natV0 diffusion =
         V0.handlers natV0 diffusion
    :<|> V1.handlers natV0 diffusion
