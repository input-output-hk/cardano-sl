{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.LegacyServer where

import           Universum

import           Cardano.Wallet.API
import           Cardano.Wallet.API.V1.Migration (HasCompileInfo,
                     HasConfigurations)

import qualified Cardano.Wallet.API.Development.LegacyHandlers as Dev
import qualified Cardano.Wallet.API.V0.Handlers as V0
import qualified Cardano.Wallet.API.V1.LegacyHandlers as V1
import           Cardano.Wallet.API.V1.Swagger (swaggerSchemaUIServer)
import qualified Cardano.Wallet.API.V1.Swagger as Swagger
import           Cardano.Wallet.Server.CLI (RunMode (..))

import           Ntp.Client (NtpStatus)
import           Pos.Core (ProtocolConstants)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Update.Configuration (curSoftwareVersion)
import           Pos.Util.CompileInfo (compileInfo)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Servant

-- | This function has the tricky task of plumbing different versions of the API,
-- with potentially different monadic stacks into a uniform @Server@ we can use
-- with Servant.
walletServer :: (HasConfigurations, HasCompileInfo)
             => (forall a. WalletWebMode a -> Handler a)
             -> ProtocolMagic
             -> ProtocolConstants
             -> Diffusion WalletWebMode
             -> TVar NtpStatus
             -> Server WalletAPI
walletServer natV0 pm pc diffusion ntpStatus = v0Handler :<|> v1Handler
  where
    v0Handler    = V0.handlers natV0 pm pc diffusion ntpStatus
    v1Handler    = V1.handlers natV0 pm pc diffusion ntpStatus


walletDevServer
    :: (HasConfigurations, HasCompileInfo)
    => (forall a. WalletWebMode a -> Handler a)
    -> ProtocolMagic
    -> ProtocolConstants
    -> Diffusion WalletWebMode
    -> TVar NtpStatus
    -> RunMode
    -> Server WalletDevAPI
walletDevServer natV0 pm pc diffusion ntpStatus runMode = devHandler :<|> walletHandler
  where
    devHandler    = Dev.handlers natV0 runMode
    walletHandler = walletServer natV0 pm pc diffusion ntpStatus


walletDocServer
    :: (HasConfigurations, HasCompileInfo)
    => Server WalletDocAPI
walletDocServer = v0DocHandler :<|> v1DocHandler
  where
    v0DocHandler = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) v0API Swagger.highLevelShortDescription)
    v1DocHandler = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) v1API Swagger.highLevelDescription)


walletDevDocServer
    :: (HasConfigurations, HasCompileInfo)
    => Server WalletDevDocAPI
walletDevDocServer = devDocHandler :<|> walletDocServer
  where
    devDocHandler = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) devAPI Swagger.highLevelShortDescription)
