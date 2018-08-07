{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.LegacyServer where

import           Universum

import           Servant

import           Cardano.Wallet.API
import           Cardano.Wallet.API.V1.Migration (HasCompileInfo,
                     HasConfigurations)
import           Cardano.Wallet.API.V1.Swagger (swaggerSchemaUIServer)
import           Cardano.Wallet.Server.CLI (RunMode (..))
import           Ntp.Client (NtpStatus)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Chain.Update (curSoftwareVersion)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Util.CompileInfo (compileInfo)
import           Pos.Util.Trace.Named (TraceNamed)
import           Pos.Wallet.Web.Mode (WalletWebMode)

import qualified Cardano.Wallet.API.Internal.Handlers as Internal
import qualified Cardano.Wallet.API.V0.Handlers as V0
import qualified Cardano.Wallet.API.V1.LegacyHandlers as V1
import qualified Cardano.Wallet.API.V1.Swagger as Swagger


-- | This function has the tricky task of plumbing different versions of the API,
-- with potentially different monadic stacks into a uniform @Server@ we can use
-- with Servant.
walletServer :: ( HasConfigurations, HasCompileInfo )
             => (forall a. WalletWebMode a -> Handler a)
             -> TraceNamed WalletWebMode
             -> ProtocolMagic
             -> TxpConfiguration
             -> Diffusion WalletWebMode
             -> TVar NtpStatus
             -> RunMode
             -> Server WalletAPI
walletServer natV0 logTrace pm txpConfig diffusion ntpStatus runMode =
         v0Handler
    :<|> v0Handler
    :<|> v1Handler
    :<|> internalHandler
  where
    v0Handler       = V0.handlers logTrace natV0 pm txpConfig diffusion ntpStatus
    v1Handler       = V1.handlers logTrace natV0 pm txpConfig diffusion ntpStatus
    internalHandler = Internal.handlers logTrace natV0 runMode


walletDocServer
    :: (HasConfigurations, HasCompileInfo)
    => Server WalletDocAPI
walletDocServer = v0DocHandler :<|> v1DocHandler
  where
    v1API'       = Proxy :: Proxy (V1API :<|> InternalAPI)
    v0DocHandler = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) v0API Swagger.highLevelShortDescription)
    v1DocHandler = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) v1API' Swagger.highLevelDescription)
