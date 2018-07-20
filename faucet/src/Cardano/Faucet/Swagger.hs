{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module Cardano.Faucet.Swagger
    ( FaucetDoc
    , swaggerServer
    , faucetDocAPI
    , faucetHandler
    ) where

import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Proxy
import           Data.String (fromString)
import           Data.Swagger
import           Data.Tagged (retag)
import qualified Data.Text as T
import           NeatInterpolation
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI (SwaggerSchemaUI)

import           Cardano.Wallet.API.V1.Swagger
import           Pos.Core.Update (SoftwareVersion)
import           Pos.Update.Configuration (HasUpdateConfiguration,
                     curSoftwareVersion)
import           Pos.Util.CompileInfo (CompileTimeInfo (..), HasCompileInfo,
                     compileInfo)

import           Cardano.Faucet
import           Cardano.Faucet.Types
import           Servant

--------------------------------------------------------------------------------
-- | Swagger UI type
type FaucetDoc = SwaggerSchemaUI "docs" "swagger.json"

-- | Combined swagger UI and 'FaucetAPI'
type FaucetDocAPI = FaucetDoc :<|> FaucetAPI

faucetDoc :: Proxy FaucetDoc
faucetDoc = Proxy

faucetDocAPI :: Proxy FaucetDocAPI
faucetDocAPI = Proxy

--------------------------------------------------------------------------------
-- | Snippet for current cardano version
cardanoVersion :: T.Text
cardanoVersion = "cardano-sl:0"

-- | Header documentation
faucetMD :: CompileTimeInfo -> T.Text
faucetMD CompileTimeInfo{..} = [text|
This is the faucet api documentation

The faucet is a component of the test net that allows users to request ADA from
a wallet to their own address for testing.

Software Version   | Git Revision
-------------------|-------------------
$cardanoVersion           | $ctiGitRevision

 |]

--------------------------------------------------------------------------------
-- | Constructor for the faucet's swagger
mkSwagger :: HasSwagger a
    => CompileTimeInfo
    -> Proxy a
    -> Swagger
mkSwagger compileInfo walletAPI = toSwagger walletAPI
  & info.title   .~ "Cardano Faucet API"
  & info.version .~ cardanoVersion
  & host ?~ "127.0.0.1:8090"
  & info.description ?~ (faucetMD compileInfo)
  & info.license ?~ ("MIT" & url ?~ URL "https://raw.githubusercontent.com/input-output-hk/cardano-sl/develop/lib/LICENSE")

--------------------------------------------------------------------------------
-- | Server for the swagger UI
swaggerServer :: (HasCompileInfo) => Server FaucetDoc
swaggerServer = swaggerSchemaUIServer (mkSwagger compileInfo faucetServerAPI)


faucetHandler :: HasCompileInfo => ServerT FaucetDocAPI M
faucetHandler = (hoistServer faucetDoc liftToM swaggerServer) :<|> faucetServer
