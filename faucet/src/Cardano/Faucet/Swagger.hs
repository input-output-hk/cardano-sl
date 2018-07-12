{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module Cardano.Faucet.Swagger
    ( FaucetDoc
    , swaggerServer
    , faucetDoc
    ) where

import           Control.Lens ((?~))
import           Data.Proxy
import           Data.Swagger
import qualified Data.Text as T
import           NeatInterpolation
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI (SwaggerSchemaUI)
import           Universum

import           Cardano.Wallet.API.V1.Swagger
import           Pos.Util.CompileInfo (CompileTimeInfo (..), HasCompileInfo,
                     compileInfo)

import           Cardano.Faucet.Endpoints

--------------------------------------------------------------------------------
-- | Swagger UI type
type FaucetDoc = SwaggerSchemaUI "docs" "swagger.json"

faucetDoc :: Proxy FaucetDoc
faucetDoc = Proxy

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
mkSwagger ci walletAPI = toSwagger walletAPI
  & info.title   .~ "Cardano Faucet API"
  & info.version .~ cardanoVersion
  & host ?~ "127.0.0.1:8090"
  & info.description ?~ (faucetMD ci)
  & info.license ?~ ("MIT" & url ?~ URL "https://raw.githubusercontent.com/input-output-hk/cardano-sl/develop/lib/LICENSE")

--------------------------------------------------------------------------------
-- | Server for the swagger UI
swaggerServer :: (HasCompileInfo) => Server FaucetDoc
swaggerServer = swaggerSchemaUIServer (mkSwagger compileInfo faucetServerAPI)
