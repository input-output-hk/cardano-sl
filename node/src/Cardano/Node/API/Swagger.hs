module Cardano.Node.API.Swagger where

import           Universum

import           Control.Lens ((?~))
import           Data.Swagger
import           Servant.Swagger

import           Pos.Web (serveImpl)
import           Pos.Chain.Update (SoftwareVersion)

forkDocServer :: _ -> _ -> Maybe _ -> IO a
forkDocServer ip port tlsParams =
    serveImpl
        (pure (swaggerSchemaUIServer documentationApi))
        ip
        port
        tlsParams
        Nothing
        Nothing

documentationApi
    :: HasSwagger a
    => SoftwareVersion
    -> Proxy a
    -> Swagger
documentationApi curSoftwareVersion prxy = toSwagger prxy
    & info.title   .~ "Cardano Node API"
    & info.version .~ fromString (show curSoftwareVersion)
    & host ?~ "127.0.0.1:8083"
    & info.license ?~ ("MIT" & url ?~ URL "https://raw.githubusercontent.com/input-output-hk/cardano-sl/develop/lib/LICENSE")
--     & paths %~ (POST,   "/api/internal/apply-update")       `setDescription` applyUpdateDescription
--     & paths %~ (POST,   "/api/internal/postpone-update")    `setDescription` postponeUpdateDescription
--     & paths %~ (DELETE, "/api/internal/reset-wallet-state") `setDescription` resetWalletStateDescription
--     & paths %~ (POST,   "/api/v1/transactions/fees")        `setDescription` estimateFeesDescription
--     & paths %~ (GET,    "/api/v1/addresses/{address}")      `setDescription` getAddressDescription
--
