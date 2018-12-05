-- necessary for `ToParamSchema Core.EpochIndex`
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Node.API.Swagger where

import           Universum

import           Control.Lens (at, (?~))
import           Data.Swagger
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI (SwaggerSchemaUI)

import           Pos.Chain.Txp (TxIn, TxOut, TxOutAux)
import           Pos.Chain.Update (SoftwareVersion)
import           Pos.Util.Swagger (swaggerSchemaUIServer)
import           Pos.Web (CConfirmedProposalState, serveDocImpl)
import           Pos.Web.Types (TlsParams)

forkDocServer
    :: HasSwagger a
    => Proxy a
    -> SoftwareVersion
    -> String
    -> Word16
    -> Maybe TlsParams
    -> IO ()
forkDocServer prxy swVersion ip port' tlsParams =
    serveDocImpl
        (pure app)
        ip
        port'
        tlsParams
        Nothing
        Nothing
  where
    app =
        serve
            (Proxy @("docs" :> "v1" :> SwaggerSchemaUI "index" "swagger.json"))
            (swaggerSchemaUIServer (documentationApi swVersion prxy))

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

instance ToParamSchema TxIn where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString

instance ToSchema TxIn where
    declareNamedSchema = pure . paramSchemaToNamedSchema defaultSchemaOptions

instance ToSchema TxOut where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "TxOut") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["coin", "address"]
            & properties .~ (mempty
                & at "coin" ?~ (Inline $ mempty
                    & type_ .~ SwaggerNumber
                    )
                & at "address" ?~ (Inline $ mempty
                    & type_ .~ SwaggerString
                    )
                )

instance ToSchema TxOutAux

instance ToSchema CConfirmedProposalState

