{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This program builds Swagger specification for Explorer web API and converts it to JSON.
-- We run this program during CI build.
-- Produced JSON will be used to create online
-- version of wallet web API description at cardanodocs.com website
-- (please see 'update_explorer_web_api_docs.sh' for technical details).

module Main
    ( main
    ) where

import           Universum

import           Control.Lens (mapped, (?~))
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Fixed (Fixed (..), Micro)
import           Data.Swagger (Swagger, ToParamSchema (..), ToSchema (..), declareNamedSchema,
                               defaultSchemaOptions, description, genericDeclareNamedSchema, host,
                               info, name, title, version)
import           Data.Typeable (Typeable, typeRep)
import           Data.Version (showVersion)
import           Options.Applicative (execParser, footer, fullDesc, header, help, helper,
                                      infoOption, long, progDesc)
import qualified Options.Applicative as Opt
import           Servant ((:>))
import           Servant.Multipart (MultipartForm)
import           Servant.Swagger (HasSwagger (toSwagger))

import qualified Paths_cardano_sl_explorer as CSLE
import qualified Pos.Explorer.Web.Api as A
import qualified Pos.Explorer.Web.ClientTypes as C
import           Pos.Explorer.Web.Error (ExplorerError)


main :: IO ()
main = do
    showProgramInfoIfRequired jsonFile
    BSL8.writeFile jsonFile $ encode swaggerSpecForExplorerApi
    putStrLn $ "Done. See " <> jsonFile <> "."
  where
    jsonFile = "explorer-web-api-swagger.json"

    -- | Showing info for the program.
    showProgramInfoIfRequired :: FilePath -> IO ()
    showProgramInfoIfRequired generatedJSON = void $ execParser programInfo
      where
        programInfo = Opt.info (helper <*> versionOption) $
            fullDesc <> progDesc "Generate Swagger specification for Explorer web API."
                     <> header   "Cardano SL Explorer web API docs generator."
                     <> footer   ("This program runs during 'cardano-sl' building on CI. " <>
                                  "Generated file '" <> generatedJSON <> "' will be used to produce HTML documentation. " <>
                                  "This documentation will be published at cardanodocs.com using 'update-explorer-web-api-docs.sh'.")

        versionOption = infoOption
            ("cardano-swagger-" <> showVersion CSLE.version)
            (long "version" <> help "Show version.")

instance HasSwagger api => HasSwagger (MultipartForm a :> api) where
    toSwagger Proxy = toSwagger $ Proxy @api

-- | Instances we need to build Swagger-specification for 'explorerApi':
-- 'ToParamSchema' - for types in parameters ('Capture', etc.),
-- 'ToSchema' - for types in bodies.
instance ToSchema      C.CHash
instance ToParamSchema C.CHash
instance ToSchema      C.CTxId
instance ToParamSchema C.CTxId
instance ToSchema      C.CAddress
instance ToParamSchema C.CAddress
instance ToParamSchema C.EpochIndex
instance ToSchema      C.CTxSummary
instance ToSchema      C.CTxEntry
instance ToSchema      C.CTxBrief
instance ToSchema      C.CBlockSummary
instance ToSchema      C.CBlockEntry
instance ToSchema      C.CAddressType
instance ToSchema      C.CAddressSummary
instance ToSchema      C.CCoin
instance ToSchema      C.CAda
instance ToSchema      C.CNetworkAddress
instance ToSchema      C.CGenesisSummary
instance ToSchema      C.CGenesisAddressInfo
instance ToSchema      C.Byte
instance ToSchema      ExplorerError
instance ToParamSchema C.CAddressesFilter

deriving instance Generic Micro

-- | Instance for Either-based types (types we return as 'Right') in responses.
-- Due 'typeOf' these types must be 'Typeable'.
-- We need this instance for correct Swagger-specification.
instance {-# OVERLAPPING #-} (Typeable a, ToSchema a) => ToSchema (Either ExplorerError a) where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped . name ?~ show (typeRep (Proxy @(Either ExplorerError a)))

-- | Build Swagger-specification from 'explorerApi'.
swaggerSpecForExplorerApi :: Swagger
swaggerSpecForExplorerApi = toSwagger A.explorerApi
    & info . title       .~ "Cardano SL Explorer Web API"
    & info . version     .~ toText (showVersion CSLE.version)
    & info . description ?~ "This is an API for Cardano SL Explorer."
    & host               ?~ "cardanoexplorer.com"
