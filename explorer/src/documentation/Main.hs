{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | This program builds Swagger specification for Explorer web API and converts it to JSON.
-- We run this program during CI build.
-- Produced JSON will be used to create online
-- version of wallet web API description at cardanodocs.com website
-- (please see 'update_explorer_web_api_docs.sh' for technical details).

module Main where

import           Universum

import           Control.Lens                 (mapped, (?~))
import           Data.Aeson                   (encode)
import qualified Data.ByteString.Lazy.Char8   as BSL8
import           Data.Swagger                 (Operation, Swagger,
                                               ToParamSchema (..),
                                               ToSchema (..), declareNamedSchema,
                                               defaultSchemaOptions,
                                               description,
                                               genericDeclareNamedSchema, host, info, name,
                                               title, version)
import           Data.Typeable                (Typeable, typeRep)
import           Data.Version                 (showVersion)
import           Servant                      ((:>))
import           Servant.Multipart            (MultipartForm)
import           Servant.Swagger              (HasSwagger (toSwagger), subOperations)

import qualified Paths_cardano_sl_explorer    as CSLE
import qualified Pos.Explorer.Web.ClientTypes as C
import qualified Pos.Explorer.Web.Api         as A
import           Pos.Explorer.Web.Error       (ExplorerError)

import qualified Description                  as D

main :: IO ()
main = do
    BSL8.writeFile jsonFile $ encode swaggerSpecForExplorerApi
    putStrLn $ "Done. See " <> jsonFile <> "."
  where
    jsonFile = "explorer-web-api-swagger.json"

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
instance ToSchema      C.CNetworkAddress
instance ToSchema      ExplorerError

-- | Instance for Either-based types (types we return as 'Right') in responses.
-- Due 'typeOf' these types must be 'Typeable'.
-- We need this instance for correct Swagger-specification.
instance {-# OVERLAPPING #-} (Typeable a, ToSchema a) => ToSchema (Either ExplorerError a) where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped . name ?~ show (typeRep (Proxy @(Either ExplorerError a)))

-- | Helper type for subApi, we use it to create description.
type Op = Traversal' Swagger Operation

-- | Build Swagger-specification from 'explorerApi'.
swaggerSpecForExplorerApi :: Swagger
swaggerSpecForExplorerApi = toSwagger A.explorerApi
    & info . title       .~ "Cardano SL Explorer Web API"
    & info . version     .~ (toText $ showVersion CSLE.version)
    & info . description ?~ "This is an API for Cardano SL Explorer."
    & host               ?~ "cardanoexplorer.com"
    -- Descriptions for all endpoints.
    & blocksPages       . description ?~ D.blocksPagesDescription
    & blocksPagesTotal  . description ?~ D.blocksPagesTotalDescription
    & blocksSummary     . description ?~ D.blocksSummaryDescription
    & blocksTxs         . description ?~ D.blocksTxsDescription
    & txsLast           . description ?~ D.txsLastDescription
    & txsSummary        . description ?~ D.txsSummaryDescription
    & addressSummary    . description ?~ D.addressSummaryDescription
    & epochSlotSearch   . description ?~ D.epochSlotSearchDescription
  where
    -- | SubOperations for all endpoints in 'explorerApi'.
    -- We need it to fill description sections in produced HTML-documentation.
    blocksPages         = subOperations (Proxy @A.BlocksPages) A.explorerApi :: Op
    blocksPagesTotal    = subOperations (Proxy @A.BlocksPagesTotal) A.explorerApi :: Op
    blocksSummary       = subOperations (Proxy @A.BlocksSummary) A.explorerApi :: Op
    blocksTxs           = subOperations (Proxy @A.BlocksTxs) A.explorerApi :: Op
    txsLast             = subOperations (Proxy @A.TxsLast) A.explorerApi :: Op
    txsSummary          = subOperations (Proxy @A.TxsSummary) A.explorerApi :: Op
    addressSummary      = subOperations (Proxy @A.AddressSummary) A.explorerApi :: Op
    epochSlotSearch     = subOperations (Proxy @A.EpochSlotSearch) A.explorerApi :: Op
