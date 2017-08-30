{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import           Control.Lens               (mapped, (?~))
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Swagger               (ToSchema (..), declareNamedSchema,
                                             defaultSchemaOptions, genericDeclareNamedSchema,
                                             host, name)
import           Data.Typeable              (Typeable, typeRep)
import           Data.Version               (showVersion)
import           Options.Applicative        (execParser, footer, fullDesc, header, help,
                                             helper, infoOption, long, progDesc)
import qualified Options.Applicative        as Opt
import           Universum

import qualified Paths_cardano_sl           as CSL

import qualified Pos.Wallet.Web.Error.Types as ET

import           Pos.Wallet.Web.Swagger

showProgramInfoIfRequired :: FilePath -> IO ()
showProgramInfoIfRequired generatedJSON = void $ execParser programInfo
  where
    programInfo = Opt.info (helper <*> versionOption) $
        fullDesc <> progDesc "Generate Swagger specification for Wallet web API."
                 <> header   "Cardano SL Wallet web API docs generator."
                 <> footer   ("This program runs during 'cardano-sl' building on Travis CI. " <>
                              "Generated file '" <> generatedJSON <> "' will be used to produce HTML documentation. " <>
                              "This documentation will be published at cardanodocs.com using 'update_wallet_web_api_docs.sh'.")

    versionOption = infoOption
        ("cardano-swagger-" <> showVersion CSL.version)
        (long "version" <> help "Show version.")

-- | Instance for Either-based types (types we return as 'Right') in responses.
-- Due 'typeOf' these types must be 'Typeable'.
-- We need this instance for correct Swagger-specification.
instance {-# OVERLAPPING #-}
         (Typeable a, ToSchema a) =>
         ToSchema (Either ET.WalletError a) where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped . name ?~ show (typeRep $ Proxy @(Either ET.WalletError a))

main :: IO ()
main = do
    showProgramInfoIfRequired jsonFile
    BSL8.writeFile jsonFile $ encode spec
    putStrLn $ "Done. See " <> jsonFile <> "."
  where
    jsonFile = "wallet-web-api-swagger.json"
    spec = swaggerSpecForWalletApi & host ?~ "localhost:8090" -- Default node's port for wallet web API.
