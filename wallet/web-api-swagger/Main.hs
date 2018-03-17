{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import           Control.Lens ((?~))
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Swagger (host)
import           Data.Version (showVersion)
import           Options.Applicative (execParser, footer, fullDesc, header, help, helper,
                                      infoOption, long, progDesc)
import qualified Options.Applicative as Opt
import           Universum

import qualified Paths_cardano_sl as CSL

import           Pos.Wallet.Web.Swagger
import           Pos.Wallet.Web.Swagger.Instances.Schema ()

showProgramInfoIfRequired :: FilePath -> IO ()
showProgramInfoIfRequired generatedJSON = void $ execParser programInfo
  where
    programInfo = Opt.info (helper <*> versionOption) $
        fullDesc <> progDesc "Generate Swagger specification for Wallet web API."
                 <> header   "Cardano SL Wallet web API docs generator."
                 <> footer   ("This program runs during 'cardano-sl' building on CI. " <>
                              "Generated file '" <> generatedJSON <> "' will be used to produce HTML documentation. " <>
                              "This documentation will be published at cardanodocs.com using 'update_wallet_web_api_docs.sh'.")

    versionOption = infoOption
        ("cardano-swagger-" <> showVersion CSL.version)
        (long "version" <> help "Show version.")

main :: IO ()
main = do
    showProgramInfoIfRequired jsonFile
    BSL8.writeFile jsonFile $ encode spec
    putStrLn $ "Done. See " <> jsonFile <> "."
  where
    jsonFile = "wallet-web-api-swagger.json"
    spec = swaggerSpecForWalletApi & host ?~ "localhost:8090" -- Default node's port for wallet web API.
