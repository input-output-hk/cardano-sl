-- | This program takes Swagger specification for wallet web API and stores it as JSON.
-- We run this program during CI build.
-- Produced JSON will be used to create online 
-- version of wallet web API description at http://cardano-docs.iohk.io
-- (please see 'update_wallet_web_api_docs.sh' for technical details).

module Main where

import           Universum

import           Data.Aeson                     (encode)
import qualified Data.ByteString.Lazy.Char8     as BSL8

import           Pos.Wallet.Web                 (swaggerSpecForWalletApi)

main :: IO ()
main = do
    BSL8.writeFile jsonFile $ encode swaggerSpecForWalletApi
    putStrLn $ "Done. See " <> jsonFile <> "."
  where
    jsonFile = "wallet-web-api-swagger.json"
