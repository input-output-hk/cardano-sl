{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | This program builds Swagger specification for wallet web API and converts it to JSON.
-- We run this program during CI build.
-- Produced JSON will be used to create online 
-- version of wallet web API description at http://cardano-docs.iohk.io
-- (please see 'update_wallet_web_api_docs.sh' for technical details).

module Main where

import           Universum

import           Control.Lens                   ((?~), mapped)
import           Data.Aeson                     (encode)
import qualified Data.ByteString.Lazy.Char8     as BSL8
import           Data.Swagger                   (Swagger, ToSchema (..), ToParamSchema,
                                                 declareNamedSchema,
                                                 genericDeclareNamedSchema, defaultSchemaOptions,
                                                 name, info, description, version, title, host)
import           Data.Typeable                  (Typeable, typeRep)
import           Data.Version                   (showVersion)

import           Servant.Swagger                (toSwagger)

import           Pos.Types                      (Coin, SoftwareVersion, ApplicationName,
                                                 ChainDifficulty, BlockVersion)
import           Pos.Util.BackupPhrase          (BackupPhrase)
import           Pos.Wallet.Web                 (CAddress, CCurrency, CHash, CInitialized, 
                                                 CProfile, CTType, CTx, CTxId, CTxMeta, CUpdateInfo,
                                                 CWallet, CWalletInit, CWalletMeta, CWalletRedeem,
                                                 CWalletType, SyncProgress,
                                                 walletApi, WalletError)
import qualified Paths_cardano_sl               as CSL

main :: IO ()
main = do
    BSL8.writeFile jsonFile $ encode swaggerSpecForWalletApi
    putStrLn $ "Done. See " <> jsonFile <> "."
  where
    jsonFile = "wallet-web-api-swagger.json"

-- | Instances we need to build Swagger-specification for 'walletApi':
-- 'ToParamSchema' - for types in parameters ('Capture', etc.),
-- 'ToSchema' - for types in bodies.
instance ToSchema      Coin
instance ToParamSchema Coin
instance ToSchema      CTxId
instance ToParamSchema CTxId
instance ToSchema      CTType
instance ToSchema      CTx
instance ToSchema      CTxMeta
instance ToSchema      CHash
instance ToParamSchema CHash
instance ToSchema      CAddress
instance ToParamSchema CAddress
instance ToSchema      CCurrency
instance ToParamSchema CCurrency
instance ToSchema      CProfile
instance ToSchema      WalletError
instance ToSchema      CWalletMeta
instance ToSchema      CWalletInit
instance ToSchema      CWalletType
instance ToSchema      CWalletRedeem
instance ToSchema      CWallet
instance ToSchema      CInitialized
instance ToSchema      CUpdateInfo
instance ToSchema      SoftwareVersion
instance ToSchema      ApplicationName
instance ToSchema      SyncProgress
instance ToSchema      ChainDifficulty
instance ToSchema      BlockVersion
instance ToSchema      BackupPhrase

-- | Instance for Either-based types (types we return as 'Right') in responses.
-- Due 'typeOf' these types must be 'Typeable'.
-- We need this instance for correct Swagger-specification.
instance {-# OVERLAPPING #-} (Typeable a, ToSchema a) => ToSchema (Either WalletError a) where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped . name ?~ toText ("Either WalletError " ++ show (typeRep right))
      where
        right :: Proxy a
        right = Proxy

-- | Build Swagger-specification from 'walletApi'.
swaggerSpecForWalletApi :: Swagger
swaggerSpecForWalletApi = toSwagger walletApi
    & info . title       .~ "Cardano SL Wallet Web API"
    & info . version     .~ (toText $ showVersion CSL.version)
    & info . description ?~ "This is an API for Cardano SL wallet."
    & host               ?~ "localhost:8090" -- Default node's port for wallet web API.
