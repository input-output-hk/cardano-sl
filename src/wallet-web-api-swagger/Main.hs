{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | This program builds Swagger specification for wallet web API and converts it to JSON.
-- We run this program during CI build.
-- Produced JSON will be used to create online
-- version of wallet web API description at http://cardano-docs.iohk.io
-- (please see 'update_wallet_web_api_docs.sh' for technical details).

module Main where

import           Universum

import           Control.Lens               (mapped, (?~))
import           Data.Aeson                 (Value (..), encode)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Swagger               (Swagger, ToParamSchema (..), ToSchema (..),
                                             declareNamedSchema, defaultSchemaOptions,
                                             default_, description,
                                             genericDeclareNamedSchema, host, info, name,
                                             title, version)
import           Data.Typeable              (Typeable, typeRep)
import           Data.Version               (showVersion)

import           Servant.Swagger            (toSwagger)

import qualified Paths_cardano_sl           as CSL
import           Pos.Types                  (ApplicationName, BlockVersion,
                                             ChainDifficulty, Coin, SoftwareVersion)
import           Pos.Util.BackupPhrase      (BackupPhrase)
import           Pos.Wallet.Web             (CAccount, CAccountAddress, CAddress,
                                             CCurrency, CHash, CInitialized, CMaybe,
                                             CPassPhrase, CProfile, CTType, CTx, CTxId,
                                             CTxMeta, CUpdateInfo, CWallet,
                                             CWalletAddress, CWalletAssurance,
                                             CWalletInit, CWalletMeta, CWalletRedeem,
                                             CWalletSet, CWalletSetAddress,
                                             CWalletSetInit, CWalletSetMeta, CWalletType,
                                             SyncProgress, WalletError, walletApi)

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
instance ToSchema      CAccountAddress
instance ToParamSchema CAccountAddress where
    toParamSchema = undefined  -- TODO [CSM-171]
instance ToSchema      CWalletAddress
instance ToParamSchema CWalletAddress where
    toParamSchema = undefined  -- TODO [CSM-171]
instance ToSchema      CWalletSetAddress
instance ToParamSchema CWalletSetAddress
instance ToSchema      CWalletAssurance
instance ToSchema      CWalletMeta
instance ToSchema      CWalletSetMeta
instance ToSchema      CWalletInit
instance ToSchema      CWalletSetInit
instance ToSchema      CWalletType
instance ToSchema      CWalletRedeem
instance ToSchema      CWalletSet
instance ToSchema      CWallet
instance ToSchema      CAccount
instance ToSchema      CInitialized
instance ToSchema      CUpdateInfo
instance ToSchema      SoftwareVersion
instance ToSchema      ApplicationName
instance ToSchema      SyncProgress
instance ToSchema      ChainDifficulty
instance ToSchema      BlockVersion
instance ToSchema      BackupPhrase
instance ToParamSchema CPassPhrase
instance ToParamSchema a => ToParamSchema (CMaybe a) where
    toParamSchema _ = toParamSchema (Proxy @a) & default_ .~ Just (String "")

-- | Instance for Either-based types (types we return as 'Right') in responses.
-- Due 'typeOf' these types must be 'Typeable'.
-- We need this instance for correct Swagger-specification.
instance {-# OVERLAPPING #-} (Typeable a, ToSchema a) => ToSchema (Either WalletError a) where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped . name ?~ show (typeRep (Proxy @(Either WalletError a)))

-- | Build Swagger-specification from 'walletApi'.
swaggerSpecForWalletApi :: Swagger
swaggerSpecForWalletApi = toSwagger walletApi
    & info . title       .~ "Cardano SL Wallet Web API"
    & info . version     .~ (toText $ showVersion CSL.version)
    & info . description ?~ "This is an API for Cardano SL wallet."
    & host               ?~ "localhost:8090" -- Default node's port for wallet web API.
