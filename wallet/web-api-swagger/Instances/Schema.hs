{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Instances of `ToSchema` & `ToParamSchema`

module Instances.Schema () where

import           Universum

import           Control.Lens          ((?~))
import           Data.Swagger          (NamedSchema (..), SwaggerType (..),
                                        ToParamSchema (..), ToSchema (..),
                                        declareNamedSchema, declareSchemaRef, format,
                                        properties, required, type_)
import           Servant.Multipart     (FileData (..))

import           Pos.Types             (ApplicationName, BlockCount (..), BlockVersion,
                                        ChainDifficulty, Coin, SlotCount (..),
                                        SoftwareVersion)
import           Pos.Util.BackupPhrase (BackupPhrase)

import qualified Pos.Wallet.Web.ClientTypes as CT
import qualified Pos.Wallet.Web.Error.Types as ET

-- | Instances we need to build Swagger-specification for 'walletApi':
-- 'ToParamSchema' - for types in parameters ('Capture', etc.),
-- 'ToSchema' - for types in bodies.
instance ToSchema      Coin
instance ToParamSchema Coin
instance ToSchema      CT.CTxId
instance ToParamSchema CT.CTxId
instance ToSchema      CT.CTx
instance ToSchema      CT.CTxMeta
instance ToSchema      CT.CHash
instance ToParamSchema CT.CHash
instance ToSchema      (CT.CId CT.Wal)
instance ToSchema      (CT.CId CT.Addr)
instance ToParamSchema (CT.CId CT.Wal)
instance ToParamSchema (CT.CId CT.Addr)
instance ToSchema      CT.CProfile
instance ToSchema      ET.WalletError

instance ToSchema      CT.CAccountId
instance ToParamSchema CT.CAccountId where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & format ?~ "walletSetAddress@walletKeyIndex"

instance ToSchema      CT.CWalletAssurance
instance ToSchema      CT.CAccountMeta
instance ToSchema      CT.CWalletMeta
instance ToSchema      CT.CAccountInit
instance ToSchema      CT.CWalletInit
instance ToSchema      CT.CWalletRedeem
instance ToSchema      CT.CWallet
instance ToSchema      CT.CAccount
instance ToSchema      CT.CAddress
instance ToSchema      CT.CPaperVendWalletRedeem
instance ToSchema      CT.CCoin
instance ToSchema      CT.CInitialized
instance ToSchema      CT.CElectronCrashReport
instance ToSchema      CT.CUpdateInfo
instance ToSchema      SoftwareVersion
instance ToSchema      ApplicationName
instance ToSchema      CT.SyncProgress
instance ToSchema      BlockCount
instance ToSchema      SlotCount
instance ToSchema      ChainDifficulty
instance ToSchema      BlockVersion
instance ToSchema      BackupPhrase
instance ToParamSchema CT.CPassPhrase

instance ToSchema FileData where
    declareNamedSchema _ = do
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)
        filepathSchema <- declareSchemaRef (Proxy :: Proxy FilePath)
        return $ NamedSchema (Just "FileData") $ mempty
            & type_ .~ SwaggerObject
            & properties .~
                [ ("fdInputFile", textSchema)
                , ("fdFileName", textSchema)
                , ("fdFileCType", textSchema)
                , ("fdFilePath", filepathSchema)
                ]
            & required .~ [ "fdInputFile", "fdFileName", "fdFileCType", "fdFilePath"]
