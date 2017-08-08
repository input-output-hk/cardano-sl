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
import qualified Pos.Wallet.Web        as W

-- | Instances we need to build Swagger-specification for 'walletApi':
-- 'ToParamSchema' - for types in parameters ('Capture', etc.),
-- 'ToSchema' - for types in bodies.
instance ToSchema      Coin
instance ToParamSchema Coin
instance ToSchema      W.CTxId
instance ToParamSchema W.CTxId
instance ToSchema      W.CTx
instance ToSchema      W.CTxMeta
instance ToSchema      W.CHash
instance ToParamSchema W.CHash
instance ToSchema      (W.CId W.Wal)
instance ToSchema      (W.CId W.Addr)
instance ToParamSchema (W.CId W.Wal)
instance ToParamSchema (W.CId W.Addr)
instance ToSchema      W.CProfile
instance ToSchema      W.WalletError

instance ToSchema      W.CAccountId
instance ToParamSchema W.CAccountId where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & format ?~ "walletSetAddress@walletKeyIndex"

instance ToSchema      W.CWalletAssurance
instance ToSchema      W.CAccountMeta
instance ToSchema      W.CWalletMeta
instance ToSchema      W.CAccountInit
instance ToSchema      W.CWalletInit
instance ToSchema      W.CWalletRedeem
instance ToSchema      W.CWallet
instance ToSchema      W.CAccount
instance ToSchema      W.CAddress
instance ToSchema      W.CPaperVendWalletRedeem
instance ToSchema      W.CCoin
instance ToSchema      W.CInitialized
instance ToSchema      W.CElectronCrashReport
instance ToSchema      W.CUpdateInfo
instance ToSchema      SoftwareVersion
instance ToSchema      ApplicationName
instance ToSchema      W.SyncProgress
instance ToSchema      BlockCount
instance ToSchema      SlotCount
instance ToSchema      ChainDifficulty
instance ToSchema      BlockVersion
instance ToSchema      BackupPhrase
instance ToParamSchema W.CPassPhrase

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
