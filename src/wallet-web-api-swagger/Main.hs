{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | This program builds Swagger specification for wallet web API and converts it to JSON.
-- We run this program during CI build.
-- Produced JSON will be used to create online
-- version of wallet web API description at cardanodocs.com website
-- (please see 'update_wallet_web_api_docs.sh' for technical details).

module Main where

import           Universum

import           Control.Lens               (mapped, (?~))
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Swagger               (NamedSchema (..), Operation, Swagger,
                                             SwaggerType (..), ToParamSchema (..),
                                             ToSchema (..), declareNamedSchema,
                                             declareSchemaRef, defaultSchemaOptions,
                                             description, format,
                                             genericDeclareNamedSchema, host, info, name,
                                             properties, required, title, type_, version)
import           Data.Typeable              (Typeable, typeRep)
import           Data.Version               (showVersion)
import           Servant                    ((:>))
import           Servant.Multipart          (FileData (..), MultipartForm)
import           Servant.Swagger            (HasSwagger (toSwagger), subOperations)

import qualified Paths_cardano_sl           as CSL
import           Pos.Types                  (ApplicationName, BlockVersion,
                                             ChainDifficulty, Coin, SoftwareVersion)
import           Pos.Util.BackupPhrase      (BackupPhrase)
import qualified Pos.Wallet.Web             as W

import qualified Description                as D

main :: IO ()
main = do
    BSL8.writeFile jsonFile $ encode swaggerSpecForWalletApi
    putStrLn $ "Done. See " <> jsonFile <> "."
  where
    jsonFile = "wallet-web-api-swagger.json"

instance HasSwagger api => HasSwagger (MultipartForm a :> api) where
    toSwagger Proxy = toSwagger $ Proxy @api

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
instance ToSchema      (W.CAddress W.WS)
instance ToSchema      (W.CAddress W.Acc)
instance ToParamSchema (W.CAddress W.WS)
instance ToParamSchema (W.CAddress W.Acc)
instance ToSchema      W.CProfile
instance ToSchema      W.WalletError

-- TODO: currently not used
instance ToSchema      W.CAccountAddress
instance ToParamSchema W.CAccountAddress where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & format ?~ "walletSetAddress@walletIndex@accountIndex@address"

instance ToSchema      W.CWalletAddress
instance ToParamSchema W.CWalletAddress where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & format ?~ "walletSetAddress@walletKeyIndex"

instance ToSchema      W.CWalletSetAssurance
instance ToSchema      W.CWalletMeta
instance ToSchema      W.CWalletSetMeta
instance ToSchema      W.CWalletInit
instance ToSchema      W.CWalletSetInit
instance ToSchema      W.CWalletRedeem
instance ToSchema      W.CWalletSet
instance ToSchema      W.CWallet
instance ToSchema      W.CAccount
instance ToSchema      W.CPaperVendWalletRedeem
instance ToSchema      W.CCoin
instance ToSchema      W.CInitialized
instance ToSchema      W.CElectronCrashReport
instance ToSchema      W.CUpdateInfo
instance ToSchema      SoftwareVersion
instance ToSchema      ApplicationName
instance ToSchema      W.SyncProgress
instance ToSchema      ChainDifficulty
instance ToSchema      BlockVersion
instance ToSchema      BackupPhrase
instance ToParamSchema W.CPassPhrase

-- | Instance for Either-based types (types we return as 'Right') in responses.
-- Due 'typeOf' these types must be 'Typeable'.
-- We need this instance for correct Swagger-specification.
instance {-# OVERLAPPING #-} (Typeable a, ToSchema a) => ToSchema (Either W.WalletError a) where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped . name ?~ show (typeRep (Proxy @(Either W.WalletError a)))

-- | Helper type for subApi, we use it to create description.
type Op = Traversal' Swagger Operation

-- | Build Swagger-specification from 'walletApi'.
swaggerSpecForWalletApi :: Swagger
swaggerSpecForWalletApi = toSwagger W.walletApi
    & info . title       .~ "Cardano SL Wallet Web API"
    & info . version     .~ (toText $ showVersion CSL.version)
    & info . description ?~ "This is an API for Cardano SL wallet."
    & host               ?~ "localhost:8090" -- Default node's port for wallet web API.
    -- Descriptions for all endpoints.
    & testReset              . description ?~ D.testResetDescription

    & getWSet                . description ?~ D.getWSetDescription
    & getWSets               . description ?~ D.getWSetsDescription
    & newWSet                . description ?~ D.newWSetDescription
    & restoreWSet            . description ?~ D.restoreWSetDescription
    & renameWSet             . description ?~ D.renameWSetDescription
    & importWSet             . description ?~ D.importWSetDescription
    & changeWSetPassphrase   . description ?~ D.changeWSetPassphraseDescription

    & getWallet              . description ?~ D.getWalletDescription
    & getWallets             . description ?~ D.getWalletsDescription
    & updateWallet           . description ?~ D.updateWalletDescription
    & newWallet              . description ?~ D.newWalletDescription
    & deleteWallet           . description ?~ D.deleteWalletDescription

    & newAccount             . description ?~ D.newAccountDescription

    & isValidAddress         . description ?~ D.isValidAddressDescription

    & getProfile             . description ?~ D.getProfileDescription
    & updateProfile          . description ?~ D.updateProfileDescription

    & newPayment             . description ?~ D.newPaymentDescription
    & newPaymentExt          . description ?~ D.newPaymentExtDescription
    & updateTx               . description ?~ D.updateTxDescription
    & getHistory             . description ?~ D.getHistoryDescription
    & searchHistory          . description ?~ D.searchHistoryDescription

    & nextUpdate             . description ?~ D.nextUpdateDescription
    & applyUpdate            . description ?~ D.applyUpdateDescription

    & redeemADA              . description ?~ D.redeemADADescription
    & redeemADAPaperVend     . description ?~ D.redeemADAPaperVendDescription

    & reportingInitialized   . description ?~ D.reportingInitializedDescription
    & reportingElectroncrash . description ?~ D.reportingElectroncrashDescription

    & getSlotsDuration       . description ?~ D.getSlotsDurationDescription
    & getVersion             . description ?~ D.getVersionDescription
    & getSyncProgress        . description ?~ D.getSyncProgressDescription
  where
    -- | SubOperations for all endpoints in 'walletApi'.
    -- We need it to fill description sections in produced HTML-documentation.
    testReset              = subOperations (Proxy @W.TestReset) W.walletApi :: Op

    getWSet                = subOperations (Proxy @W.GetWalletSet) W.walletApi :: Op
    getWSets               = subOperations (Proxy @W.GetWalletSets) W.walletApi :: Op
    newWSet                = subOperations (Proxy @W.NewWalletSet) W.walletApi :: Op
    restoreWSet            = subOperations (Proxy @W.RestoreWalletSet) W.walletApi :: Op
    renameWSet             = subOperations (Proxy @W.RenameWalletSet) W.walletApi :: Op
    importWSet             = subOperations (Proxy @W.ImportWalletSet) W.walletApi :: Op
    changeWSetPassphrase   = subOperations (Proxy @W.ChangeWalletSetPassphrase) W.walletApi :: Op

    getWallet              = subOperations (Proxy @W.GetWallet) W.walletApi :: Op
    getWallets             = subOperations (Proxy @W.GetWallets) W.walletApi :: Op
    updateWallet           = subOperations (Proxy @W.UpdateWallet) W.walletApi :: Op
    newWallet              = subOperations (Proxy @W.NewWallet) W.walletApi :: Op
    deleteWallet           = subOperations (Proxy @W.DeleteWallet) W.walletApi :: Op

    newAccount             = subOperations (Proxy @W.NewAccount) W.walletApi :: Op

    isValidAddress         = subOperations (Proxy @W.IsValidAddress) W.walletApi :: Op

    getProfile             = subOperations (Proxy @W.GetProfile) W.walletApi :: Op
    updateProfile          = subOperations (Proxy @W.UpdateProfile) W.walletApi :: Op

    newPayment             = subOperations (Proxy @W.NewPayment) W.walletApi :: Op
    newPaymentExt          = subOperations (Proxy @W.NewPaymentExt) W.walletApi :: Op
    updateTx               = subOperations (Proxy @W.UpdateTx) W.walletApi :: Op
    getHistory             = subOperations (Proxy @W.GetHistory) W.walletApi :: Op
    searchHistory          = subOperations (Proxy @W.SearchHistory) W.walletApi :: Op

    nextUpdate             = subOperations (Proxy @W.NextUpdate) W.walletApi :: Op
    applyUpdate            = subOperations (Proxy @W.ApplyUpdate) W.walletApi :: Op

    redeemADA              = subOperations (Proxy @W.RedeemADA) W.walletApi :: Op
    redeemADAPaperVend     = subOperations (Proxy @W.RedeemADAPaperVend) W.walletApi :: Op

    reportingInitialized   = subOperations (Proxy @W.ReportingInitialized) W.walletApi :: Op
    reportingElectroncrash = subOperations (Proxy @W.ReportingElectroncrash) W.walletApi :: Op

    getSlotsDuration       = subOperations (Proxy @W.GetSlotsDuration) W.walletApi :: Op
    getVersion             = subOperations (Proxy @W.GetVersion) W.walletApi :: Op
    getSyncProgress        = subOperations (Proxy @W.GetSyncProgress) W.walletApi :: Op
