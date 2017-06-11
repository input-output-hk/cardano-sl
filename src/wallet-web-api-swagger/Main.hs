{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Main
  ( main
  ) where

import           Control.Lens                       (mapped, (?~))
import           Data.Aeson                         (encode)
import qualified Data.ByteString.Lazy.Char8         as BSL8
import           Data.Swagger                       (NamedSchema (..), Operation, Swagger,
                                                     SwaggerType (..), ToParamSchema (..),
                                                     ToSchema (..), declareNamedSchema,
                                                     declareSchemaRef,
                                                     defaultSchemaOptions, description,
                                                     format, genericDeclareNamedSchema,
                                                     host, info, name, properties,
                                                     required, title, type_, version)
import           Data.Typeable                      (Typeable, typeRep)
import           Data.Version                       (showVersion)
import           Options.Applicative.Simple         (execParser, footer, fullDesc, header,
                                                     help, helper, infoOption, long,
                                                     progDesc)
import qualified Options.Applicative.Simple         as S
import           Servant                            ((:>))
import           Servant.Multipart                  (FileData (..), MultipartForm)
import           Servant.Swagger                    (HasSwagger (toSwagger),
                                                     subOperations)
import           Servant.Swagger.Internal.TypeLevel (IsSubAPI)
import           Universum

import qualified Paths_cardano_sl                   as CSL
import           Pos.Types                          (ApplicationName, BlockVersion,
                                                     ChainDifficulty, Coin,
                                                     SoftwareVersion)
import           Pos.Util.BackupPhrase              (BackupPhrase)
import           Pos.Util.Servant                   (CDecodeArg, VerbMod)
import qualified Pos.Wallet.Web                     as W

import qualified Description                        as D

showProgramInfoIfRequired :: FilePath -> IO ()
showProgramInfoIfRequired generatedJSON = void $ execParser programInfo
  where
    programInfo = S.info (helper <*> versionOption) $
        fullDesc <> progDesc "Generate Swagger specification for Wallet web API."
                 <> header   "Cardano SL Wallet web API docs generator."
                 <> footer   ("This program runs during 'cardano-sl' building on Travis CI. " <>
                              "Generated file '" <> generatedJSON <> "' will be used to produce HTML documentation. " <>
                              "This documentation will be published at cardanodocs.com using 'update_wallet_web_api_docs.sh'.")

    versionOption = infoOption
        ("cardano-swagger-" <> showVersion CSL.version)
        (long "version" <> help "Show version.")


main :: IO ()
main = do
    showProgramInfoIfRequired jsonFile
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
instance ToSchema      (W.CId W.Wal)
instance ToSchema      (W.CId W.Addr)
instance ToParamSchema (W.CId W.Wal)
instance ToParamSchema (W.CId W.Addr)
instance ToSchema      W.CProfile
instance ToSchema      W.WalletError

-- TODO: currently not used
instance ToSchema      W.CWAddressMeta
instance ToParamSchema W.CWAddressMeta where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & format ?~ "walletSetAddress@walletIndex@accountIndex@address"

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

instance HasSwagger v => HasSwagger (VerbMod mod v) where
    toSwagger _ = toSwagger (Proxy @v)

instance HasSwagger (apiType a :> res) => HasSwagger (CDecodeArg apiType a :> res) where
    toSwagger _ = toSwagger (Proxy @(apiType a :> res))

-- | Wallet API operations.
wop
    :: forall sub.
       ( IsSubAPI (W.ApiPrefix :> sub) W.WalletApi
       , HasSwagger (W.ApiPrefix :> sub)
       )
    => Traversal' Swagger Operation
wop = subOperations (Proxy @(W.ApiPrefix :> sub)) W.walletApi

-- | Build Swagger-specification from 'walletApi'.
swaggerSpecForWalletApi :: Swagger
swaggerSpecForWalletApi = toSwagger W.walletApi
    & info . title       .~ "Cardano SL Wallet Web API"
    & info . version     .~ (toText $ showVersion CSL.version)
    & info . description ?~ "This is an API for Cardano SL wallet."
    & host               ?~ "localhost:8090" -- Default node's port for wallet web API.
    -- Descriptions for all endpoints.
    & wop @W.TestReset              . description ?~ D.testReset

    & wop @W.GetWallet              . description ?~ D.getWallet
    & wop @W.GetWallets             . description ?~ D.getWallets
    & wop @W.NewWallet              . description ?~ D.newWallet
    & wop @W.RestoreWallet          . description ?~ D.restoreWallet
    & wop @W.RenameWallet           . description ?~ D.renameWallet
    & wop @W.DeleteWallet           . description ?~ D.deleteWallet
    & wop @W.ImportWallet           . description ?~ D.importWallet
    & wop @W.ChangeWalletPassphrase . description ?~ D.changeWalletPassphrase

    & wop @W.GetAccount             . description ?~ D.getAccount
    & wop @W.GetAccounts            . description ?~ D.getAccounts
    & wop @W.UpdateAccount          . description ?~ D.updateAccount
    & wop @W.NewAccount             . description ?~ D.newAccount
    & wop @W.DeleteAccount          . description ?~ D.deleteAccount

    & wop @W.NewWAddress            . description ?~ D.newWAddress

    & wop @W.IsValidAddress         . description ?~ D.isValidAddress

    & wop @W.GetProfile             . description ?~ D.getProfile
    & wop @W.UpdateProfile          . description ?~ D.updateProfile

    & wop @W.NewPayment             . description ?~ D.newPayment
    & wop @W.NewPaymentExt          . description ?~ D.newPaymentExt
    & wop @W.UpdateTx               . description ?~ D.updateTx
    & wop @W.SearchHistory          . description ?~ D.searchHistory

    & wop @W.NextUpdate             . description ?~ D.nextUpdate
    & wop @W.ApplyUpdate            . description ?~ D.applyUpdate

    & wop @W.RedeemADA              . description ?~ D.redeemADA
    & wop @W.RedeemADAPaperVend     . description ?~ D.redeemADAPaperVend

    & wop @W.ReportingInitialized   . description ?~ D.reportingInitialized
    & wop @W.ReportingElectroncrash . description ?~ D.reportingElectroncrash

    & wop @W.GetSlotsDuration       . description ?~ D.getSlotsDuration
    & wop @W.GetVersion             . description ?~ D.getVersion
    & wop @W.GetSyncProgress        . description ?~ D.getSyncProgress
