{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Descriptions for each endpoint, for Swagger-documentation.

module Pos.Wallet.Web.Swagger.Description where

import           Universum

import           Control.Lens                       ((?~))
import           Data.Swagger                       (Operation, Swagger, description)
import           Servant                            ((:>))
import           Servant.Swagger                    (HasSwagger, subOperations)
import           Servant.Swagger.Internal.TypeLevel (IsSubAPI)

import           Pos.Wallet.Web.Swagger.CustomSwagger     (HasCustomSwagger (..))
import           Pos.Wallet.Web.Swagger.Instances.Schema  ()
import           Pos.Wallet.Web.Swagger.Instances.Swagger ()
import           Pos.Wallet.Web.Api

-- | Wallet API operations, i.e. modifier of part of api related to
-- single endpoint.
wop
    :: forall sub.
       ( IsSubAPI (ApiPrefix :> sub) WalletApi
       , HasSwagger (ApiPrefix :> sub)
       )
    => Proxy sub -> Traversal' Swagger Operation
wop _ = subOperations (Proxy @(ApiPrefix :> sub)) walletApi

modifyDescription
    :: (IsSubAPI (ApiPrefix :> api) WalletApi
       , HasSwagger api
       )
    => Text -> Proxy api -> Swagger -> Swagger
modifyDescription desc api = wop api . description ?~ desc


instance HasCustomSwagger api => HasCustomSwagger (ApiPrefix :> api) where
    swaggerModifier _ = swaggerModifier (Proxy @api)

instance HasCustomSwagger TestReset where
    swaggerModifier = modifyDescription
        "Delete all secret keys. It works in development mode only, \
        \returns HTTP 403 otherwise."

instance HasCustomSwagger GetWallet where
    swaggerModifier = modifyDescription
        "Get information about a wallet by its ID (address)."

instance HasCustomSwagger GetWallets where
    swaggerModifier = modifyDescription
        "Get information about all available wallets."

instance HasCustomSwagger NewWallet where
    swaggerModifier = modifyDescription
        "Create a new wallet."

instance HasCustomSwagger UpdateWallet where
    swaggerModifier = modifyDescription
        "Update wallet's meta information."

instance HasCustomSwagger RestoreWallet where
    swaggerModifier = modifyDescription
        "Create a new wallet."

instance HasCustomSwagger DeleteWallet where
    swaggerModifier = modifyDescription
        "Delete given wallet with all contained accounts."

instance HasCustomSwagger ImportWallet where
    swaggerModifier = modifyDescription
        "Import user's secret key from the path to generate wallet."

instance HasCustomSwagger ChangeWalletPassphrase where
    swaggerModifier = modifyDescription
        "Change passphrase of given wallet."


instance HasCustomSwagger GetAccount where
    swaggerModifier = modifyDescription
        "Get information about a account by account's ID \
        \(address + index of account in wallet)."

instance HasCustomSwagger GetAccounts where
    swaggerModifier = modifyDescription
        "Get information about all available accounts."

instance HasCustomSwagger UpdateAccount where
    swaggerModifier = modifyDescription
        "Update account's meta information."

instance HasCustomSwagger NewAccount where
    swaggerModifier = modifyDescription
        "Create a new account in given wallet."

instance HasCustomSwagger DeleteAccount where
    swaggerModifier = modifyDescription
        "Delete a account by account's ID (address + index of \
        \account in wallet)."


instance HasCustomSwagger NewAddress where
    swaggerModifier = modifyDescription
        "Create a new address in given account."


instance HasCustomSwagger IsValidAddress where
    swaggerModifier = modifyDescription
        "Returns True if given address is valid, False otherwise."


instance HasCustomSwagger GetProfile where
    swaggerModifier = modifyDescription
        "Get user profile's meta data."

instance HasCustomSwagger UpdateProfile where
    swaggerModifier = modifyDescription
        "Update user profile."


instance HasCustomSwagger NewPayment where
    swaggerModifier = modifyDescription
        "Create a new payment transaction."

instance HasCustomSwagger TxFee where
    swaggerModifier = modifyDescription
        "Estimate fees for performing given transaction. \
        \Transaction will not be created."

instance HasCustomSwagger UpdateTx where
    swaggerModifier = modifyDescription
        "Update payment transaction."

instance HasCustomSwagger GetHistory where
    swaggerModifier = modifyDescription
        "Get the history of transactions."


instance HasCustomSwagger NextUpdate where
    swaggerModifier = modifyDescription
        "Get information about the next update."

instance HasCustomSwagger ApplyUpdate where
    swaggerModifier = modifyDescription
        "Apply last update."


instance HasCustomSwagger RedeemADA where
    swaggerModifier = modifyDescription
        "Redeem ADA."

instance HasCustomSwagger RedeemADAPaperVend where
    swaggerModifier = modifyDescription
        "Redeem ADA, paper vending."


instance HasCustomSwagger ReportingInitialized where
    swaggerModifier = modifyDescription
        "Send node's report on initialization time."

instance HasCustomSwagger ReportingElectroncrash where
    swaggerModifier = modifyDescription
        "Send node's report on electron crash info."


instance HasCustomSwagger GetSlotsDuration where
    swaggerModifier = modifyDescription
        "Get blockchain slot duration in milliseconds."

instance HasCustomSwagger GetVersion where
    swaggerModifier = modifyDescription
        "Get current version of the node."

instance HasCustomSwagger GetSyncProgress where
    swaggerModifier = modifyDescription
        "Sync progress, with info about local chain difficulty,\
        \network chain difficulty and connected peers."


instance HasCustomSwagger ImportBackupJSON where
    swaggerModifier = modifyDescription
        "Import full information about wallet from a given file."

instance HasCustomSwagger ExportBackupJSON where
    swaggerModifier = modifyDescription
        "Export full information about wallet in JSON format into a file under \
        \given path. Wallet may be later restored from this file with \
        \'ImportBackupJSON' endpoint."
