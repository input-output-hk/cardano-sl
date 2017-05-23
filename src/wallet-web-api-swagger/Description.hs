{-# LANGUAGE OverloadedStrings #-}

-- | Descriptions for each endpoint, for Swagger-documentation.

module Description
  ( module Description
  ) where

import           Universum

testResetDescription

  , getWSetDescription
  , getWSetsDescription
  , newWSetDescription
  , restoreWSetDescription
  , renameWSetDescription
  , deleteWSetDescription
  , importWSetDescription
  , changeWSetPassphraseDescription

  , getWalletDescription
  , getWalletsDescription
  , updateWalletDescription
  , newWalletDescription
  , deleteWalletDescription

  , newAccountDescription

  , isValidAddressDescription

  , getProfileDescription
  , updateProfileDescription

  , newPaymentDescription
  , newPaymentExtDescription
  , updateTxDescription
  , getHistoryDescription
  , searchHistoryDescription

  , nextUpdateDescription
  , applyUpdateDescription

  , redeemADADescription
  , redeemADAPaperVendDescription

  , reportingInitializedDescription
  , reportingElectroncrashDescription

  , getSlotsDurationDescription
  , getVersionDescription
  , getSyncProgressDescription :: Text
testResetDescription              = "Delete all secret keys. It works in development mode only, " <>
                                    "returns HTTP 403 otherwise."

getWSetDescription                = "Get information about a wallet set by its ID (address)."
getWSetsDescription               = "Get information about all available wallet sets."
newWSetDescription                = "Create a new wallet set."
restoreWSetDescription            = "Create a new wallet set."
renameWSetDescription             = "Change name of given wallet set."
deleteWSetDescription             = "Delete given wallet set with all contained wallets."
importWSetDescription             = "Import user's secret key from the path to generate wallet set."
changeWSetPassphraseDescription   = "Change passphrase of given wallet set."

newAccountDescription             = "Create a new account in given wallet."

getWalletDescription              = "Get information about a wallet by wallet's ID (address)."
getWalletsDescription             = "Get information about all available wallets."
updateWalletDescription           = "Update wallet's meta information."
newWalletDescription              = "Create a new wallet in given wallet set."
deleteWalletDescription           = "Delete a wallet by wallet's ID (address)."

isValidAddressDescription         = "Returns True if given address is valid, False otherwise."

getProfileDescription             = "Get user profile's meta data."
updateProfileDescription          = "Update user profile."

newPaymentDescription             = "Create a new payment transaction."
newPaymentExtDescription          = "Create a new payment transaction with extended information."
updateTxDescription               = "Update payment transaction."
getHistoryDescription             = "Get the history of transactions."
searchHistoryDescription          = "Search in the history of transactions."

nextUpdateDescription             = "Get information about the next update."
applyUpdateDescription            = "Apply last update."

redeemADADescription              = "Redeem ADA."
redeemADAPaperVendDescription     = "Redeem ADA, paper vending."

reportingInitializedDescription   = "Send node's report on initialization time."
reportingElectroncrashDescription = "Send node's report on electron crash info."

getSlotsDurationDescription       = "Get blockchain slot duration in milliseconds."
getVersionDescription             = "Get current version of the node."
getSyncProgressDescription        = "Sync progress, with info about local chain difficulty, " <>
                                    "network chain difficulty and connected peers."
