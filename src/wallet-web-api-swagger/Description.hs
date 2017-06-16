{-# LANGUAGE OverloadedStrings #-}

-- | Descriptions for each endpoint, for Swagger-documentation.

module Description
  ( module Description
  ) where

import           Universum

testReset

  , getWallet
  , getWallets
  , newWallet
  , restoreWallet
  , renameWallet
  , deleteWallet
  , importWallet
  , changeWalletPassphrase

  , getAccount
  , getAccounts
  , updateAccount
  , newAccount
  , deleteAccount

  , newAddress

  , isValidAddress

  , getProfile
  , updateProfile

  , newPayment
  , newPaymentExt
  , updateTx
  , getHistory
  , searchHistory

  , nextUpdate
  , applyUpdate

  , redeemADA
  , redeemADAPaperVend

  , reportingInitialized
  , reportingElectroncrash

  , getSlotsDuration
  , getVersion
  , getSyncProgress :: Text
testReset              = "Delete all secret keys. It works in development mode only, " <>
                                    "returns HTTP 403 otherwise."

getWallet              = "Get information about a wallet by its ID (address)."
getWallets             = "Get information about all available wallets."
newWallet              = "Create a new wallet."
restoreWallet          = "Create a new wallet."
renameWallet           = "Change name of given wallet."
deleteWallet           = "Delete given wallet with all contained wallets."
importWallet           = "Import user's secret key from the path to generate wallet."
changeWalletPassphrase =
    "Change passphrase of given wallet.\n\
    \NOTE: this will change IDs of all accounts belonging to given wallet, and \
    \will send all money from old accounts to newly created ones. \
    \In case of transaction rollback, wallet will become to store new empty \
    \accounts, while money will remain at the old accounts, thus make sure to \
    \create a backup before performing this action."

getAccount             = "Get information about a account by account's ID \
                         \(address + index of account in wallet)."
getAccounts            = "Get information about all available accounts."
updateAccount          = "Update account's meta information."
newAccount             = "Create a new account in given wallet."
deleteAccount          = "Delete a account by account's ID (address + index of \
                         \account in wallet)."

newAddress             = "Create a new address in given account."

isValidAddress         = "Returns True if given address is valid, False otherwise."

getProfile             = "Get user profile's meta data."
updateProfile          = "Update user profile."

newPayment             = "Create a new payment transaction."
newPaymentExt          = "Create a new payment transaction with extended information."
updateTx               = "Update payment transaction."
getHistory             = "Get the history of transactions."
searchHistory          = "Search in the history of transactions."

nextUpdate             = "Get information about the next update."
applyUpdate            = "Apply last update."

redeemADA              = "Redeem ADA."
redeemADAPaperVend     = "Redeem ADA, paper vending."

reportingInitialized   = "Send node's report on initialization time."
reportingElectroncrash = "Send node's report on electron crash info."

getSlotsDuration       = "Get blockchain slot duration in milliseconds."
getVersion             = "Get current version of the node."
getSyncProgress        = "Sync progress, with info about local chain difficulty," <>
                         "network chain difficulty and connected peers."
