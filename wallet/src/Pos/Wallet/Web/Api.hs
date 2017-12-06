{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Servant API for wallet.

module Pos.Wallet.Web.Api
       ( WalletApi
       , WalletApiNoPrefix
       , walletApi

       , ApiPrefix
       , WalletVerb

       , TestReset
       , TestState

       , GetWallet
       , GetWallets
       , NewWallet
       , UpdateWallet
       , RestoreWallet
       , DeleteWallet
       , ImportWallet
       , ChangeWalletPassphrase

       , GetAccount
       , GetAccounts
       , UpdateAccount
       , DeleteAccount
       , NewAccount

       , NewAddress

       , IsValidAddress

       , GetProfile
       , UpdateProfile

       , NewPayment
       , NewPaymentBatch
       , TxFee
       , ResetFailedPtxs
       , UpdateTx
       , GetHistory

       , NextUpdate
       , PostponeUpdate
       , ApplyUpdate

       , RedeemADA
       , RedeemADAPaperVend

       , ReportingInitialized

       , GetSlotsDuration
       , GetVersion
       , GetSyncProgress
       , LocalTimeDifference

       , ImportBackupJSON
       , ExportBackupJSON

       , GetClientInfo

       , WalletSwaggerApi
       , swaggerWalletApi
       ) where


import           Universum

import           Control.Lens (from)
import           Control.Monad.Catch (try)
import           Data.Reflection (Reifies (..))
import           Servant.API ((:<|>), (:>), Capture, Delete, Description, Get, JSON, Post, Put,
                              QueryParam, ReqBody, Summary, Verb)
import           Servant.API.ContentTypes (NoContent, OctetStream)
import           Servant.Swagger.UI (SwaggerSchemaUI)

import           Pos.Client.Txp.Util (InputSelectionPolicy)
import           Pos.Core (Coin, SoftwareVersion)
import           Pos.Util.Servant (ApiLoggingConfig, CCapture, CQueryParam, CReqBody, DCQueryParam,
                                   DReqBody, LoggingApi, ModifiesApiRes (..),
                                   ReportDecodeError (..), VerbMod, serverHandlerL')
import           Pos.Wallet.Web.ClientTypes (Addr, CAccount, CAccountId, CAccountInit, CAccountMeta,
                                             CAddress, CCoin, CFilePath, CId, CInitialized,
                                             CPaperVendWalletRedeem, CPassPhrase, CProfile, CTx,
                                             CTxId, CTxMeta, CUpdateInfo, CWallet, CWalletInit,
                                             CWalletMeta, CWalletRedeem, ClientInfo,
                                             NewBatchPayment, ScrollLimit, ScrollOffset,
                                             SyncProgress, Wal)
import           Pos.Wallet.Web.Error (WalletError (DecodeError), catchEndpointErrors)
import           Pos.Wallet.Web.Methods.Misc (WalletStateSnapshot)

-- | Common prefix for all endpoints.
type ApiPrefix = "api"

-- | API result modification mode used here.
data WalletVerbTag

-- | Wrapper over 'Verb', which allows to catch exceptions thrown
-- by endpoints.
type WalletVerb verb = VerbMod WalletVerbTag verb

instance ModifiesApiRes WalletVerbTag where
    type ApiModifiedRes WalletVerbTag a = Either WalletError a
    modifyApiResult _ = try . catchEndpointErrors . (either throwM pure =<<)

instance ReportDecodeError (WalletVerb (Verb (mt :: k1) (st :: Nat) (ct :: [*]) a)) where
    reportDecodeError _ err = throwM (DecodeError err) ^. from serverHandlerL'

-- | Specifes servant logging config.
data WalletLoggingConfig

-- If logger config will ever be determined in runtime, 'Data.Reflection.reify'
-- can be used.
instance Reifies WalletLoggingConfig ApiLoggingConfig where
    reflect _ = "node" <> "wallet" <> "servant"

-- | Shortcut for common api result types.
type WRes verbType a = WalletVerb (verbType '[JSON] a)

-- All endpoints are defined as a separate types, for description in Swagger-based HTML-documentation.

type TestReset =
       "test"
    :> "reset"
    :> Summary "Clear wallet state and remove all secret key."
    :> WRes Post NoContent

type TestState =
       "test"
    :> "state"
    :> Summary "Print wallet state as JSON"
    :> Get '[OctetStream] WalletStateSnapshot

-------------------------------------------------------------------------
-- Wallets
-------------------------------------------------------------------------

type GetWallet =
       "wallets"
    :> Summary "Get information about a wallet by its ID (address)."
    :> Capture "walletId" (CId Wal)
    :> WRes Get CWallet

type GetWallets =
       "wallets"
    :> Summary "Get information about all available wallets."
    :> WRes Get [CWallet]

type NewWallet =
       "wallets"
    :> "new"
    :> Summary "Create a new wallet."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletInit
    :> WRes Post CWallet

type UpdateWallet =
       "wallets"
    :> Summary "Update wallet's meta information."
    :> Capture "walletId" (CId Wal)
    :> ReqBody '[JSON] CWalletMeta
    :> WRes Put CWallet

type RestoreWallet =
       "wallets"
    :> "restore"
    :> Summary "Restore existing wallet."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletInit
    :> WRes Post CWallet

type DeleteWallet =
       "wallets"
    :> Summary "Delete given wallet with all contained accounts."
    :> Capture "walletId" (CId Wal)
    :> WRes Delete NoContent

type ImportWallet =
       "wallets"
    :> "keys"
    :> Summary "Import user's secret key from the path to generate wallet."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CFilePath
    :> WRes Post CWallet

type ChangeWalletPassphrase =
       "wallets"
    :> "password"
    :> Summary "Change passphrase of given wallet."
    :> Capture "walletId" (CId Wal)
    :> DCQueryParam "old" CPassPhrase
    :> DCQueryParam "new" CPassPhrase
    :> WRes Post NoContent

-------------------------------------------------------------------------
-- Accounts
-------------------------------------------------------------------------

type GetAccount =
       "accounts"
    :> Summary "Get information about a account by its ID"
    :> CCapture "accountId" CAccountId
    :> WRes Get CAccount

type GetAccounts =
       "accounts"
    :> Summary "Get information about all available accounts."
    :> QueryParam "accountId" (CId Wal)
    :> WRes Get [CAccount]

type UpdateAccount =
       "accounts"
    :> Summary "Update account's meta information."
    :> CCapture "accountId" CAccountId
    :> ReqBody '[JSON] CAccountMeta
    :> WRes Put CAccount

type NewAccount =
       "accounts"
    :> Summary "Create a new account in given wallet."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CAccountInit
    :> WRes Post CAccount

type DeleteAccount =
       "accounts"
    :> Summary "Delete an account by ID."
    :> CCapture "accountId" CAccountId
    :> WRes Delete NoContent

-------------------------------------------------------------------------
-- Wallet addresses
-------------------------------------------------------------------------

type NewAddress =
       "addresses"
    :> Summary "Create a new address in given account."
    :> DCQueryParam "passphrase" CPassPhrase
    :> CReqBody '[JSON] CAccountId
    :> WRes Post CAddress

-------------------------------------------------------------------------
-- Addresses
-------------------------------------------------------------------------

type IsValidAddress =
       "addresses"
    :> Summary "Returns True if given address is valid, False otherwise."
    :> Capture "address" (CId Addr)  -- exact type of 'CId' shouldn't matter
    :> WRes Get Bool

-------------------------------------------------------------------------
-- Profile(s)
-------------------------------------------------------------------------

type GetProfile =
       "profile"
    :> Summary "Get user profile's meta data."
    :> WRes Get CProfile

type UpdateProfile =
       "profile"
    :> Summary "Update user profile."
    :> ReqBody '[JSON] CProfile
    :> WRes Post CProfile

-------------------------------------------------------------------------
-- Transactions
-------------------------------------------------------------------------

type NewPayment =
       "txs"
    :> "payments"
    :> Summary "Create a new payment transaction."
    :> DCQueryParam "passphrase" CPassPhrase
    :> CCapture "from" CAccountId
    :> Capture "to" (CId Addr)
    :> Capture "amount" Coin
    :> DReqBody '[JSON] (Maybe InputSelectionPolicy)
    :> WRes Post CTx

type NewPaymentBatch =
       "txs"
    :> "payments"
    :> "batch"
    :> Summary "Create a new payment transaction (can send to multiple recipients)."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] NewBatchPayment
    :> WRes Post CTx

type TxFee =
       "txs"
    :> "fee"
    :> Summary "Estimate fees for performing given transaction."
    :> Description
        "Evaluate fee which would be used for transaction created with given \
        \parameters. Note that fee may change on any operation on wallet \
        \occurs. \
        \Transaction will not be actually created."
    :> CCapture "from" CAccountId
    :> Capture "to" (CId Addr)
    :> Capture "amount" Coin
    :> DReqBody '[JSON] (Maybe InputSelectionPolicy)
    :> WRes Post CCoin

type ResetFailedPtxs =
       "txs"
    :> "resubmission"
    :> "reset"
    :> Summary "Clear the 'do not resubmit' flag from transactions that have it."
    :> Description
        "For all transactions in CPtxWontApply condition, \
        \reset them to CPtxApplying condition so that they will \
        \be passed to resubmition"
    :> WRes Get ()

type UpdateTx =
       "txs"
    :> "payments"
    :> Summary "Update payment transaction."
    :> CCapture "address" CAccountId
    :> Capture "transaction" CTxId
    :> ReqBody '[JSON] CTxMeta
    :> WRes Post NoContent

type GetHistory =
       "txs"
    :> "histories"
    :> Summary "Get the history of transactions."
    :> QueryParam "walletId" (CId Wal)
    :> CQueryParam "accountId" CAccountId
    :> QueryParam "address" (CId Addr)
    :> QueryParam "skip" ScrollOffset
    :> QueryParam "limit" ScrollLimit
    :> WRes Get ([CTx], Word)

-------------------------------------------------------------------------
-- Updates
-------------------------------------------------------------------------

type NextUpdate =
       "update"
    :> Summary "Get information about the next update."
    :> WRes Get CUpdateInfo

type PostponeUpdate =
       "update"
    :> "postpone"
    :> Summary "Postpone last update."
    :> WRes Post NoContent

type ApplyUpdate =
       "update"
    :> "apply"
    :> Summary "Apply last update."
    :> WRes Post NoContent

-------------------------------------------------------------------------
-- Redemptions
-------------------------------------------------------------------------

type RedeemADA =
       "redemptions"
    :> "ada"
    :> Summary "Redeem ADA."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletRedeem
    :> WRes Post CTx

type RedeemADAPaperVend =
       "papervend"
    :> "redemptions"
    :> "ada"
    :> Summary "Redeem ADA, paper vending."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CPaperVendWalletRedeem
    :> WRes Post CTx

-------------------------------------------------------------------------
-- Reporting
-------------------------------------------------------------------------

type ReportingInitialized =
       "reporting"
    :> "initialized"
    :> Summary "Send node's report on initialization time."
    :> ReqBody '[JSON] CInitialized
    :> WRes Post NoContent

-------------------------------------------------------------------------
-- Settings
-------------------------------------------------------------------------

type GetSlotsDuration =
       "settings"
    :> "slots"
    :> "duration"
    :> Summary "Get blockchain slot duration in milliseconds."
    :> WRes Get Word

type GetVersion =
       "settings"
    :> "version"
    :> Summary "Get current version of the node."
    :> WRes Get SoftwareVersion

type GetSyncProgress =
       "settings"
    :> "sync"
    :> "progress"
    :> Summary "Current sync progress"
    :> Description
        "Fetch info about local chain difficulty, \
        \network chain difficulty and connected peers."
    :> WRes Get SyncProgress

type LocalTimeDifference =
       "settings"
    :> "time"
    :> "difference"
    :> Summary "Get local time difference in milliseconds."
    :> WRes Get Word

-------------------------------------------------------------------------
-- JSON backup
-------------------------------------------------------------------------

type ImportBackupJSON =
       "backup"
    :> "import"
    :> Summary "Import full information about wallet from a given file."
    :> ReqBody '[JSON] CFilePath
    :> WRes Post CWallet

type ExportBackupJSON =
       "backup"
    :> "export"
    :> Summary "Export full information about wallet to a given file"
    :> Description
        "Wallet may be later restored from this file with \
        \ endpoint above."
    :> Capture "walletId" (CId Wal)
    :> ReqBody '[JSON] CFilePath
    :> WRes Post NoContent

-------------------------------------------------------------------------
-- Settings
-------------------------------------------------------------------------

type GetClientInfo =
       "info"
    :> Summary
        "Get general information about this service."
    :> WRes Get ClientInfo

-- | Servant API which provides access to wallet.
type WalletApi = ApiPrefix :> WalletApiNoPrefix

type WalletApiNoPrefix = (
     -- NOTE: enabled in prod mode https://issues.serokell.io/issue/CSM-333
     TestReset
    :<|>
     TestState
    :<|>
     -------------------------------------------------------------------------
     -- Wallets
     -------------------------------------------------------------------------
     GetWallet
    :<|>
     GetWallets
    :<|>
     NewWallet
    :<|>
     UpdateWallet
    :<|>
     RestoreWallet
    :<|>
     DeleteWallet
    :<|>
     ImportWallet
    :<|>
     ChangeWalletPassphrase
    :<|>
     -------------------------------------------------------------------------
     -- Accounts
     -------------------------------------------------------------------------
     GetAccount
    :<|>
     GetAccounts
    :<|>
     UpdateAccount
    :<|>
     NewAccount
    :<|>
     DeleteAccount
    :<|>
     -------------------------------------------------------------------------
     -- Walllet addresses
     -------------------------------------------------------------------------
     NewAddress
    :<|>
     -------------------------------------------------------------------------
     -- Addresses
     -------------------------------------------------------------------------
     IsValidAddress
    :<|>
     -------------------------------------------------------------------------
     -- Profile(s)
     -------------------------------------------------------------------------
     GetProfile
    :<|>
     UpdateProfile
    :<|>
     -------------------------------------------------------------------------
     -- Transactons
     -------------------------------------------------------------------------
     NewPayment
    :<|>
     NewPaymentBatch
    :<|>
     TxFee
    :<|>
     ResetFailedPtxs
    :<|>
     UpdateTx
    :<|>
     GetHistory
    :<|>
     -------------------------------------------------------------------------
     -- Updates
     -------------------------------------------------------------------------
     NextUpdate
    :<|>
     PostponeUpdate
    :<|>
     ApplyUpdate
    :<|>
     -------------------------------------------------------------------------
     -- Redemptions
     -------------------------------------------------------------------------
     RedeemADA
    :<|>
     RedeemADAPaperVend
    :<|>
     -------------------------------------------------------------------------
     -- Reporting
     -------------------------------------------------------------------------
     ReportingInitialized
    :<|>
     -------------------------------------------------------------------------
     -- Settings
     -------------------------------------------------------------------------
     GetSlotsDuration
    :<|>
     GetVersion
    :<|>
     GetSyncProgress
    :<|>
     LocalTimeDifference
    :<|>
     -------------------------------------------------------------------------
     -- JSON backup
     -------------------------------------------------------------------------
     ImportBackupJSON
    :<|>
     ExportBackupJSON
    :<|>
     -------------------------------------------------------------------------
     -- Client info: various versions
     -------------------------------------------------------------------------
     GetClientInfo
    )

-- | Helper Proxy.
walletApi :: Proxy WalletApi
walletApi = Proxy

-------------------------------------------------------------------------
-- Swagger
-------------------------------------------------------------------------
type SwaggerApi =
    -- this serves both: swagger.json and swagger-ui
    SwaggerSchemaUI "docs" "swagger.json"

type WalletSwaggerApi =
     LoggingApi WalletLoggingConfig WalletApi
    :<|>
     SwaggerApi

-- | Helper Proxy.
swaggerWalletApi :: Proxy WalletSwaggerApi
swaggerWalletApi = Proxy
