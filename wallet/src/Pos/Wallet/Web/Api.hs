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
       , TxFee
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
import           Servant.API ((:<|>), (:>), Capture, Delete, Get, JSON, Post, Put, QueryParam,
                              ReqBody, Verb)
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
                                             CWalletMeta, CWalletRedeem, ClientInfo, ScrollLimit,
                                             ScrollOffset, SyncProgress, Wal)
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
    :> WRes Post NoContent

type TestState =
       "test"
    :> "state"
    :> Get '[OctetStream] WalletStateSnapshot

-------------------------------------------------------------------------
-- Wallets
-------------------------------------------------------------------------

type GetWallet =
       "wallets"
    :> Capture "walletId" (CId Wal)
    :> WRes Get CWallet

type GetWallets =
       "wallets"
    :> WRes Get [CWallet]

type NewWallet =
       "wallets"
    :> "new"
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletInit
    :> WRes Post CWallet

type UpdateWallet =
       "wallets"
    :> Capture "walletId" (CId Wal)
    :> ReqBody '[JSON] CWalletMeta
    :> WRes Put CWallet

type RestoreWallet =
       "wallets"
    :> "restore"
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletInit
    :> WRes Post CWallet

type DeleteWallet =
       "wallets"
    :> Capture "walletId" (CId Wal)
    :> WRes Delete NoContent

type ImportWallet =
       "wallets"
    :> "keys"
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CFilePath
    :> WRes Post CWallet

type ChangeWalletPassphrase =
       "wallets"
    :> "password"
    :> Capture "walletId" (CId Wal)
    :> DCQueryParam "old" CPassPhrase
    :> DCQueryParam "new" CPassPhrase
    :> WRes Post NoContent

-------------------------------------------------------------------------
-- Accounts
-------------------------------------------------------------------------

type GetAccount =
       "accounts"
    :> CCapture "accountId" CAccountId
    :> WRes Get CAccount

type GetAccounts =
       "accounts"
    :> QueryParam "accountId" (CId Wal)
    :> WRes Get [CAccount]

type UpdateAccount =
       "accounts"
    :> CCapture "accountId" CAccountId
    :> ReqBody '[JSON] CAccountMeta
    :> WRes Put CAccount

type NewAccount =
       "accounts"
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CAccountInit
    :> WRes Post CAccount

type DeleteAccount =
       "accounts"
    :> CCapture "accountId" CAccountId
    :> WRes Delete NoContent

-------------------------------------------------------------------------
-- Wallet addresses
-------------------------------------------------------------------------

type NewAddress =
       "addresses"
    :> DCQueryParam "passphrase" CPassPhrase
    :> CReqBody '[JSON] CAccountId
    :> WRes Post CAddress

-------------------------------------------------------------------------
-- Addresses
-------------------------------------------------------------------------

type IsValidAddress =
       "addresses"
    :> Capture "address" (CId Addr)  -- exact type of 'CId' shouldn't matter
    :> WRes Get Bool

-------------------------------------------------------------------------
-- Profile(s)
-------------------------------------------------------------------------

type GetProfile =
       "profile"
    :> WRes Get CProfile

type UpdateProfile =
       "profile"
    :> ReqBody '[JSON] CProfile
    :> WRes Post CProfile

-------------------------------------------------------------------------
-- Transactions
-------------------------------------------------------------------------

type NewPayment =
       "txs"
    :> "payments"
    :> DCQueryParam "passphrase" CPassPhrase
    :> CCapture "from" CAccountId
    :> Capture "to" (CId Addr)
    :> Capture "amount" Coin
    :> DReqBody '[JSON] (Maybe InputSelectionPolicy)
    :> WRes Post CTx

type TxFee =
       "txs"
    :> "fee"
    :> CCapture "from" CAccountId
    :> Capture "to" (CId Addr)
    :> Capture "amount" Coin
    :> DReqBody '[JSON] (Maybe InputSelectionPolicy)
    :> WRes Post CCoin

type UpdateTx =
       "txs"
    :> "payments"
    :> CCapture "address" CAccountId
    :> Capture "transaction" CTxId
    :> ReqBody '[JSON] CTxMeta
    :> WRes Post NoContent

type GetHistory =
       "txs"
    :> "histories"
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
    :> WRes Get CUpdateInfo

type PostponeUpdate =
       "update"
    :> "postpone"
    :> WRes Post NoContent

type ApplyUpdate =
       "update"
    :> "apply"
    :> WRes Post NoContent

-------------------------------------------------------------------------
-- Redemptions
-------------------------------------------------------------------------

type RedeemADA =
       "redemptions"
    :> "ada"
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletRedeem
    :> WRes Post CTx

type RedeemADAPaperVend =
       "papervend"
    :> "redemptions"
    :> "ada"
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CPaperVendWalletRedeem
    :> WRes Post CTx

-------------------------------------------------------------------------
-- Reporting
-------------------------------------------------------------------------

type ReportingInitialized =
       "reporting"
    :> "initialized"
    :> ReqBody '[JSON] CInitialized
    :> WRes Post NoContent

-------------------------------------------------------------------------
-- Settings
-------------------------------------------------------------------------

type GetSlotsDuration =
       "settings"
    :> "slots"
    :> "duration"
    :> WRes Get Word

type GetVersion =
       "settings"
    :> "version"
    :> WRes Get SoftwareVersion

type GetSyncProgress =
       "settings"
    :> "sync"
    :> "progress"
    :> WRes Get SyncProgress

type LocalTimeDifference =
       "settings"
    :> "time"
    :> "difference"
    :> WRes Get Word

-------------------------------------------------------------------------
-- JSON backup
-------------------------------------------------------------------------

type ImportBackupJSON =
       "backup"
    :> "import"
    :> ReqBody '[JSON] CFilePath
    :> WRes Post CWallet

type ExportBackupJSON =
       "backup"
    :> "export"
    :> Capture "walletId" (CId Wal)
    :> ReqBody '[JSON] CFilePath
    :> WRes Post NoContent

-------------------------------------------------------------------------
-- Settings
-------------------------------------------------------------------------

type GetClientInfo =
       "info"
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
     TxFee
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
