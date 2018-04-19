{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Servant API for wallet.

module Pos.Wallet.Web.Api
       (
       -- * Wallet API
         WalletApi
       , walletApi
       , WalletApiNoPrefix
       , WalletApiRecord(..)
       -- ** Branches of the API
       , WTestApi        , WTestApiRecord(..)
       , WWalletsApi     , WWalletsApiRecord(..)
       , WAccountsApi    , WAccountsApiRecord(..)
       , WAddressesApi   , WAddressesApiRecord(..)
       , WProfileApi     , WProfileApiRecord(..)
       , WTxsApi         , WTxsApiRecord(..)
       , WUpdateApi      , WUpdateApiRecord(..)
       , WRedemptionsApi , WRedemptionsApiRecord(..)
       , WReportingApi   , WReportingApiRecord(..)
       , WSettingsApi    , WSettingsApiRecord(..)
       , WBackupApi      , WBackupApiRecord(..)
       , WInfoApi        , WInfoApiRecord(..)
       , WSystemApi      , WSystemApiRecord(..)
       -- * Types for particular endpoints for benchmarking.
       , GetAccounts
       , GetHistory
       , GetSyncProgress
       , GetWallet
       , GetWallets
       , NewAddress
       , IsValidAddress
       , NewPayment
       , NewWallet
       -- ** Something
       , WalletVerb

       -- * Swagger API

       , WalletSwaggerApi
       , swaggerWalletApi
       ) where

import           Universum

import           Control.Exception.Safe (try)
import           Control.Lens (from)
import           Data.Reflection (Reifies (..))
import           Servant.API ((:<|>), (:>), Capture, Delete, Description, Get, JSON, Post, Put,
                              QueryParam, ReqBody, Summary, Verb)
import           Servant.API.ContentTypes (NoContent, OctetStream)
import           Servant.Generic ((:-), AsApi, ToServant)
import           Servant.Swagger.UI (SwaggerSchemaUI)

import           Pos.Client.Txp.Util (InputSelectionPolicy)
import           Pos.Core (Coin, SoftwareVersion)
import           Pos.Util.Servant (ApiLoggingConfig (..), CCapture, CQueryParam, CReqBody,
                                   DCQueryParam, DReqBody, LoggingApi, ModifiesApiRes (..),
                                   ReportDecodeError (..), VerbMod, serverHandlerL')
import           Pos.Wallet.Web.ClientTypes (Addr, CAccount, CAccountId, CAccountInit, CAccountMeta,
                                             CAddress, CCoin, CFilePath, CId, CInitialized,
                                             CPaperVendWalletRedeem, CPassPhrase, CProfile, CTx,
                                             CTxId, CUpdateInfo, CWallet, CWalletInit, CWalletMeta,
                                             CWalletRedeem, ClientInfo, NewBatchPayment,
                                             ScrollLimit, ScrollOffset, SyncProgress, Wal)
import           Pos.Wallet.Web.Error (WalletError (DecodeError), catchEndpointErrors)
import           Pos.Wallet.Web.Methods.Misc (PendingTxsSummary, WalletStateSnapshot)

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
    reflect _ = ApiLoggingConfig ("node" <> "wallet" <> "servant")

-- | Shortcut for common api result types.
type WRes verbType a = WalletVerb (verbType '[JSON] a)

-------------------------------------------------------------------------
-- Swagger API
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

----------------------------------------------------------------------------
-- Wallet API
----------------------------------------------------------------------------

-- | Servant API which provides access to wallet.
type WalletApi = "api" :> WalletApiNoPrefix

type WalletApiNoPrefix = ToServant (WalletApiRecord AsApi)

data WalletApiRecord route = WalletApiRecord
  { _test        :: route :- WTestApi             -- /test
  , _wallets     :: route :- WWalletsApi          -- /wallets
  , _accounts    :: route :- WAccountsApi         -- /accounts
  , _addresses   :: route :- WAddressesApi        -- /addresses
  , _profile     :: route :- WProfileApi          -- /profile
  , _txs         :: route :- WTxsApi              -- /txs
  , _update      :: route :- WUpdateApi           -- /update
  , _redemptions :: route :- WRedemptionsApi      -- /redemptions +
                                                  --   /papervend/redemptions
  , _reporting   :: route :- WReportingApi        -- /reporting
  , _settings    :: route :- WSettingsApi         -- /settings
  , _backup      :: route :- WBackupApi           -- /backup
  , _info        :: route :- WInfoApi             -- /info
  , _system      :: route :- WSystemApi           -- /system
  }
  deriving (Generic)

-- | Helper Proxy.
walletApi :: Proxy WalletApi
walletApi = Proxy

-- ~~~~~~~~~~
--   /test
-- ~~~~~~~~~~

-- | The "/test" branch of the API
type WTestApi = "test" :> ToServant (WTestApiRecord AsApi)

data WTestApiRecord route = WTestApiRecord
  {
    -- NOTE: enabled in prod mode https://issues.serokell.io/issue/CSM-333
    _testReset :: route
    :- "reset"
    :> Summary "Clear wallet state and remove all secret key."
    :> WRes Post NoContent

  , _testState :: route
    :- "state"
    :> Summary "Print wallet state as JSON"
    :> Get '[OctetStream] WalletStateSnapshot
  }
  deriving (Generic)

-- ~~~~~~~~~~
--   /wallets
-- ~~~~~~~~~~

-- | The "/wallets" branch of the API
type WWalletsApi = "wallets" :> ToServant (WWalletsApiRecord AsApi)

type GetWallet =
    Summary "Get information about a wallet by its ID (address)."
    :> Capture "walletId" (CId Wal)
    :> WRes Get CWallet

type GetWallets =
    Summary "Get information about all available wallets."
    :> WRes Get [CWallet]

type NewWallet =
    "new"
    :> Summary "Create a new wallet."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletInit
    :> WRes Post CWallet

data WWalletsApiRecord route = WWalletsApiRecord
  {
    _getWallet :: route
    :- GetWallet

  , _getWallets :: route
    :- GetWallets

  , _newWallet :: route
    :- NewWallet

  , _updateWallet :: route
    :- Summary "Update wallet's meta information."
    :> Capture "walletId" (CId Wal)
    :> ReqBody '[JSON] CWalletMeta
    :> WRes Put CWallet

  , _restoreWallet :: route
    :- "restore"
    :> Summary "Restore existing wallet."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletInit
    :> WRes Post CWallet

  , _deleteWallet :: route
    :- Summary "Delete given wallet with all contained accounts."
    :> Capture "walletId" (CId Wal)
    :> WRes Delete NoContent

  , _importWallet :: route
    :- "keys"
    :> Summary "Import user's secret key from the path to generate wallet."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CFilePath
    :> WRes Post CWallet

  , _changeWalletPassphrase :: route
    :- "password"
    :> Summary "Change passphrase of given wallet."
    :> Capture "walletId" (CId Wal)
    :> DCQueryParam "old" CPassPhrase
    :> DCQueryParam "new" CPassPhrase
    :> WRes Post NoContent
  }
  deriving (Generic)

-- ~~~~~~~~~~
--   /accounts
-- ~~~~~~~~~~

-- | The "/accounts" branch of the API
type WAccountsApi = "accounts" :> ToServant (WAccountsApiRecord AsApi)

type GetAccounts =
    Summary "Get information about all available accounts."
    :> QueryParam "accountId" (CId Wal)
    :> WRes Get [CAccount]

data WAccountsApiRecord route = WAccountsApiRecord
  {
    _getAccount :: route
    :- Summary "Get information about a account by its ID"
    :> CCapture "accountId" CAccountId
    :> WRes Get CAccount

  , _getAccounts :: route
    :- GetAccounts

  , _updateAccount :: route
    :- Summary "Update account's meta information."
    :> CCapture "accountId" CAccountId
    :> ReqBody '[JSON] CAccountMeta
    :> WRes Put CAccount

  , _newAccount :: route
    :- Summary "Create a new account in given wallet."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CAccountInit
    :> WRes Post CAccount

  , _deleteAccount :: route
    :- Summary "Delete an account by ID."
    :> CCapture "accountId" CAccountId
    :> WRes Delete NoContent
  }
  deriving (Generic)

-- ~~~~~~~~~~
--   /addresses
-- ~~~~~~~~~~

-- | The "/addresses" branch of the API
type WAddressesApi = "addresses" :> ToServant (WAddressesApiRecord AsApi)

type NewAddress =
    Summary "Create a new address in given account."
    :> DCQueryParam "passphrase" CPassPhrase
    :> CReqBody '[JSON] CAccountId
    :> WRes Post CAddress

type IsValidAddress =
    Summary "Returns True if given address is valid, False otherwise."
    :> Capture "address" (CId Addr)  -- exact type of 'CId' shouldn't matter
    :> WRes Get Bool

data WAddressesApiRecord route = WAddressesApiRecord
  {
    _newAddress :: route
    :- NewAddress

  , _isValidAddress :: route
    :- IsValidAddress
  }
  deriving (Generic)

-- ~~~~~~~~~~
--   /profile
-- ~~~~~~~~~~

-- | The "/profile" branch of the API
type WProfileApi = "profile" :> ToServant (WProfileApiRecord AsApi)

data WProfileApiRecord route = WProfileApiRecord
  {
    _getProfile :: route
    :- Summary "Get user profile's meta data."
    :> WRes Get CProfile

  , _updateProfile :: route
    :- Summary "Update user profile."
    :> ReqBody '[JSON] CProfile
    :> WRes Post CProfile
  }
  deriving (Generic)

-- ~~~~~~~~~~
--   /txs
-- ~~~~~~~~~~

-- | The "/txs" branch of the API
type WTxsApi = "txs" :> ToServant (WTxsApiRecord AsApi)

type NewPayment =
    "payments"
    :> Summary "Create a new payment transaction."
    :> DCQueryParam "passphrase" CPassPhrase
    :> CCapture "from" CAccountId
    :> Capture "to" (CId Addr)
    :> Capture "amount" Coin
    :> DReqBody '[JSON] (Maybe InputSelectionPolicy)
    :> WRes Post CTx

type GetHistory =
    "histories"
    :> Summary "Get the history of transactions."
    :> QueryParam "walletId" (CId Wal)
    :> CQueryParam "accountId" CAccountId
    :> QueryParam "address" (CId Addr)
    :> QueryParam "skip" ScrollOffset
    :> QueryParam "limit" ScrollLimit
    :> WRes Get ([CTx], Word)

data WTxsApiRecord route = WTxsApiRecord
  {
    _newPayment :: route
    :- NewPayment

  , _newPaymentBatch :: route
    :- "payments"
    :> "batch"
    :> Summary "Create a new payment transaction \
               \(can send to multiple recipients)."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] NewBatchPayment
    :> WRes Post CTx

  , _txFee :: route
    :- "fee"
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

  , _resetFailedPtxs :: route
    :- "resubmission"
    :> "reset"
    :> Summary "Clear the 'do not resubmit' flag from transactions \
               \that have it."
    :> Description
        "For all transactions in CPtxWontApply condition, \
        \reset them to CPtxApplying condition so that they will \
        \be passed to resubmition"
    :> WRes Get NoContent

  , _cancelApplyingPtxs :: route
    :- "resubmission"
    :> "cancel"
    :> Summary "Cancel all transactions in CPtxApplying condition (unconfirmed)."
    :> WRes Post NoContent

  , _cancelSpecificApplyingPtx :: route
    :- "resubmission"
    :> "cancelsingle"
    :> Capture "transaction" CTxId
    :> Summary "Cancel specific transaction in CPtxApplying condition."
    :> WRes Post NoContent

  , _getHistory :: route
    :- GetHistory

  , _pendingSummary :: route
    :- "pending"
    :> "summary"
    :> Summary "Get the pending tx summaries."
    :> WRes Get [PendingTxsSummary]
  }
  deriving (Generic)

-- ~~~~~~~~~~
--   /update
-- ~~~~~~~~~~

-- | The "/update" branch of the API
type WUpdateApi = "update" :> ToServant (WUpdateApiRecord AsApi)

data WUpdateApiRecord route = WUpdateApiRecord
  {
    _nextUpdate :: route
    :- Summary "Get information about the next update."
    :> WRes Get CUpdateInfo

  , _postponeUpdate :: route
    :- "postpone"
    :> Summary "Postpone last update."
    :> WRes Post NoContent

  , _applyUpdate :: route
    :- "apply"
    :> Summary "Apply last update."
    :> WRes Post NoContent
  }
  deriving (Generic)

-- ~~~~~~~~~~
--   /redemptions and /papervend/redemptions
-- ~~~~~~~~~~

-- | The "\/redemptions" and "\/papervend\/redemptions" branches of the API
type WRedemptionsApi = ToServant (WRedemptionsApiRecord AsApi)

data WRedemptionsApiRecord route = WRedemptionsApiRecord
  {
    _redeemADA :: route
    :- "redemptions"
    :> "ada"
    :> Summary "Redeem ADA."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletRedeem
    :> WRes Post CTx

  , _redeemADAPaperVend :: route
    :- "papervend"
    :> "redemptions"
    :> "ada"
    :> Summary "Redeem ADA, paper vending."
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CPaperVendWalletRedeem
    :> WRes Post CTx
  }
  deriving (Generic)

-- ~~~~~~~~~~
--   /reporting
-- ~~~~~~~~~~

-- | The "/reporting" branch of the API
type WReportingApi = "reporting" :> ToServant (WReportingApiRecord AsApi)

data WReportingApiRecord route = WReportingApiRecord
  {
    _reportingInitialized :: route
    :- "initialized"
    :> Summary "Send node's report on initialization time."
    :> ReqBody '[JSON] CInitialized
    :> WRes Post NoContent
  }
  deriving (Generic)

-- ~~~~~~~~~~
--   /settings
-- ~~~~~~~~~~

-- | The "/settings" branch of the API
type WSettingsApi = "settings" :> ToServant (WSettingsApiRecord AsApi)

type GetSyncProgress =
    "sync"
    :> "progress"
    :> Summary "Current sync progress"
    :> Description
        "Fetch info about local chain difficulty, \
        \network chain difficulty and connected peers."
    :> WRes Get SyncProgress

data WSettingsApiRecord route = WSettingsApiRecord
  {
    _getSlotsDuration :: route
    :- "slots"
    :> "duration"
    :> Summary "Get blockchain slot duration in milliseconds."
    :> WRes Get Word

  , _getVersion :: route
    :- "version"
    :> Summary "Get current version of the node."
    :> WRes Get SoftwareVersion

  , _getSyncProgress :: route
    :- GetSyncProgress

  , _localTimeDifference :: route
    :- "time"
    :> "difference"
    :> Summary "Get local time difference in microseconds."
    :> WRes Get Integer
  }
  deriving (Generic)

-- ~~~~~~~~~~
--   /backup (JSON backup)
-- ~~~~~~~~~~

-- | The "/backup" branch of the API
type WBackupApi = "backup" :> ToServant (WBackupApiRecord AsApi)

data WBackupApiRecord route = WBackupApiRecord
  {
    _importBackupJSON :: route
    :- "import"
    :> Summary "Import full information about wallet from a given file."
    :> ReqBody '[JSON] CFilePath
    :> WRes Post CWallet

  , _exportBackupJSON :: route
    :- "export"
    :> Summary "Export full information about wallet to a given file"
    :> Description
        "Wallet may be later restored from this file with \
        \ endpoint above."
    :> Capture "walletId" (CId Wal)
    :> ReqBody '[JSON] CFilePath
    :> WRes Post NoContent
  }
  deriving (Generic)

-- ~~~~~~~~~~
--   /system
-- ~~~~~~~~~~

type WSystemApi = "system" :> ToServant (WSystemApiRecord AsApi)

data WSystemApiRecord route = WSystemApiRecord
  {
    _requestShutdown :: route
    :- "shutdown"
    :> Summary "Request a shutdown from node."
    :> WRes Post NoContent
  }
  deriving (Generic)

-- ~~~~~~~~~~
--   /info (client info)
-- ~~~~~~~~~~

-- | The "/info" branch of the API
type WInfoApi = "info" :> ToServant (WInfoApiRecord AsApi)

data WInfoApiRecord route = WInfoApiRecord
  {
    _getClientInfo :: route
    :- Summary "Get general information about this service."
    :> WRes Get ClientInfo
  }
  deriving (Generic)
