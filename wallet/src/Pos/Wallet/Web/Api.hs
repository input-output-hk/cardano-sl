{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Servant API for wallet.

module Pos.Wallet.Web.Api
       ( WalletApi
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
       , CancelApplyingPtxs
       , CancelSpecificApplyingPtx
       , UpdateTx
       , GetHistory
       , GetPendingTxsSummary

       , NextUpdate
       , PostponeUpdate
       , ApplyUpdate

       , RedeemADA
       , RedeemADAPaperVend

       , ReportingInitialized

       , GetSlotsDuration
       , GetVersion
       , GetSyncProgress

       , ImportBackupJSON
       , ExportBackupJSON

       , GetClientInfo

       , WalletSwaggerApi
       , swaggerWalletApi
       ) where


import           Control.Lens                (from)
import           Control.Monad.Catch         (try)
import           Data.Reflection             (Reifies (..))
import           Servant.API                 ((:<|>), (:>), Capture, Delete, Get, JSON,
                                              Post, Put, QueryParam, ReflectMethod (..),
                                              ReqBody, Verb)
import           Servant.API.ContentTypes    (OctetStream)
import           Servant.Server              (HasServer (..))
import           Servant.Swagger.UI          (SwaggerSchemaUI)
import           Universum

-------
import           Pos.Client.Txp.Util        (InputSelectionPolicy)
import           Pos.Types                  (Coin, SoftwareVersion)
import           Pos.Util.Servant           (ApiLoggingConfig, CCapture, CQueryParam,
                                             CReqBody, DCQueryParam, DReqBody,
                                             HasLoggingServer (..), LoggingApi,
                                             ModifiesApiRes (..), ReportDecodeError (..),
                                             VerbMod, WithTruncatedLog (..),
                                             applyLoggingToHandler, inRouteServer,
                                             serverHandlerL')
import           Pos.Wallet.Web.ClientTypes (Addr, CAccount, CAccountId, CAccountInit,
                                             CAccountMeta, CAddress, CCoin, CFilePath, ClientInfo,
                                             CId, CInitialized, CPaperVendWalletRedeem,
                                             CPassPhrase, CProfile, CTx, CTxId, CTxMeta,
                                             CUpdateInfo, CWallet, CWalletInit,
                                             CWalletMeta, CWalletRedeem, ScrollLimit, SinceTime,
                                             ScrollOffset, NewBatchPayment,
                                             SyncProgress, Wal)
import           Pos.Wallet.Web.Error       (WalletError (DecodeError),
                                             catchEndpointErrors)
import           Pos.Wallet.Web.Methods.Misc (PendingTxsSummary, WalletStateSnapshot)

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

instance ( HasServer (Verb mt st ct $ ApiModifiedRes WalletVerbTag a) ctx
         , Reifies config ApiLoggingConfig
         , ReflectMethod mt
         , Buildable (WithTruncatedLog a)
         ) =>
         HasLoggingServer config (WalletVerb (Verb (mt :: k1) (st :: Nat) (ct :: [*]) a)) ctx where
    routeWithLog =
        -- TODO [CSM-466] avoid manually rewriting rule for composite api modification
        inRouteServer @(Verb mt st ct $ ApiModifiedRes WalletVerbTag a) route $
        \(paramsInfo, handler) ->
            handler & serverHandlerL' %~ modifyApiResult (Proxy @WalletVerbTag)
                    & applyLoggingToHandler (Proxy @config) (Proxy @mt) . (paramsInfo, )

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
    :> WRes Post ()

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
    :> WRes Delete ()

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
    :> WRes Post ()

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
    :> WRes Delete ()

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

type NewPaymentBatch =
       "txs"
    :> "payments"
    :> "batch"
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] NewBatchPayment
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
    :> WRes Post ()

type CancelApplyingPtxs =
       "txs"
    :> "resubmission"
    :> "cancel"
    :> WRes Post ()

type CancelSpecificApplyingPtx =
       "txs"
    :> "resubmission"
    :> "cancelsingle"
    :> Capture "transaction" CTxId
    :> WRes Post ()

type GetHistory =
       "txs"
    :> "histories"
    :> QueryParam "walletId" (CId Wal)
    :> CQueryParam "accountId" CAccountId
    :> QueryParam "address" (CId Addr)
    :> QueryParam "since" SinceTime
    :> QueryParam "skip" ScrollOffset
    :> QueryParam "limit" ScrollLimit
    :> WRes Get ([CTx], Word)

type GetPendingTxsSummary =
        "txs"
    :> "pending"
    :> "summary"
    :> WRes Get [PendingTxsSummary]

-------------------------------------------------------------------------
-- Updates
-------------------------------------------------------------------------

type NextUpdate =
       "update"
    :> WRes Get CUpdateInfo

type PostponeUpdate =
       "update"
    :> "postpone"
    :> WRes Post ()

type ApplyUpdate =
       "update"
    :> "apply"
    :> WRes Post ()

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
    :> WRes Post ()

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
    :> WRes Post ()

-------------------------------------------------------------------------
-- Settings
-------------------------------------------------------------------------

type GetClientInfo =
       "info"
    :> WRes Get ClientInfo

-- | Servant API which provides access to wallet.
-- TODO: Should be composed depending on the resource - wallets, txs, ... http://haskell-servant.github.io/tutorial/0.4/server.html#nested-apis
type WalletApi = ApiPrefix :> (
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
     -- TODO: A single profile? Should be possible in the future to have
     -- multiple profiles?
     GetProfile
    :<|>
     UpdateProfile
    :<|>
     -------------------------------------------------------------------------
     -- Transactons
     -------------------------------------------------------------------------
    -- TODO: for now we only support one2one sending. We should extend this
    -- to support many2many
     NewPayment
    :<|>
     NewPaymentBatch
    :<|>
     TxFee
    :<|>
     CancelApplyingPtxs
    :<|>
     CancelSpecificApplyingPtx
    :<|>
      -- FIXME: Should capture the URL parameters in the payload.
     UpdateTx
    :<|>
     GetHistory
    :<|>
     GetPendingTxsSummary
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
