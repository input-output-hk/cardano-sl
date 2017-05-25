{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
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

       , GetWalletSet
       , GetWalletSets
       , NewWalletSet
       , RestoreWalletSet
       , RenameWalletSet
       , DeleteWalletSet
       , ImportWalletSet
       , ChangeWalletSetPassphrase

       , GetWallet
       , GetWallets
       , UpdateWallet
       , DeleteWallet
       , NewWallet

       , NewAccount

       , IsValidAddress

       , GetProfile
       , UpdateProfile

       , NewPayment
       , NewPaymentExt
       , UpdateTx
       , GetHistory
       , SearchHistory

       , NextUpdate
       , ApplyUpdate

       , RedeemADA
       , RedeemADAPaperVend

       , ReportingInitialized
       , ReportingElectroncrash

       , GetSlotsDuration
       , GetVersion
       , GetSyncProgress
       ) where


import           Control.Monad.Catch        (try)
import           Control.Monad.Except       (ExceptT (..))
import           Servant.API                ((:<|>), (:>), Capture, Delete, Get, JSON,
                                             Post, Put, QueryParam, ReqBody, Verb)
import           Servant.Multipart          (MultipartForm)
import           Servant.Server             (Handler (..), HasServer (..))
import           Universum

import           Pos.Types                  (Coin, SoftwareVersion)
import           Pos.Wallet.Web.ClientTypes (Acc, CAccount, CAddress,
                                             CElectronCrashReport, CInitialized,
                                             CPaperVendWalletRedeem, CPassPhrase,
                                             CProfile, CTx, CTxId, CTxMeta, CUpdateInfo,
                                             CWallet, CWalletAddress, CWalletInit,
                                             CWalletMeta, CWalletRedeem, CWalletSet,
                                             CWalletSet, CWalletSetInit, SyncProgress, WS)
import           Pos.Wallet.Web.Error       (WalletError, catchEndpointErrors)

-- | Common prefix for all endpoints.
type ApiPrefix = "api"

-- | Wrapper over 'Verb', which allows to catch exceptions thrown
-- by endpoints.
data WalletVerb verb

-- | Shortcut for common api result types.
type WRes verbType a = WalletVerb $ verbType '[JSON] (Either WalletError a)

-- TODO: shorten variables names?
instance HasServer (Verb method status content $ Either WalletError a) context =>
         HasServer (WalletVerb $ Verb method status content $ Either WalletError a) context where
    type ServerT (WalletVerb $ Verb method status content $ Either WalletError a) m =
        ServerT (Verb method status content a) m

    route _ ctx del = route verbProxy ctx (handlerCatch <$> del)
      where
        verbProxy = Proxy @(Verb method status content $ Either WalletError a)
        handlerCatch :: Handler a -> Handler (Either WalletError a)
        handlerCatch =
            Handler .
            ExceptT .
            try .
            catchEndpointErrors .
            (either throwM pure =<<) .
            runExceptT .
            runHandler'


-- All endpoints are defined as a separate types, for description in Swagger-based HTML-documentation.

type TestReset =
       "test"
    :> "reset"
    :> WRes Post ()

-------------------------------------------------------------------------
-- Wallet sets
-------------------------------------------------------------------------

type GetWalletSet =
       "wallets"
    :> "sets"
    :> Capture "walletSetId" (CAddress WS)
    :> WRes Get CWalletSet

type GetWalletSets =
       "wallets"
    :> "sets"
    :> WRes Get [CWalletSet]

type NewWalletSet =
       "wallets"
    :> "sets"
    :> "new"
    :> QueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletSetInit
    :> WRes Post CWalletSet

type RestoreWalletSet =
       "wallets"
    :> "sets"
    :> "restore"
    :> QueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletSetInit
    :> WRes Post CWalletSet

type RenameWalletSet =
       "wallets"
    :> "sets"
    :> "rename"
    :> Capture "walletSetId" (CAddress WS)
    :> Capture "name" Text
    :> WRes Post CWalletSet

type DeleteWalletSet =
       "wallets"
    :> "sets"
    :> Capture "walletSetId" (CAddress WS)
    :> WRes Delete ()

type ImportWalletSet =
       "wallets"
    :> "sets"
    :> "keys"
    :> QueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] Text
    :> WRes Post CWalletSet

type ChangeWalletSetPassphrase =
       "wallets"
    :> "sets"
    :> "password"
    :> Capture "walletSetId" (CAddress WS)
    :> QueryParam "old" CPassPhrase
    :> QueryParam "new" CPassPhrase
    :> WRes Post ()

-------------------------------------------------------------------------
-- Wallets
-------------------------------------------------------------------------

type GetWallet =
       "wallets"
    :> Capture "walletId" CWalletAddress
    :> WRes Get CWallet

type GetWallets =
       "wallets"
    :> QueryParam "walletSetId" (CAddress WS)
    :> WRes Get [CWallet]

type UpdateWallet =
       "wallets"
    :> Capture "walletId" CWalletAddress
    :> ReqBody '[JSON] CWalletMeta
    :> WRes Put CWallet

type NewWallet =
       "wallets"
    :> QueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletInit
    :> WRes Post CWallet

type DeleteWallet =
       "wallets"
    :> Capture "walletId" CWalletAddress
    :> WRes Delete ()

-------------------------------------------------------------------------
-- Accounts
-------------------------------------------------------------------------

type NewAccount =
       "account"
    :> QueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletAddress
    :> WRes Post CAccount

-------------------------------------------------------------------------
-- Addresses
-------------------------------------------------------------------------

type IsValidAddress =
       "addresses"
    :> Capture "address" Text
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
    :> QueryParam "passphrase" CPassPhrase
    :> Capture "from" CWalletAddress
    :> Capture "to" (CAddress Acc)
    :> Capture "amount" Coin
    :> WRes Post CTx

type NewPaymentExt =
       "txs"
    :> "payments"
    :> QueryParam "passphrase" CPassPhrase
    :> Capture "from" CWalletAddress
    :> Capture "to" (CAddress Acc)
    :> Capture "amount" Coin
    :> Capture "title" Text
    :> Capture "description" Text
    :> WRes Post CTx


type UpdateTx =
       "txs"
    :> "payments"
    :> Capture "address" CWalletAddress
    :> Capture "transaction" CTxId
    :> ReqBody '[JSON] CTxMeta
    :> WRes Post ()

type GetHistory =
       "txs"
    :> "histories"
    :> Capture "walletId" CWalletAddress
    :> QueryParam "skip" Word
    :> QueryParam "limit" Word
    :> WRes Get ([CTx], Word)

type SearchHistory =
       "txs"
    :> "histories"
    :> Capture "walletId" CWalletAddress
    :> Capture "search" Text
    :> QueryParam "account" (CAddress Acc)
    :> QueryParam "skip" Word
    :> QueryParam "limit" Word
    :> WRes Get ([CTx], Word)


-------------------------------------------------------------------------
-- Updates
-------------------------------------------------------------------------

type NextUpdate =
       "update"
    :> WRes Get CUpdateInfo

type ApplyUpdate =
       "update"
    :> WRes Post ()


-------------------------------------------------------------------------
-- Redemptions
-------------------------------------------------------------------------

type RedeemADA =
       "redemptions"
    :> "ada"
    :> QueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletRedeem
    :> WRes Post CTx

type RedeemADAPaperVend =
       "papervend"
    :> "redemptions"
    :> "ada"
    :> QueryParam "passphrase" CPassPhrase
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

type ReportingElectroncrash =
       "reporting"
    :> "electroncrash"
    :> MultipartForm CElectronCrashReport
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

-- | Servant API which provides access to wallet.
-- TODO: Should be composed depending on the resource - wallets, txs, ... http://haskell-servant.github.io/tutorial/0.4/server.html#nested-apis
type WalletApi = ApiPrefix :> (
     -- only works in development mode, gives 403 otherwise
     TestReset
    :<|>
     -------------------------------------------------------------------------
     -- Wallet sets
     -------------------------------------------------------------------------
     GetWalletSet
    :<|>
     GetWalletSets
    :<|>
     NewWalletSet
    :<|>
     RestoreWalletSet
    :<|>
     RenameWalletSet
    :<|>
     DeleteWalletSet
    :<|>
     ImportWalletSet
    :<|>
     ChangeWalletSetPassphrase
    :<|>
     -------------------------------------------------------------------------
     -- Wallets
     -------------------------------------------------------------------------
     GetWallet
    :<|>
     GetWallets
    :<|>
     UpdateWallet
    :<|>
     NewWallet
    :<|>
     DeleteWallet
    :<|>
     -------------------------------------------------------------------------
     -- Accounts
     -------------------------------------------------------------------------
     NewAccount
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
    -- TODO: for now we only support one2one sending. We should extend this
    -- to support many2many
     NewPaymentExt
    :<|>
      -- FIXME: Should capture the URL parameters in the payload.
     UpdateTx
    :<|>
     GetHistory
    :<|>
     SearchHistory
    :<|>
     -------------------------------------------------------------------------
     -- Updates
     -------------------------------------------------------------------------
     NextUpdate
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
     ReportingElectroncrash
    :<|>
     -------------------------------------------------------------------------
     -- Settings
     -------------------------------------------------------------------------
     GetSlotsDuration
    :<|>
     GetVersion
    :<|>
     GetSyncProgress
    )

-- | Helper Proxy.
walletApi :: Proxy WalletApi
walletApi = Proxy
