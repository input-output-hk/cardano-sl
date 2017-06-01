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
import           Servant.API                ((:<|>), (:>), Capture, Delete, Get, JSON,
                                             Post, Put, QueryParam, ReqBody, Verb)
import           Servant.Multipart          (MultipartForm)
import           Servant.Server             (Handler (..))
import           Universum

import           Pos.Types                  (Coin, SoftwareVersion)
import           Pos.Util.Servant           (ModifiesApiRes (..), ReportDecodeError (..),
                                             VerbMod)
import           Pos.Util.Servant           (CCapture, CQueryParam, CReqBody)
import           Pos.Wallet.Web.ClientTypes (Addr, CAccount, CAccountId, CAccountInit,
                                             CAccountMeta, CAddress, CElectronCrashReport,
                                             CId, CInitialized, CPaperVendWalletRedeem,
                                             CPassPhrase, CProfile, CTx, CTxId, CTxMeta,
                                             CUpdateInfo, CWallet, CWalletInit,
                                             CWalletRedeem, SyncProgress, Wal)
import           Pos.Wallet.Web.Error       (WalletError (DecodeError),
                                             catchEndpointErrors)

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
    reportDecodeError _ err = Handler . ExceptT . throwM $ DecodeError err

-- | Shortcut for common api result types.
type WRes verbType a = WalletVerb (verbType '[JSON] a)


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
    :> Capture "walletSetId" (CId Wal)
    :> WRes Get CWallet

type GetWalletSets =
       "wallets"
    :> "sets"
    :> WRes Get [CWallet]

type NewWalletSet =
       "wallets"
    :> "sets"
    :> "new"
    :> CQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletInit
    :> WRes Post CWallet

type RestoreWalletSet =
       "wallets"
    :> "sets"
    :> "restore"
    :> CQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletInit
    :> WRes Post CWallet

type RenameWalletSet =
       "wallets"
    :> "sets"
    :> "rename"
    :> Capture "walletSetId" (CId Wal)
    :> Capture "name" Text
    :> WRes Post CWallet

type DeleteWalletSet =
       "wallets"
    :> "sets"
    :> Capture "walletSetId" (CId Wal)
    :> WRes Delete ()

type ImportWalletSet =
       "wallets"
    :> "sets"
    :> "keys"
    :> CQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] Text
    :> WRes Post CWallet

type ChangeWalletSetPassphrase =
       "wallets"
    :> "sets"
    :> "password"
    :> Capture "walletSetId" (CId Wal)
    :> CQueryParam "old" CPassPhrase
    :> CQueryParam "new" CPassPhrase
    :> WRes Post ()

-------------------------------------------------------------------------
-- Wallets
-------------------------------------------------------------------------

type GetWallet =
       "wallets"
    :> CCapture "walletId" CAccountId
    :> WRes Get CAccount

type GetWallets =
       "wallets"
    :> QueryParam "walletSetId" (CId Wal)
    :> WRes Get [CAccount]

type UpdateWallet =
       "wallets"
    :> CCapture "walletId" CAccountId
    :> ReqBody '[JSON] CAccountMeta
    :> WRes Put CAccount

type NewWallet =
       "wallets"
    :> CQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CAccountInit
    :> WRes Post CAccount

type DeleteWallet =
       "wallets"
    :> CCapture "walletId" CAccountId
    :> WRes Delete ()

-------------------------------------------------------------------------
-- Accounts
-------------------------------------------------------------------------

type NewAccount =
       "account"
    :> CQueryParam "passphrase" CPassPhrase
    :> CReqBody '[JSON] CAccountId
    :> WRes Post CAddress

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
    :> CQueryParam "passphrase" CPassPhrase
    :> CCapture "from" CAccountId
    :> Capture "to" (CId Addr)
    :> Capture "amount" Coin
    :> WRes Post CTx

type NewPaymentExt =
       "txs"
    :> "payments"
    :> CQueryParam "passphrase" CPassPhrase
    :> CCapture "from" CAccountId
    :> Capture "to" (CId Addr)
    :> Capture "amount" Coin
    :> Capture "title" Text
    :> Capture "description" Text
    :> WRes Post CTx


type UpdateTx =
       "txs"
    :> "payments"
    :> CCapture "address" CAccountId
    :> Capture "transaction" CTxId
    :> ReqBody '[JSON] CTxMeta
    :> WRes Post ()

type GetHistory =
       "txs"
    :> "histories"
    :> CCapture "walletId" CAccountId
    :> QueryParam "skip" Word
    :> QueryParam "limit" Word
    :> WRes Get ([CTx], Word)

type SearchHistory =
       "txs"
    :> "histories"
    :> CCapture "walletId" CAccountId
    :> Capture "search" Text
    :> QueryParam "account" (CId Addr)
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
    :> CQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletRedeem
    :> WRes Post CTx

type RedeemADAPaperVend =
       "papervend"
    :> "redemptions"
    :> "ada"
    :> CQueryParam "passphrase" CPassPhrase
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
