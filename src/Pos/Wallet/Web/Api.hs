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

       , GetWallet
       , GetWallets
       , NewWallet
       , RestoreWallet
       , RenameWallet
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
       , UpdateTx
       , GetHistory

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
import           Pos.Util.Servant           (CCapture, CQueryParam, CReqBody,
                                             DCQueryParam, ModifiesApiRes (..),
                                             ReportDecodeError (..), VerbMod)
import           Pos.Wallet.Web.ClientTypes (Addr, CAccount, CAccountId, CAccountInit, CCoin,
                                             CAccountMeta, CAddress, CElectronCrashReport,
                                             CId, CInitialized, CPaperVendWalletRedeem,
                                             CPassPhrase, CProfile, CTx, CTxId, CTxMeta,
                                             CUpdateInfo, CWallet, CWalletInit,
                                             CWalletMeta, CWalletRedeem, SyncProgress,
                                             Wal)
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

type RenameWallet =
       "wallets"
    :> "rename"
    :> Capture "walletId" (CId Wal)
    :> Capture "name" Text
    :> WRes Post CWallet

type DeleteWallet =
       "wallets"
    :> Capture "walletId" (CId Wal)
    :> WRes Delete ()

type ImportWallet =
       "wallets"
    :> "keys"
    :> DCQueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] Text
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
    :> DCQueryParam "passphrase" CPassPhrase
    :> CCapture "from" CAccountId
    :> Capture "to" (CId Addr)
    :> Capture "amount" Coin
    :> WRes Post CTx

type TxFee =
       "txs"
    :> "fee"
    :> DCQueryParam "passphrase" CPassPhrase
    :> CCapture "from" CAccountId
    :> Capture "to" (CId Addr)
    :> Capture "amount" Coin
    :> WRes Get CCoin

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
    :> QueryParam "walletId" (CId Wal)
    :> CQueryParam "accountId" CAccountId
    :> QueryParam "address" (CId Addr)
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
     RenameWallet
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
     TxFee
    :<|>
      -- FIXME: Should capture the URL parameters in the payload.
     UpdateTx
    :<|>
     GetHistory
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
