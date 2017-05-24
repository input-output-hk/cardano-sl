{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

-- | Servant API for wallet.

module Pos.Wallet.Web.Api
       ( WalletApi
       , walletApi

       , ApiPrefix

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


import           Servant.API                ((:<|>), (:>), Capture, Delete, Get, JSON,
                                             Post, Put, QueryParam, ReqBody)
import           Servant.Multipart          (MultipartForm)
import           Universum

import           Pos.Types                  (Coin, SoftwareVersion)
import           Pos.Wallet.Web.ClientTypes (Acc, CAccount, CAddress,
                                             CElectronCrashReport, CInitialized,
                                             CPaperVendWalletRedeem, CPassPhrase,
                                             CProfile, CTx, CTxId, CTxMeta, CUpdateInfo,
                                             CWallet, CWalletAddress, CWalletInit,
                                             CWalletMeta, CWalletRedeem, CWalletSet,
                                             CWalletSet, CWalletSetInit, SyncProgress, WS)
import           Pos.Wallet.Web.Error       (WalletError)

-- | Common prefix for all endpoints.
type ApiPrefix = "api"

-- | All endpoints are defined as a separate types, for description in Swagger-based HTML-documentation.

type TestReset =
       "test"
    :> "reset"
    :> Post '[JSON] (Either WalletError ())

-------------------------------------------------------------------------
-- Wallet sets
-------------------------------------------------------------------------

type GetWalletSet =
       "wallets"
    :> "sets"
    :> Capture "walletSetId" (CAddress WS)
    :> Get '[JSON] (Either WalletError CWalletSet)

type GetWalletSets =
       "wallets"
    :> "sets"
    :> Get '[JSON] (Either WalletError [CWalletSet])

type NewWalletSet =
       "wallets"
    :> "sets"
    :> "new"
    :> QueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletSetInit
    :> Post '[JSON] (Either WalletError CWalletSet)

type RestoreWalletSet =
       "wallets"
    :> "sets"
    :> "restore"
    :> QueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletSetInit
    :> Post '[JSON] (Either WalletError CWalletSet)

type RenameWalletSet =
       "wallets"
    :> "sets"
    :> "rename"
    :> Capture "walletSetId" (CAddress WS)
    :> Capture "name" Text
    :> Post '[JSON] (Either WalletError CWalletSet)

type DeleteWalletSet =
       "wallets"
    :> "sets"
    :> Capture "walletSetId" (CAddress WS)
    :> Delete '[JSON] (Either WalletError ())

type ImportWalletSet =
       "wallets"
    :> "sets"
    :> "keys"
    :> QueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] Text
    :> Post '[JSON] (Either WalletError CWalletSet)

type ChangeWalletSetPassphrase =
       "wallets"
    :> "sets"
    :> "password"
    :> Capture "walletSetId" (CAddress WS)
    :> QueryParam "old" CPassPhrase
    :> QueryParam "new" CPassPhrase
    :> Post '[JSON] (Either WalletError ())

-------------------------------------------------------------------------
-- Wallets
-------------------------------------------------------------------------

type GetWallet =
       "wallets"
    :> Capture "walletId" CWalletAddress
    :> Get '[JSON] (Either WalletError CWallet)

type GetWallets =
       "wallets"
    :> QueryParam "walletSetId" (CAddress WS)
    :> Get '[JSON] (Either WalletError [CWallet])

type UpdateWallet =
       "wallets"
    :> Capture "walletId" CWalletAddress
    :> ReqBody '[JSON] CWalletMeta
    :> Put '[JSON] (Either WalletError CWallet)

type NewWallet =
       "wallets"
    :> QueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletInit
    :> Post '[JSON] (Either WalletError CWallet)

type DeleteWallet =
       "wallets"
    :> Capture "walletId" CWalletAddress
    :> Delete '[JSON] (Either WalletError ())

-------------------------------------------------------------------------
-- Accounts
-------------------------------------------------------------------------

type NewAccount =
       "account"
    :> QueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletAddress
    :> Post '[JSON] (Either WalletError CAccount)

-------------------------------------------------------------------------
-- Addresses
-------------------------------------------------------------------------

type IsValidAddress =
       "addresses"
    :> Capture "address" Text
    :> Get '[JSON] (Either WalletError Bool)

-------------------------------------------------------------------------
-- Profile(s)
-------------------------------------------------------------------------

type GetProfile =
       "profile"
    :> Get '[JSON] (Either WalletError CProfile)

type UpdateProfile =
       "profile"
    :> ReqBody '[JSON] CProfile
    :> Post '[JSON] (Either WalletError CProfile)


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
    :> Post '[JSON] (Either WalletError CTx)

type NewPaymentExt =
       "txs"
    :> "payments"
    :> QueryParam "passphrase" CPassPhrase
    :> Capture "from" CWalletAddress
    :> Capture "to" (CAddress Acc)
    :> Capture "amount" Coin
    :> Capture "title" Text
    :> Capture "description" Text
    :> Post '[JSON] (Either WalletError CTx)


type UpdateTx =
       "txs"
    :> "payments"
    :> Capture "address" CWalletAddress
    :> Capture "transaction" CTxId
    :> ReqBody '[JSON] CTxMeta
    :> Post '[JSON] (Either WalletError ())

type GetHistory =
       "txs"
    :> "histories"
    :> Capture "walletId" CWalletAddress
    :> QueryParam "skip" Word
    :> QueryParam "limit" Word
    :> Get '[JSON] (Either WalletError ([CTx], Word))

type SearchHistory =
       "txs"
    :> "histories"
    :> Capture "walletId" CWalletAddress
    :> Capture "search" Text
    :> QueryParam "account" (CAddress Acc)
    :> QueryParam "skip" Word
    :> QueryParam "limit" Word
    :> Get '[JSON] (Either WalletError ([CTx], Word))


-------------------------------------------------------------------------
-- Updates
-------------------------------------------------------------------------

type NextUpdate =
       "update"
    :> Get '[JSON] (Either WalletError CUpdateInfo)

type ApplyUpdate =
       "update"
    :> Post '[JSON] (Either WalletError ())


-------------------------------------------------------------------------
-- Redemptions
-------------------------------------------------------------------------

type RedeemADA =
       "redemptions"
    :> "ada"
    :> QueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletRedeem
    :> Post '[JSON] (Either WalletError CTx)

type RedeemADAPaperVend =
       "papervend"
    :> "redemptions"
    :> "ada"
    :> QueryParam "passphrase" CPassPhrase
    :> ReqBody '[JSON] CPaperVendWalletRedeem
    :> Post '[JSON] (Either WalletError CTx)


-------------------------------------------------------------------------
-- Reporting
-------------------------------------------------------------------------

type ReportingInitialized =
       "reporting"
    :> "initialized"
    :> ReqBody '[JSON] CInitialized
    :> Post '[JSON] (Either WalletError ())

type ReportingElectroncrash =
       "reporting"
    :> "electroncrash"
    :> MultipartForm CElectronCrashReport
    :> Post '[JSON] (Either WalletError ())


-------------------------------------------------------------------------
-- Settings
-------------------------------------------------------------------------

type GetSlotsDuration =
       "settings"
    :> "slots"
    :> "duration"
    :> Get '[JSON] (Either WalletError Word)

type GetVersion =
       "settings"
    :> "version"
    :> Get '[JSON] (Either WalletError SoftwareVersion)

type GetSyncProgress =
       "settings"
    :> "sync"
    :> "progress"
    :> Get '[JSON] (Either WalletError SyncProgress)

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
