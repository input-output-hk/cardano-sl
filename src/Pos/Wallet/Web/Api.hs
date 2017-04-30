{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

-- | Servant API for wallet.

module Pos.Wallet.Web.Api
       ( WalletApi
       , walletApi
       , TestReset
       , GetWallet 
       , GetWallets
       , UpdateWallet
       , DeleteWallet
       , ImportKey
       , WalletRestore
       , NewWallet
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
import           Pos.Wallet.Web.ClientTypes (CAddress, CCurrency, CElectronCrashReport,
                                             CInitialized, CPaperVendWalletRedeem,
                                             CPassPhrase, CProfile, CTx, CTxId, CTxMeta,
                                             CUpdateInfo, CWallet, CWalletInit,
                                             CWalletMeta, CWalletRedeem, SyncProgress)
import           Pos.Wallet.Web.Error       (WalletError)

-- | Common prefix for all endpoints.
type API = "api"

-- | All endpoints are defined as a separate types, for description in Swagger-based HTML-documentation.

type TestReset = API
    :> "test"
    :> "reset"
    :> Post '[JSON] (Either WalletError ())

type GetWallet = API
    :> "wallets"
    :> Capture "walletId" CAddress
    :> Get '[JSON] (Either WalletError CWallet)

type GetWallets = API
    :> "wallets"
    :> Get '[JSON] (Either WalletError [CWallet])

type UpdateWallet = API
    :> "wallets"
    :> Capture "walletId" CAddress
    :> ReqBody '[JSON] CWalletMeta
    :> Put '[JSON] (Either WalletError CWallet)

type DeleteWallet = API
    :> "wallets"
    :> Capture "walletId" CAddress
    :> Delete '[JSON] (Either WalletError ())

type ImportKey = API
    :> "wallets"
    :> "keys"
    :> ReqBody '[JSON] Text
    :> Post '[JSON] (Either WalletError CWallet)

type WalletRestore = API
    :> "wallets"
    :> "restore"
    :> Capture "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletInit
    :> Post '[JSON] (Either WalletError CWallet)

type NewWallet = API
    :> "wallets"
    :> Capture "passphrase" CPassPhrase
    :> ReqBody '[JSON] CWalletInit
    :> Post '[JSON] (Either WalletError CWallet)

type IsValidAddress = API
    :> "addresses"
    :> Capture "address" Text
    :> "currencies"
    :> Capture "currency" CCurrency
    :> Get '[JSON] (Either WalletError Bool)

type GetProfile = API
    :> "profile"
    :> Get '[JSON] (Either WalletError CProfile)

type UpdateProfile = API
    :> "profile"
    :> ReqBody '[JSON] CProfile
    :> Post '[JSON] (Either WalletError CProfile)

type NewPayment = API
    :> "txs"
    :> "payments"
    :> Capture "passphrase" CPassPhrase
    :> Capture "from" CAddress
    :> Capture "to" CAddress
    :> Capture "amount" Coin
    :> Post '[JSON] (Either WalletError CTx)

type NewPaymentExt = API
    :> "txs"
    :> "payments"
    :> Capture "passphrase" CPassPhrase
    :> Capture "from" CAddress
    :> Capture "to" CAddress
    :> Capture "amount" Coin
    :> Capture "currency" CCurrency
    :> Capture "title" Text
    :> Capture "description" Text
    :> Post '[JSON] (Either WalletError CTx)

type UpdateTx = API
    :> "txs"
    :> "payments"
    :> Capture "address" CAddress
    :> Capture "transaction" CTxId
    :> ReqBody '[JSON] CTxMeta
    :> Post '[JSON] (Either WalletError ())

type GetHistory = API
    :> "txs"
    :> "histories"
    :> Capture "address" CAddress
    :> QueryParam "skip" Word
    :> QueryParam "limit" Word
    :> Get '[JSON] (Either WalletError ([CTx], Word))

type SearchHistory = API
    :> "txs"
    :> "histories"
    :> Capture "address" CAddress
    :> Capture "search" Text
    :> QueryParam "skip" Word
    :> QueryParam "limit" Word
    :> Get '[JSON] (Either WalletError ([CTx], Word))

type NextUpdate = API
    :> "update"
    :> Get '[JSON] (Either WalletError CUpdateInfo)

type ApplyUpdate = API
    :> "update"
    :> Post '[JSON] (Either WalletError ())

type RedeemADA = API
    :> "redemptions"
    :> "ada"
    :> ReqBody '[JSON] CWalletRedeem
    :> Post '[JSON] (Either WalletError CTx)

type RedeemADAPaperVend = API
    :> "papervend"
    :> "redemptions"
    :> "ada"
    :> ReqBody '[JSON] CPaperVendWalletRedeem
    :> Post '[JSON] (Either WalletError CTx)

type ReportingInitialized = API
    :> "reporting"
    :> "initialized"
    :> ReqBody '[JSON] CInitialized
    :> Post '[JSON] (Either WalletError ())

type ReportingElectroncrash = API
    :> "reporting"
    :> "electroncrash"
    :> MultipartForm CElectronCrashReport
    :> Post '[JSON] (Either WalletError ())

type GetSlotsDuration = API
    :> "settings"
    :> "slots"
    :> "duration"
    :> Get '[JSON] (Either WalletError Word)

type GetVersion = API
    :> "settings"
    :> "version"
    :> Get '[JSON] (Either WalletError SoftwareVersion)

type GetSyncProgress = API
    :> "settings"
    :> "sync"
    :> "progress"
    :> Get '[JSON] (Either WalletError SyncProgress)

-- | Servant API which provides access to wallet.
-- TODO: Should be composed depending on the resource - wallets, txs, ... http://haskell-servant.github.io/tutorial/0.4/server.html#nested-apis
type WalletApi =
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
     UpdateWallet
    :<|>
     DeleteWallet
    :<|>
     ImportKey
    :<|>
     WalletRestore 
    :<|>
     NewWallet
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

-- | Helper Proxy.
walletApi :: Proxy WalletApi
walletApi = Proxy
