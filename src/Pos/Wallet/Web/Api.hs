{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API for wallet.

module Pos.Wallet.Web.Api
       ( WalletApi
       , walletApi
       ) where

import           Data.Proxy                 (Proxy (Proxy))

import           Pos.Types                  (Coin, SoftwareVersion)
import           Pos.Wallet.Web.ClientTypes (CAddress, CCurrency, CInitialized, CProfile,
                                             CTx, CTxId, CTxMeta, CUpdateInfo, CWallet,
                                             CWalletInit, CWalletMeta, CWalletRedeem,
                                             SyncProgress)
import           Pos.Wallet.Web.Error       (WalletError)
import           Servant.API                ((:<|>), (:>), Capture, Delete, Get, JSON,
                                             Post, Put, QueryParam, ReqBody)
import           Universum

-- | Servant API which provides access to wallet.
-- TODO: Should be composed depending on the resource - wallets, txs, ... http://haskell-servant.github.io/tutorial/0.4/server.html#nested-apis
type WalletApi =
     -- only works in development mode, gives 403 otherwise
     "api"
     :> "test"
     :> "reset"
     :> Post '[JSON] (Either WalletError ())
    :<|>
     -------------------------------------------------------------------------
     -- Wallets
     -------------------------------------------------------------------------
     "api"
     :> "wallets"
     :> Capture "walletId" CAddress
     :> Get '[JSON] (Either WalletError CWallet)
    :<|>
     "api"
     :> "wallets"
     :> Get '[JSON] (Either WalletError [CWallet])
    :<|>
     "api"
     :> "wallets"
     :> Capture "walletId" CAddress
     :> ReqBody '[JSON] CWalletMeta
     :> Put '[JSON] (Either WalletError CWallet)
    :<|>
     "api"
     :> "wallets"
     :> ReqBody '[JSON] CWalletInit
     :> Post '[JSON] (Either WalletError CWallet)
    :<|>
     "api"
     :> "wallets"
     :> Capture "walletId" CAddress
     :> Delete '[JSON] (Either WalletError ())
    :<|>
     "api"
     :> "wallets"
     :> "keys"
     :> ReqBody '[JSON] Text
     :> Post '[JSON] (Either WalletError CWallet)
    :<|>
     "api"
     :> "wallets"
     :> "restore"
     :> ReqBody '[JSON] CWalletInit
     :> Post '[JSON] (Either WalletError CWallet)
    :<|>
     ----------------------------------------------------------------------------
     -- Addresses
     ----------------------------------------------------------------------------
     "api"
     :> "addresses"
     :> Capture "address" Text
     :> "currencies"
     :> Capture "currency" CCurrency
     :> Get '[JSON] (Either WalletError Bool)
    :<|>
     ----------------------------------------------------------------------------
     -- Profile(s)
     ----------------------------------------------------------------------------
     -- TODO: A single profile? Should be possible in the future to have multiple profiles?
     "api"
     :> "profile"
     :> Get '[JSON] (Either WalletError CProfile)
    :<|>
     "api"
     :> "profile"
     :> ReqBody '[JSON] CProfile
     :> Post '[JSON] (Either WalletError CProfile)
    :<|>
     ----------------------------------------------------------------------------
     -- Transactons
     ----------------------------------------------------------------------------
    -- TODO: for now we only support one2one sending. We should extend this to support many2many
     "api"
     :> "txs"
     :> "payments"
     :> Capture "from" CAddress
     :> Capture "to" CAddress
     :> Capture "amount" Coin
     :> Post '[JSON] (Either WalletError CTx)
    :<|>
    -- TODO: for now we only support one2one sending. We should extend this to support many2many
     "api"
     :> "txs"
     :> "payments"
     :> Capture "from" CAddress
     :> Capture "to" CAddress
     :> Capture "amount" Coin
     :> Capture "currency" CCurrency
     :> Capture "title" Text
     :> Capture "description" Text
     :> Post '[JSON] (Either WalletError CTx)
    :<|>
      -- FIXME: Should capture the URL parameters in the payload.
      "api"
      :> "txs"
      :> "payments"
      :> Capture "address" CAddress
      :> Capture "transaction" CTxId
      :> ReqBody '[JSON] CTxMeta
      :> Post '[JSON] (Either WalletError ())
    :<|>
     "api"
     :> "txs"
     :> "histories"
     :> Capture "address" CAddress
     :> QueryParam "skip" Word
     :> QueryParam "limit" Word
     :> Get '[JSON] (Either WalletError ([CTx], Word))
    :<|>
     "api"
     :> "txs"
     :> "histories"
     :> Capture "address" CAddress
     :> Capture "search" Text
     :> QueryParam "skip" Word
     :> QueryParam "limit" Word
     :> Get '[JSON] (Either WalletError ([CTx], Word))
    :<|>
     ----------------------------------------------------------------------------
     -- Updates
     ----------------------------------------------------------------------------
     "api"
     :> "update"
     :> Get '[JSON] (Either WalletError CUpdateInfo)
    :<|>
     "api"
     :> "update"
     :> Post '[JSON] (Either WalletError ())
    :<|>
     ----------------------------------------------------------------------------
     -- Redemptions
     ----------------------------------------------------------------------------
     "api"
     :> "redemptions"
     :> "ada"
     :> ReqBody '[JSON] CWalletRedeem
     :> Post '[JSON] (Either WalletError CTx)
    :<|>
     ----------------------------------------------------------------------------
     -- Reporting
     ----------------------------------------------------------------------------
     "api"
     :> "reporting"
     :> "initialized"
     :> ReqBody '[JSON] CInitialized
     :> Post '[JSON] (Either WalletError ())
    :<|>
     ----------------------------------------------------------------------------
     -- Settings
     ----------------------------------------------------------------------------
     "api"
     :> "settings"
     :> "slots"
     :> "duration"
     :> Get '[JSON] (Either WalletError Word)
    :<|>
     "api"
     :> "settings"
     :> "version"
     :> Get '[JSON] (Either WalletError SoftwareVersion)
    :<|>
     "api"
     :> "settings"
     :> "sync"
     :> "progress"
     :> Get '[JSON] (Either WalletError SyncProgress)



-- | Helper Proxy.
walletApi :: Proxy WalletApi
walletApi = Proxy
