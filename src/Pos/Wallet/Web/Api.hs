{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API for wallet.

module Pos.Wallet.Web.Api
       ( WalletApi
       , walletApi
       ) where

import           Data.Proxy                 (Proxy (Proxy))

import           Pos.Types                  (Coin, SoftwareVersion)
import           Pos.Wallet.Web.ClientTypes (CAddress, CCurrency, CProfile, CTx, CTxId,
                                             CTxMeta, CUpdateInfo, CWallet, CWalletInit,
                                             CWalletMeta, CWalletRedeem)
import           Pos.Wallet.Web.Error       (WalletError)
import           Servant.API                ((:<|>), (:>), Capture, Get, JSON, Post,
                                             ReqBody)
import           Universum

-- | Servant API which provides access to wallet.
type WalletApi =
     "api" :> "get_wallet" :> Capture "address" CAddress :> Get '[JSON] (Either WalletError CWallet)
    :<|>
     "api" :> "get_wallets" :> Get '[JSON] (Either WalletError [CWallet])
    :<|>
    -- TODO: for now we only support one2one sending. We should extend this to support many2many
     "api" :> "send" :> Capture "from" CAddress :> Capture "to" CAddress :> Capture "amount" Coin :> Post '[JSON] (Either WalletError CTx)
    :<|>
    -- TODO: for now we only support one2one sending. We should extend this to support many2many
     "api" :> "send" :> Capture "from" CAddress :> Capture "to" CAddress :> Capture "amount" Coin :> Capture "currency" CCurrency :> Capture "title" Text :> Capture "description" Text :> Post '[JSON] (Either WalletError CTx)
    :<|>
     "api" :> "txhistory" :> Capture "address" CAddress :> Capture "skip" Word :> Capture "limit" Word :> Get '[JSON] (Either WalletError ([CTx], Word))
    :<|>
     "api" :> "search_txhistory" :> Capture "address" CAddress :> Capture "search" Text :> Capture "skip" Word :> Capture "limit" Word :> Get '[JSON] (Either WalletError ([CTx], Word))
    :<|>
     "api" :> "update_transaction" :> Capture "address" CAddress :> Capture "transaction" CTxId :> ReqBody '[JSON] CTxMeta :> Post '[JSON] (Either WalletError ())
    :<|>
     "api" :> "new_wallet" :> ReqBody '[JSON] CWalletInit :> Post '[JSON] (Either WalletError CWallet)
    :<|>
     "api" :> "restore_wallet" :> ReqBody '[JSON] CWalletInit :> Post '[JSON] (Either WalletError CWallet)
    :<|>
     "api" :> "update_wallet" :> Capture "address" CAddress :> ReqBody '[JSON] CWalletMeta :> Post '[JSON] (Either WalletError CWallet)
    :<|>
    -- FIXME: this should be DELETE method
     "api" :> "delete_wallet" :> Capture "address" CAddress :> Post '[JSON] (Either WalletError ())
    :<|>
     "api" :> "valid_address" :> Capture "currency" CCurrency :> Capture "address" Text :> Get '[JSON] (Either WalletError Bool)
    :<|>
     "api" :> "get_profile" :> Get '[JSON] (Either WalletError CProfile)
    :<|>
     "api" :> "update_profile" :> ReqBody '[JSON] CProfile :> Post '[JSON] (Either WalletError CProfile)
    :<|>
     "api" :> "redeem_ada" :> ReqBody '[JSON] CWalletRedeem :> Post '[JSON] (Either WalletError CWallet)
    :<|>
     "api" :> "next_update" :> Get '[JSON] (Either WalletError CUpdateInfo)
    :<|>
     "api" :> "apply_update" :> Post '[JSON] (Either WalletError ())
    :<|>
     "api" :> "slot_duration" :> Get '[JSON] (Either WalletError Word)
    :<|>
     "api" :> "system_version" :> Get '[JSON] (Either WalletError SoftwareVersion)

-- | Helper Proxy.
walletApi :: Proxy WalletApi
walletApi = Proxy
