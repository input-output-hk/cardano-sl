{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API for wallet.

module Pos.Wallet.Web.Api
       ( WalletApi
       , walletApi
       ) where

import           Data.Proxy                 (Proxy (Proxy))

import           Pos.Types                  (Coin)
import           Pos.Wallet.Web.ClientTypes (CAddress, CCurrency, CTx, CTxId,
                                             CTxMeta, CWallet, CWalletMeta)
import           Pos.Wallet.Web.Error       (WalletError)
import           Servant.API                ((:<|>), (:>), Capture, Get, Header,
                                             Headers, JSON, Post, ReqBody)
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
     "api" :> "history" :> Capture "address" CAddress :> Get '[JSON] (Either WalletError [CTx])
    :<|>
     "api" :> "update_transaction" :> Capture "address" CAddress :> Capture "transaction" CTxId :> ReqBody '[JSON] CTxMeta :> Post '[JSON] (Either WalletError ())
    :<|>
     "api" :> "new_wallet" :> ReqBody '[JSON] CWalletMeta :> Post '[JSON] (Either WalletError CWallet)
    :<|>
     "api" :> "update_wallet" :> Capture "address" CAddress :> ReqBody '[JSON] CWalletMeta :> Post '[JSON] (Either WalletError CWallet)
    :<|>
    -- FIXME: this should be DELETE method
     "api" :> "delete_wallet" :> Capture "address" CAddress :> Post '[JSON] (Either WalletError ())
    :<|>
     "api" :> "valid_address" :> Capture "currency" CCurrency :> Capture "address" Text :> Get '[JSON] (Either WalletError Bool)

-- | Helper Proxy.
walletApi :: Proxy WalletApi
walletApi = Proxy
