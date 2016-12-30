{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API for wallet.

module Pos.Wallet.Web.Api
       ( WalletApi
       , walletApi
       ) where

import           Data.Proxy                 (Proxy (Proxy))
import           Pos.Types                  (Coin, Tx)
import           Pos.Wallet.Web.ClientTypes (CAddress, CCurrency, CTx, CTxId, CTxMeta,
                                             CWallet, CWalletMeta)
import           Servant.API                ((:<|>), (:>), Capture, Get, Header, Headers,
                                             JSON, Post, ReqBody)
import           Universum                  (Bool, Text)

-- | Servant API which provides access to wallet.
type WalletApi =
     "api" :> "get_wallet" :> Capture "address" CAddress :> Get '[JSON] CWallet
    :<|>
     "api" :> "get_wallets" :> Get '[JSON] [CWallet]
    :<|>
    -- TODO: for now we only support one2one sending. We should extend this to support many2many
     "api" :> "send" :> Capture "from" CAddress :> Capture "to" CAddress :> Capture "amount" Coin :> Post '[JSON] CTx
    :<|>
     "api" :> "history" :> Capture "address" CAddress :> Get '[JSON] [CTx]
    :<|>
     "api" :> "update_transaction" :> Capture "address" CAddress :> Capture "transaction" CTxId :> ReqBody '[JSON] CTxMeta :> Post '[JSON] ()
    :<|>
     "api" :> "new_wallet" :> ReqBody '[JSON] CWalletMeta :> Post '[JSON] CWallet
    :<|>
     "api" :> "update_wallet" :> Capture "address" CAddress :> ReqBody '[JSON] CWalletMeta :> Post '[JSON] CWallet
    :<|>
    -- FIXME: this should be DELETE method
     "api" :> "delete_wallet" :> Capture "address" CAddress :> Post '[JSON] ()
    :<|>
     "api" :> "valid_address" :> Capture "currency" CCurrency :> Capture "address" Text :> Get '[JSON] Bool

-- | Helper Proxy.
walletApi :: Proxy WalletApi
walletApi = Proxy
