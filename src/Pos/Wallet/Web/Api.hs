{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API for wallet.

module Pos.Wallet.Web.Api
       ( WalletApi
       , walletApi
       ) where

import           Data.Proxy                 (Proxy (Proxy))
import           Servant.API                ((:<|>), (:>), Capture, Get, JSON,
                                             Post)
import           Universum

import           Pos.Types                  (Address, Coin)
import           Pos.Wallet.Web.ClientTypes (CWallet)

-- | Servant API which provides access to wallet.
type WalletApi =
    "addresses" :> Get '[JSON] [Address]
  :<|>
    "balances" :> Get '[JSON] [(Address, Coin)]
  :<|>
    "send" :> Capture "from" Word :> Capture "to" Address :> Capture "amount" Coin :> Post '[JSON] ()
  -- :<|>
  --   "wallets" :> Get '[JSON] [CWallet]

-- | Helper Proxy.
walletApi :: Proxy WalletApi
walletApi = Proxy
