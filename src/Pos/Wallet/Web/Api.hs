{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API for wallet.

module Pos.Wallet.Web.Api
       ( WalletApi
       , walletApi
       , Cors
       ) where

import           Data.Proxy                 (Proxy (Proxy))
import           Pos.Types                  (Address, Coin, Tx)
import           Pos.Wallet.Web.ClientTypes (CAddress)
import           Servant.API                ((:<|>), (:>), Capture, Get, Header, Headers,
                                             JSON, Post)
import           Universum                  (Text)

type Cors a = Headers '[Header "Access-Control-Allow-Origin" Text] a

-- | Servant API which provides access to wallet.
type WalletApi =
     "api" :> "addresses" :> Get '[JSON] (Cors [CAddress])
    :<|>
     "api" :> "balances" :> Get '[JSON] (Cors [(CAddress, Coin)])
    :<|>
     "api" :> "send" :> Capture "from" Address :> Capture "to" Address :> Capture "amount" Coin :> Post '[JSON] (Cors ())
    :<|>
     "api" :> "history" :> Capture "address" Address :> Get '[JSON] (Cors [Tx])
    :<|>
     "api" :> "new_address" :> Post '[JSON] (Cors CAddress)
    :<|>
    -- FIXME: this should be DELETE method
     "api" :> "delete_address" :> Capture "address" Address :> Post '[JSON] (Cors ())

-- | Helper Proxy.
walletApi :: Proxy WalletApi
walletApi = Proxy
