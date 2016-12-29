{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API for wallet.

module Pos.Wallet.Web.Api
       ( WalletApi
       , walletApi
       , Cors
       ) where

import           Data.Proxy                 (Proxy (Proxy))
import           Pos.Types                  (Coin, Tx)
import           Pos.Wallet.Web.ClientTypes (CAddress, CTx, CTxId, CTxMeta, CWallet,
                                             CWalletMeta)
import           Servant.API                ((:<|>), (:>), Capture, Get, Header, Headers,
                                             JSON, Post, ReqBody)
import           Universum                  (Text)

type Cors a = Headers '[Header "Access-Control-Allow-Origin" Text] a

-- | Servant API which provides access to wallet.
type WalletApi =
     "api" :> "get_wallet" :> Capture "address" CAddress :> Get '[JSON] (Cors CWallet)
    :<|>
     "api" :> "get_wallets" :> Get '[JSON] (Cors [CWallet])
    :<|>
    -- TODO: for now we only support one2one sending. We should extend this to support many2many
     "api" :> "send" :> Capture "from" CAddress :> Capture "to" CAddress :> Capture "amount" Coin :> Post '[JSON] (Cors CTx)
    :<|>
     "api" :> "history" :> Capture "address" CAddress :> Get '[JSON] (Cors [CTx])
    :<|>
     "api" :> "update_transaction" :> Capture "address" CAddress :> Capture "transaction" CTxId :> ReqBody '[JSON] CTxMeta :> Post '[JSON] (Cors ())
    :<|>
     "api" :> "new_wallet" :> ReqBody '[JSON] CWalletMeta :> Post '[JSON] (Cors CWallet)
    :<|>
     "api" :> "update_wallet" :> Capture "address" CAddress :> ReqBody '[JSON] CWalletMeta :> Post '[JSON] (Cors CWallet)
    :<|>
    -- FIXME: this should be DELETE method
     "api" :> "delete_wallet" :> Capture "address" CAddress :> Post '[JSON] (Cors ())

-- | Helper Proxy.
walletApi :: Proxy WalletApi
walletApi = Proxy
