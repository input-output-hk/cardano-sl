{-# LANGUAGE TypeOperators #-}

-- | Client functions for Wallet Web API. To be more precise,
-- only for endpoints we have to benchmark.

module Client.Pos.Wallet.Web.Api
    ( getHistory
    , getWallet
    , getWallets
    , newPayment
    ) where

import           Universum

import           Servant.API                ((:<|>) (..), (:>))
import           Servant.Client             (ClientM, client)

import           Pos.Client.Txp.Util        (InputSelectionPolicy)
import           Pos.Core.Types             (Coin)
import           Pos.Wallet.Web.Api         (ApiPrefix, GetHistory, GetWallet, GetWallets,
                                             NewPayment)
import           Pos.Wallet.Web.ClientTypes (Addr, CAccountId (..), CId (..), CPassPhrase,
                                             CTx, CWallet, ScrollLimit, ScrollOffset, Wal)
import           Pos.Wallet.Web.Error       (WalletError)

-- | "Benchmarking API" which includes
-- endpoints we need for benchmarking.
type WalletBenchApi = ApiPrefix :> (
     GetHistory
    :<|>
     GetWallet
    :<|>
     GetWallets
    :<|>
     NewPayment
    )

-- | Clients for "Benchmarking API".
getHistory
    :: Maybe (CId Wal)
    -> Maybe CAccountId
    -> Maybe (CId Addr)
    -> Maybe ScrollOffset
    -> Maybe ScrollLimit
    -> ClientM $ Either WalletError ([CTx], Word)
getWallet
    :: CId Wal
    -> ClientM $ Either WalletError CWallet
getWallets
    :: ClientM $ Either WalletError [CWallet]
newPayment
    :: Maybe CPassPhrase
    -> CAccountId
    -> CId Addr
    -> Coin
    -> Maybe InputSelectionPolicy
    -> ClientM $ Either WalletError CTx
getHistory :<|> getWallet :<|> getWallets :<|> newPayment =
    client (Proxy @WalletBenchApi)
