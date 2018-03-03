{-# LANGUAGE TypeOperators #-}

-- | Client functions for Wallet Web API. To be more precise,
-- only for endpoints we have to benchmark.

module Client.Pos.Wallet.Web.Api
    ( getAccounts
    , getHistory
    , getSyncProgress
    , getWallet
    , getWallets
    , isValidAddress
    , newAddress
    , newPayment
    , newWallet
    ) where

import           Universum

import           Servant.API                ((:<|>) (..), (:>))
import           Servant.Client             (ClientM, client)

import           Pos.Client.Txp.Util        (InputSelectionPolicy)
import           Pos.Core.Types             (Coin)
import           Pos.Wallet.Web.Api         (ApiPrefix,
                                             GetAccounts, GetHistory, GetSyncProgress,
                                             GetWallet, GetWallets, IsValidAddress,
                                             NewAddress, NewPayment, NewWallet)
import           Pos.Wallet.Web.ClientTypes (Addr, CAccount (..), CAccountId (..),
                                             CAddress (..), CId (..),
                                             CPassPhrase, CTx, CWallet, CWalletInit,
                                             ScrollLimit, ScrollOffset, SyncProgress,
                                             Wal)
import           Pos.Wallet.Web.Error       (WalletError)

-- | "Benchmarking API" which includes
-- endpoints we need for benchmarking.
type WalletBenchApi = ApiPrefix :> (
     GetAccounts
    :<|>
     GetHistory
    :<|>
     GetSyncProgress
    :<|>
     GetWallet
    :<|>
     GetWallets
    :<|>
     IsValidAddress
    :<|>
     NewAddress
    :<|>
     NewPayment
    :<|>
     NewWallet
    )

-- | Clients for "Benchmarking API".
getAccounts
    :: Maybe (CId Wal)
    -> ClientM $ Either WalletError [CAccount]
getHistory
    :: Maybe (CId Wal)
    -> Maybe CAccountId
    -> Maybe (CId Addr)
    -> Maybe ScrollOffset
    -> Maybe ScrollLimit
    -> ClientM $ Either WalletError ([CTx], Word)
getSyncProgress
    :: ClientM $ Either WalletError SyncProgress
getWallet
    :: CId Wal
    -> ClientM $ Either WalletError CWallet
getWallets
    :: ClientM $ Either WalletError [CWallet]
isValidAddress
    :: CId Addr
    -> ClientM $ Either WalletError Bool
newAddress
    :: Maybe CPassPhrase
    -> CAccountId
    -> ClientM $ Either WalletError CAddress
newPayment
    :: Maybe CPassPhrase
    -> CAccountId
    -> CId Addr
    -> Coin
    -> Maybe InputSelectionPolicy
    -> ClientM $ Either WalletError CTx
newWallet
    :: Maybe CPassPhrase
    -> CWalletInit
    -> ClientM $ Either WalletError CWallet
getAccounts
  :<|> getHistory
  :<|> getSyncProgress
  :<|> getWallet
  :<|> getWallets
  :<|> isValidAddress
  :<|> newAddress
  :<|> newPayment
  :<|> newWallet = client (Proxy @WalletBenchApi)
