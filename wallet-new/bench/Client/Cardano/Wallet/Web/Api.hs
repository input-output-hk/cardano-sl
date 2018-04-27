{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Client functions for Wallet Web API. To be more precise,
-- only for endpoints we have to benchmark.

module Client.Cardano.Wallet.Web.Api
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
import           Pos.Core.Common            (Coin)
-- We call legasy API directly, it will be changed soon!
import           Pos.Wallet.Web.Api         (GetAccounts, GetHistory, GetSyncProgress,
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
type WalletBenchApi = "api" :> (
         ("accounts"  :> GetAccounts)
    :<|> ("txs"       :> GetHistory)
    :<|> ("txs"       :> NewPayment)
    :<|> ("settings"  :> GetSyncProgress)
    :<|> ("wallets"   :> GetWallet)
    :<|> ("wallets"   :> GetWallets)
    :<|> ("wallets"   :> NewWallet)
    :<|> ("addresses" :> IsValidAddress)
    :<|> ("addresses" :> NewAddress)
    )

-- | Clients for "Benchmarking API".
getAccounts
    :: Maybe (CId Wal)
    -> ClientM (Either WalletError [CAccount])
getHistory
    :: Maybe (CId Wal)
    -> Maybe CAccountId
    -> Maybe (CId Addr)
    -> Maybe ScrollOffset
    -> Maybe ScrollLimit
    -> ClientM (Either WalletError ([CTx], Word))
newPayment
    :: Maybe CPassPhrase
    -> CAccountId
    -> CId Addr
    -> Coin
    -> Maybe InputSelectionPolicy
    -> ClientM (Either WalletError CTx)
getSyncProgress
    :: ClientM (Either WalletError SyncProgress)
getWallet
    :: CId Wal
    -> ClientM (Either WalletError CWallet)
getWallets
    :: ClientM (Either WalletError [CWallet])
newWallet
    :: Maybe CPassPhrase
    -> CWalletInit
    -> ClientM (Either WalletError CWallet)
isValidAddress
    :: CId Addr
    -> ClientM (Either WalletError Bool)
newAddress
    :: Maybe CPassPhrase
    -> CAccountId
    -> ClientM (Either WalletError CAddress)
getAccounts
  :<|> getHistory
  :<|> newPayment
  :<|> getSyncProgress
  :<|> getWallet
  :<|> getWallets
  :<|> newWallet
  :<|> isValidAddress
  :<|> newAddress = client (Proxy @WalletBenchApi)
