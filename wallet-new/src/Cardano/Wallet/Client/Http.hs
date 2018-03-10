{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Client.Http
    ( module Cardano.Wallet.Client.Http
      -- * Abstract Client export
    , module Cardano.Wallet.Client
    ) where

import           Universum

import Control.Lens (_Left)
import           Data.Bifunctor
import           Network.HTTP.Client (Manager)
import           Servant ((:<|>) (..), (:>))
import           Servant.Client (BaseUrl, ClientEnv (..), client, runClientM)

import qualified Cardano.Wallet.API.V1 as V1
import           Cardano.Wallet.Client

-- | Given a 'BaseUrl' and an @http-client@ 'Manager', this returns
-- a 'WalletClient' that operates in 'IO'.
mkHttpClient
    :: BaseUrl
    -> Manager
    -> WalletClient IO
mkHttpClient baseUrl manager = WalletClient
    { getAddressIndexPaginated
        = \x -> run . getAddressIndexR x
    , postAddress
        = run . postAddressR
    , getAddressValidity
        = run . getAddressValidityR
    -- wallets endpoints
    , postWallet
        = run . postWalletR
    , getWalletIndexFilterSorts
        = \fil so mp -> run . getWalletIndexFilterSortsR fil so mp
    , updateWalletPassword
        = \x -> run . updateWalletPasswordR x
    , deleteWallet
        = unNoContent . run . deleteWalletR
    , getWallet
        = run . getWalletR
    , updateWallet
        = \x -> run . updateWalletR x
    -- account endpoints
    , deleteAccount
        = \x -> unNoContent . run . deleteAccountR x
    , getAccount
        = \x -> run . getAccountR x
    , getAccountIndexPaged
        = \x y -> run . getAccountIndexPagedR x y
    , postAccount
        = \w -> run . postAccountR w
    , updateAccount
        = \x y -> run . updateAccountR x y
    -- transactions endpoints
    , postTransaction
        = run . postTransactionR
    , getTransactionIndex
        = \walletId mAccountIndex mAddress mPage ->
             run . getTransactionIndexR walletId mAccountIndex mAddress mPage
    , getTransactionFee
        = run . getTransactionFeeR
    -- updates
    , getNextUpdate
        = run getNextUpdateR
    , postWalletUpdate
        = run postWalletUpdateR
    -- settings
    , getNodeSettings
        = run getNodeSettingsR
    -- info
    , getNodeInfo
        = run getNodeInfoR
    }

  where
    unNoContent = map void
    clientEnv = ClientEnv manager baseUrl
    run       = fmap (over _Left ClientHttpError) . (`runClientM` clientEnv)
    (getAddressIndexR :<|> postAddressR :<|> getAddressValidityR)
        :<|> (postWalletR
            :<|> getWalletIndexFilterSortsR
            :<|> updateWalletPasswordR
            :<|> deleteWalletR
            :<|> getWalletR
            :<|> updateWalletR
            :<|> deleteAccountR
            :<|> getAccountR
            :<|> getAccountIndexPagedR
            :<|> postAccountR
            :<|> updateAccountR)
        :<|> (postTransactionR
            :<|> getTransactionIndexR
            :<|> getTransactionFeeR)
        :<|> (getNextUpdateR
            :<|> postWalletUpdateR)
        :<|> getNodeSettingsR
        :<|> getNodeInfoR
        = client (Proxy @("api" :> "v1" :> V1.API))
