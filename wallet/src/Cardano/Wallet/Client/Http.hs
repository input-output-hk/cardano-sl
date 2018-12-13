{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Client.Http
    ( module Cardano.Wallet.Client.Http
      -- * Abstract Client export
    , module Cardano.Wallet.Client
    -- * Servant Client Export
    , module Servant.Client
    ) where

import           Universum

import           Control.Lens (_Left)
import           Data.Aeson (decode)
import           Network.HTTP.Client (Manager)
import           Servant ((:<|>) (..), (:>))
import           Servant.Client (BaseUrl (..), ClientEnv (..), ClientM,
                     ServantError (..), client, runClientM)

import           Cardano.Wallet.API (WIPAPI)
import qualified Cardano.Wallet.API.Internal as Internal
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
    , getAddress
        = run . getAddressR
    -- wallets endpoints
    , postWallet
        = run . postWalletR
    , getWalletIndexFilterSorts
        = \mp mpp filters sorts -> run $
            getWalletIndexFilterSortsR mp mpp filters sorts
    , updateWalletPassword
        = \x -> run . updateWalletPasswordR x
    , deleteWallet
        = unNoContent . run . deleteWalletR
    , getWallet
        = run . getWalletR
    , updateWallet
        = \x -> run . updateWalletR x
    , getUtxoStatistics
        = run . getUtxoStatisticsR
    , postEosWallet
        = run . postEosWalletR
    , deleteEosWallet
        = unNoContent . run . deleteEosWalletR
    , postUnsignedTransaction
        = run . postUnsignedTransactionR
    , postSignedTransaction
        = run . postSignedTransactionR
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
    , getAccountAddresses
        = \x y p pp filters -> run $ getAccountAddressesR x y p pp filters
    , getAccountBalance
        = \x -> run . getAccountBalanceR x
    -- transactions endpoints
    , postTransaction
        = run . postTransactionR
    , getTransactionIndexFilterSorts
        = \walletId mAccountIndex mAddress mPage mpp filters ->
             run . getTransactionIndexFilterSortsR walletId mAccountIndex mAddress mPage mpp filters
    , getTransactionFee
        = run . getTransactionFeeR
    , redeemAda
        = run ... redeemAdaR
    -- settings
    , getNodeSettings
        = run getNodeSettingsR
    -- info
    , getNodeInfo
        = run . getNodeInfoR

    -- Internal API

    , nextUpdate
        = run $ nextUpdateR

    , applyUpdate
        = unNoContent $ run $ applyUpdateR

    , postponeUpdate
        = unNoContent $ run $ postponeUpdateR

    , resetWalletState
        = unNoContent $ run $ resetWalletStateR

    , importWallet
        = run . importWalletR
    }

  where

    -- Must give the type. GHC will not infer it to be polymorphic in 'a'.
    run :: forall a . ClientM a -> IO (Either ClientError a)
    run = fmap (over _Left parseJsendError) . (`runClientM` clientEnv)

    unNoContent = map void
    cookieJar = Nothing
    clientEnv = ClientEnv manager baseUrl cookieJar
    parseJsendError servantErr =
        case servantErr of
            FailureResponse resp ->
                case decode (responseBody resp) of
                    Just err -> ClientWalletError err
                    Nothing  -> ClientHttpError servantErr
            _ -> ClientHttpError servantErr
    getAddressIndexR
        :<|> postAddressR
        :<|> getAddressR
        = addressesAPI

    postWalletR
        :<|> getWalletIndexFilterSortsR
        :<|> updateWalletPasswordR
        :<|> deleteWalletR
        :<|> getWalletR
        :<|> updateWalletR
        :<|> getUtxoStatisticsR
        = walletsAPI

    postEosWalletR
        :<|> deleteEosWalletR
        :<|> postUnsignedTransactionR
        :<|> postSignedTransactionR
        = client (Proxy @ WIPAPI)

    deleteAccountR
        :<|> getAccountR
        :<|> getAccountIndexPagedR
        :<|> postAccountR
        :<|> updateAccountR
        :<|> getAccountAddressesR
        :<|> getAccountBalanceR
        = accountsAPI

    postTransactionR
        :<|> getTransactionIndexFilterSortsR
        :<|> getTransactionFeeR
        :<|> redeemAdaR
        = transactionsAPI


    nextUpdateR
        :<|> applyUpdateR
        :<|> postponeUpdateR
        :<|> resetWalletStateR
        :<|> importWalletR
        = internalAPI

    addressesAPI
        :<|> walletsAPI
        :<|> accountsAPI
        :<|> transactionsAPI
        :<|> getNodeSettingsR
        :<|> getNodeInfoR
        = v1API

    v1API :<|> internalAPI = client (Proxy @(V1API :<|> InternalAPI))

type V1API = "api" :> "v1" :> V1.API
type InternalAPI = "api" :> "internal" :> Internal.API
