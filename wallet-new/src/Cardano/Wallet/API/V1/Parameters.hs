{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Cardano.Wallet.API.V1.Parameters where

import           Universum

import           Servant
import           Servant.Client

import           Cardano.Wallet.API.Request (RequestParams (..))
import           Cardano.Wallet.API.Request.Pagination (Page (..), PaginationParams (..),
                                                        PerPage (..))
import           Cardano.Wallet.API.Types (DQueryParam, mapRouter)


-- | Unpacked pagination parameters.
type WithWalletRequestParams c =
       DQueryParam "page"     Page
    :> DQueryParam "per_page" PerPage
    :> c

-- | Stub datatype which is used as special API argument specifier for
-- grouped pagination parameters.
data WalletRequestParams

instance HasServer subApi ctx =>
         HasServer (WalletRequestParams :> subApi) ctx where
    type ServerT (WalletRequestParams :> subApi) m =
        RequestParams -> ServerT subApi m
    route =
        mapRouter @(WithWalletRequestParams subApi) route $
        \f ppPage ppPerPage -> f $ RequestParams {
              rpPaginationParams = PaginationParams ppPage ppPerPage
            }
    hoistServerWithContext _ ct hoist' s = hoistServerWithContext (Proxy @subApi) ct hoist' . s

instance HasClient m subApi => HasClient m (WalletRequestParams :> subApi) where
    type Client m (WalletRequestParams :> subApi) = Maybe Page -> Maybe PerPage -> Client m subApi
    clientWithRoute proxyM _ =
        clientWithRoute proxyM (Proxy :: Proxy (QueryParam "page" Page :> QueryParam "per_page" PerPage :> subApi))
