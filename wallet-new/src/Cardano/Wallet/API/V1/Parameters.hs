{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Cardano.Wallet.API.V1.Parameters where

import           Universum

import           Cardano.Wallet.API.Types    (AlternativeApiArg, DQueryParam,
                                              WithDefaultApiArg)
import           Cardano.Wallet.API.V1.Types

import           Data.Text                   (Text)
import           Servant

type ResponseTypeParam = WithDefaultApiArg
    (AlternativeApiArg (QueryParam "response_type") (Header "Daedalus-Response-Format"))
    ResponseType

type WalletRequestParams =
       DQueryParam "page"     Page
    :> DQueryParam "per_page" PerPage
    :> ResponseTypeParam

type family WithWalletRequestParams c :: * where
  WithWalletRequestParams c = DQueryParam "page"     Page
                           :> DQueryParam "per_page" PerPage
                           :> ResponseTypeParam
                           :> c

-- | Instance of `HasServer` which erases the `Tags` from its routing,
-- as the latter is needed only for Swagger.
instance (HasServer subApi context) => HasServer (WalletRequestParams :> subApi) context where
  type ServerT (WalletRequestParams :> subApi) m =
      Page -> PerPage -> ResponseType -> ServerT subApi m
  route _ = route (Proxy @(WithWalletRequestParams subApi))
