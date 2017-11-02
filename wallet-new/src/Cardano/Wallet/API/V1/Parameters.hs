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

import           Cardano.Wallet.API.Types    (DQueryParam)
import           Cardano.Wallet.API.V1.Types

import           Data.Text                   (Text)
import           Servant


type WalletRequestParams =
       QueryParam "page"     Page
    :> QueryParam "per_page" PerPage
    :> QueryParam "response_type"       ResponseType
    :> Header "Daedalus-Response-Format" ResponseFormat

type family WithWalletRequestParams c :: * where
  WithWalletRequestParams c = QueryParam "page"     Page
                           :> QueryParam "per_page" PerPage
                           :> QueryParam "response_type"       ResponseType
                           :> Header "Daedalus-Response-Format" ResponseFormat
                           :> c

-- | Instance of `HasServer` which erases the `Tags` from its routing,
-- as the latter is needed only for Swagger.
instance (HasServer subApi context) => HasServer (WalletRequestParams :> subApi) context where
  type ServerT (WalletRequestParams :> subApi) m =
      Maybe Page -> Maybe PerPage -> Maybe ResponseType -> Maybe ResponseFormat -> ServerT subApi m
  route _ = route (Proxy @(WithWalletRequestParams subApi))
