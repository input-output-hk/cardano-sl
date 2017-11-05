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
                                              WithDefaultApiArg, inRouteServer)
import           Cardano.Wallet.API.V1.Types

import           Data.Text                   (Text)
import           Servant

-- | A special parameter which combines `response_type` query argument and
-- `Daedalus-Response-Format` header (which have the same meaning) in one API argument.
type ResponseTypeParam = WithDefaultApiArg
    (AlternativeApiArg (QueryParam "response_type") (Header "Daedalus-Response-Format"))
    ResponseType

-- | Unpacked pagination parameters.
type WithWalletRequestParams c =
       DQueryParam "page"     Page
    :> DQueryParam "per_page" PerPage
    :> ResponseTypeParam
    :> c

-- | Stub datatype which is used as special API argument specifier for
-- grouped pagination parameters.
data WalletRequestParams

instance HasServer (WithWalletRequestParams subApi) ctx =>
         HasServer (WalletRequestParams :> subApi) ctx where
    type ServerT (WalletRequestParams :> subApi) m =
        PaginationParams -> ServerT subApi m
    route =
        inRouteServer @(WithWalletRequestParams subApi) route $
        \f ppPage ppPerPage ppResponseType -> f $ PaginationParams {..}
