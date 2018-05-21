{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Swagger instances

module Pos.Wallet.Web.Swagger.Instances.Swagger where

import           Universum

import           Servant ((:>))
import           Servant.Multipart (MultipartForm)
import           Servant.Swagger (HasSwagger (..))

instance HasSwagger api => HasSwagger (MultipartForm a :> api) where
    toSwagger Proxy = toSwagger $ Proxy @api
