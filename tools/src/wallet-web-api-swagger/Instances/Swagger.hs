{-# LANGUAGE ScopedTypeVariables #-}

-- | Swagger instances

module Instances.Swagger () where

import           Universum

import           Servant           ((:>))
import           Servant.Multipart (MultipartForm)
import           Servant.Swagger   (HasSwagger (..))

instance HasSwagger api => HasSwagger (MultipartForm a :> api) where
    toSwagger Proxy = toSwagger $ Proxy @api
