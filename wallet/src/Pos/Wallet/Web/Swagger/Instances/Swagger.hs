{-# LANGUAGE ScopedTypeVariables #-}

-- | Swagger instances

module Pos.Wallet.Web.Swagger.Instances.Swagger where

import           Universum

import           Servant           ((:>))
import           Servant.Multipart (MultipartForm)
import           Servant.Swagger   (HasSwagger (..))

import           Pos.Util.Servant  (CDecodeApiArg, VerbMod, WithDefaultApiArg)

instance HasSwagger api => HasSwagger (MultipartForm a :> api) where
    toSwagger Proxy = toSwagger $ Proxy @api

instance HasSwagger v =>
         HasSwagger (VerbMod mod v) where
    toSwagger _ = toSwagger (Proxy @v)

instance HasSwagger (apiType a :> res) =>
         HasSwagger (CDecodeApiArg apiType a :> res) where
    toSwagger _ = toSwagger (Proxy @(apiType a :> res))

instance HasSwagger (apiType a :> res) =>
         HasSwagger (WithDefaultApiArg apiType a :> res) where
    toSwagger _ = toSwagger (Proxy @(apiType a :> res))
