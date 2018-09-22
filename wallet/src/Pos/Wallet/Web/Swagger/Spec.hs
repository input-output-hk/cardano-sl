{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Swagger specification

module Pos.Wallet.Web.Swagger.Spec
    ( swaggerSpecForWalletApi
    ) where

import           Universum

import           Control.Lens ((?~))
import           Data.Swagger (Swagger, description, info, title, version)
import           Data.Version (Version (Version), showVersion)
import           Formatting.Buildable (build)
import           Servant.Swagger (toSwagger)

import           Pos.Wallet.Web.Api (walletApi)
import           Pos.Wallet.Web.Swagger.Instances.Schema ()
import           Servant.API.ContentTypes (NoContent (..))


instance Buildable NoContent where
    build NoContent = build ()

-- | Build Swagger-specification from 'walletApi'.
swaggerSpecForWalletApi :: Swagger
swaggerSpecForWalletApi = toSwagger walletApi
    & info . title       .~ "Cardano SL Wallet Web API"
    & info . version     .~ toText (showVersion $ Version [ 1, 3, 0 ] [])
    & info . description ?~ "This is an API for Cardano SL wallet."
