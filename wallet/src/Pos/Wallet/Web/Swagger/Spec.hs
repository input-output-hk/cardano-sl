
-- | Swagger specification

module Pos.Wallet.Web.Swagger.Spec
    ( swaggerSpecForWalletApi
    ) where

import           Universum

import           Control.Lens                         ((?~))
import           Data.Swagger                         (Swagger, description, info, title,
                                                       version)
import           Data.Text.Buildable                  (build)
import           Data.Version                         (showVersion)
import qualified Paths_cardano_sl                     as CSL

import           Pos.Wallet.Web.Api                   (walletApi)
import           Pos.Wallet.Web.Swagger.CustomSwagger (toCustomSwagger)
import           Pos.Wallet.Web.Swagger.Description   ()
import           Servant.API.ContentTypes             (NoContent (..))


instance Buildable NoContent where
    build NoContent = build ()

-- | Build Swagger-specification from 'walletApi'.
swaggerSpecForWalletApi :: Swagger
swaggerSpecForWalletApi = toCustomSwagger walletApi
    & info . title       .~ "Cardano SL Wallet Web API"
    & info . version     .~ toText (showVersion CSL.version)
    & info . description ?~ "This is an API for Cardano SL wallet."
