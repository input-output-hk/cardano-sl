{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module SwaggerSpec where

import           Universum

import qualified Prelude

import qualified Cardano.Wallet.API.V1 as V1
import           Cardano.Wallet.API.V1.Swagger ()
import           Cardano.Wallet.Orphans.Aeson ()
import           Cardano.Wallet.Orphans.Arbitrary ()
import           Data.String.Conv
import           Data.Swagger
import           Pos.Wallet.Aeson.ClientTypes ()
import           Servant.API.ContentTypes
import           Servant.Swagger.Test
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances ()


-- Syntethic instances and orphans to be able to use `validateEveryToJSON`.
-- In the future, hopefully, we will never need these.

instance {-# OVERLAPPABLE #-} Buildable a => Prelude.Show a where
    show = toS . pretty

instance ToSchema NoContent where
    declareNamedSchema _ = pure (NamedSchema Nothing mempty)

spec :: Spec
spec = modifyMaxSuccess (const 10) $
    describe "Swagger Integration" $ do
        xdescribe "(V1) ToJSON matches ToSchema" $ validateEveryToJSON (Proxy @ V1.API)
