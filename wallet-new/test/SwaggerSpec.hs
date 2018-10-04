{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module SwaggerSpec (spec) where

import           Universum

import qualified Prelude

import           Data.String.Conv
import           Data.Swagger
import           Pos.Wallet.Aeson.ClientTypes ()
import           Servant.API.ContentTypes
import           Servant.Swagger.Test ()
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances ()

import           Cardano.Wallet.API.Response (ValidJSON)
import qualified Cardano.Wallet.API.V1 as V1
import           Cardano.Wallet.API.V1.Swagger ()
import           Cardano.Wallet.Orphans.Aeson ()
import           Cardano.Wallet.Orphans.Arbitrary ()

-- for vendored code
import           Data.Aeson (ToJSON)
import           Servant.Swagger.Internal.Test (props)
import           Servant.Swagger.Internal.TypeLevel (BodyTypes, Every, TMap)
import           Test.QuickCheck (Arbitrary)

-- Syntethic instances and orphans to be able to use `validateEveryToJSON`.
-- In the future, hopefully, we will never need these.

instance {-# OVERLAPPABLE #-} Buildable a => Prelude.Show a where
    show = toS . pretty

-- | This instance is a little weird -- we have defined 'NoContent' to have
-- a 'ToJSON' instance that reuses @'toJSON' ()@, which gives @[]@:
--
-- @
-- >>> toJSON NoContent
-- Array []
-- @
instance ToSchema NoContent where
    declareNamedSchema _ =
        pure $ NamedSchema Nothing $ mempty
            & type_ .~ SwaggerArray
            & maxLength .~ Just 0

spec :: Spec
spec = modifyMaxSuccess (const 10) $
    describe "Swagger Integration" $ do
        parallel $ describe "(V1) ToJSON matches ToSchema" $
            validateEveryToJSON' (Proxy @ V1.API)

-- vendored
validateEveryToJSON'
    :: forall proxy api
    . TMap (Every [Typeable, Show, Arbitrary, ToJSON, ToSchema]) (BodyTypes ValidJSON api)
    => proxy api   -- ^ Servant API.
    -> Spec
validateEveryToJSON' _ = props
  (Proxy :: Proxy [ToJSON, ToSchema])
  (null . validateToJSON)
  (Proxy :: Proxy (BodyTypes ValidJSON api))
