module SwaggerSpec where

import           Cardano.Wallet.API
import           Cardano.Wallet.API.V1.Swagger ()
import           Data.Aeson
import           Data.Swagger
import           Data.Swagger.Schema
import           Servant.API.ContentTypes
import           Servant.Swagger.Test
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances     ()


-- Syntethic instances to be able to use `validateEveryToJSON`.

instance Arbitrary NoContent where
    arbitrary = pure NoContent

instance ToJSON NoContent where
    toJSON NoContent = String mempty

instance ToSchema NoContent where
    declareNamedSchema _ = pure (NamedSchema Nothing mempty)

-- | Apparently these specs will fail for OneOf with:
--
-- > let t = OneOf (Right (ExtendedResponse [Account "foo"] (Metadata 10 10 10 10)))
-- > validateToJSON (t :: OneOf [Account] (ExtendedResponse [Account]))
-- ["expected JSON value of type SwaggerArray"]
--
-- Is there a way to make the specs pass?

spec :: Spec
spec = describe "Swagger Integration" $ do
  describe "ToJSON matches ToSchema" $ validateEveryToJSON walletAPI
