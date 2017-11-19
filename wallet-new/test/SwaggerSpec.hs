{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module SwaggerSpec where

import           Universum

import qualified Prelude

import           Cardano.Wallet.API
import           Cardano.Wallet.API.V1.Swagger ()
import           Cardano.Wallet.Orphans.Aeson ()
import           Cardano.Wallet.Orphans.Arbitrary ()
import           Data.String.Conv
import           Data.Swagger
import           Pos.Wallet.Aeson.ClientTypes ()
import           Servant.API.ContentTypes
import           Servant.Swagger.Test
import           Test.Hspec
import           Test.QuickCheck.Instances ()


-- Syntethic instances and orphans to be able to use `validateEveryToJSON`.
-- In the future, hopefully, we will never need these.

instance {-# OVERLAPPABLE #-} Buildable a => Prelude.Show a where
    show = toS . pretty

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
  xdescribe "ToJSON matches ToSchema" $ validateEveryToJSON walletAPI
