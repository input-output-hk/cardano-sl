module Cardano.Wallet.API.Response.JSend where

import           Universum

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Char as Char
import           Data.Swagger
import qualified Data.Text.Buildable
import           Test.QuickCheck (Arbitrary (..), elements)

data ResponseStatus =
      SuccessStatus
    | FailStatus
    | ErrorStatus
    deriving (Show, Eq, Ord, Enum, Bounded)

deriveJSON defaultOptions { constructorTagModifier = map Char.toLower . reverse . drop 6 . reverse } ''ResponseStatus

instance Arbitrary ResponseStatus where
    arbitrary = elements [minBound .. maxBound]

instance ToSchema ResponseStatus where
    declareNamedSchema _ = do
        pure $ NamedSchema (Just "ResponseStatus") $ mempty
            & type_ .~ SwaggerString
            & enum_ .~ Just ["success", "fail", "error"]

instance Buildable ResponseStatus where
    build SuccessStatus = "success"
    build FailStatus    = "fail"
    build ErrorStatus   = "error"
