
module Cardano.Wallet.API.Response.JSend where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Char as Char
import           Test.QuickCheck (Arbitrary (..), elements)
import           Universum

data ResponseStatus =
      SuccessStatus
    | FailStatus
    | ErrorStatus
    deriving (Show, Eq, Ord, Enum, Bounded)

deriveJSON defaultOptions { constructorTagModifier = map Char.toLower . reverse . drop 6 . reverse } ''ResponseStatus

instance Arbitrary ResponseStatus where
    arbitrary = elements [minBound .. maxBound]
