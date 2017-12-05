{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
module Cardano.Wallet.API.Request (
    RequestParams (..)
  , ResponseFormat(..)
  -- * Handly re-exports
  , module Cardano.Wallet.API.Request.Pagination
  ) where

import           Universum

import           Data.Default (Default (def))
import qualified Data.Text.Buildable
import           Formatting (build, sformat)
import           Test.QuickCheck
import           Web.HttpApiData

import           Cardano.Wallet.API.Request.Pagination (PaginationMetadata (..), PaginationParams)

-- NOTE: Make no mistake: despite the type is labeled 'ResponseFormat' it belongs here
-- as it's just another parameter for a _request_.

-- | A `ResponseFormat` determines which type of response we want to return.
-- For now there's only two response formats - plain and extended with pagination data.
data ResponseFormat = Plain | Extended
    deriving (Show, Eq, Generic, Enum, Bounded)

instance Buildable ResponseFormat where
    build Plain    = "plain"
    build Extended = "extended"

instance FromHttpApiData ResponseFormat where
    parseQueryParam qp = parseQueryParam @Text qp >>= \case
        "plain"    -> Right Plain
        "extended" -> Right Extended
        _          -> Right def -- yield the default

instance ToHttpApiData ResponseFormat where
    toQueryParam = sformat build

instance Default ResponseFormat where
    def = Plain

instance Arbitrary ResponseFormat where
    arbitrary = oneof $ map pure [minBound..maxBound]

data RequestParams = RequestParams
    { rpResponseFormat   :: ResponseFormat
    -- ^ The user-specified 'Response' format.
    , rpPaginationParams :: PaginationParams
    -- ^ The pagination-related parameters
    }
