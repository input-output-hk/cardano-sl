{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}
module Cardano.Wallet.API.Response (
    Metadata (..)
  , ResponseStatus(..)
  , WalletResponse(..)
  , JSONValidationError(..)
  , UnsupportedMimeTypeError(..)
  -- * Generating responses for collections
  , respondWith
  , fromSlice
  -- * Generating responses for single resources
  , single

  -- * A slice of a collection
  , SliceOf(..)

  , ValidJSON
  ) where

import           Prelude
import           Universum (Buildable, Exception, Text)

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Formatting (bprint, build)
import qualified Formatting.Buildable
import           Generics.SOP.TH (deriveGeneric)
import           GHC.Generics (Generic)
import           Servant (err400, err415)
import           Test.QuickCheck

import           Cardano.Wallet.API.Indices (Indexable, IxSet)
import           Cardano.Wallet.API.Request (RequestParams (..))
import           Cardano.Wallet.API.Request.Filter (FilterOperations (..))
import           Cardano.Wallet.API.Request.Pagination (Page (..),
                     PaginationMetadata (..), PaginationParams (..),
                     PerPage (..))
import           Cardano.Wallet.API.Request.Sort (SortOperations (..))
import           Cardano.Wallet.API.Response.Filter.IxSet as FilterBackend
import           Cardano.Wallet.API.Response.JSend (HasDiagnostic (..),
                     ResponseStatus (..))
import           Cardano.Wallet.API.Response.Sort.IxSet as SortBackend
import           Cardano.Wallet.API.V1.Errors (ToServantError (..))
import           Cardano.Wallet.API.V1.Generic (jsendErrorGenericParseJSON,
                     jsendErrorGenericToJSON)
import           Pos.Util.Servant as ServantUtil

data SliceOf a = SliceOf {
    paginatedSlice :: [a]
  -- ^ A paginated fraction of the resource
  , paginatedTotal :: Int
  -- ^ The total number of entries
  }

instance Arbitrary a => Arbitrary (SliceOf a) where
  arbitrary = SliceOf <$> arbitrary <*> arbitrary

-- | Inefficient function to build a response out of a @generator@ function. When the data layer will
-- be rewritten the obvious solution is to slice & dice the data as soon as possible (aka out of the DB), in this order:
--
-- 1. Query/Filtering operations (affects the number of total entries for pagination);
-- 2. Sorting operations
-- 3. Pagination
--
-- See also <https://specs.openstack.org/openstack/api-wg/guidelines/pagination_filter_sort.html this document>, which
-- states:
-- "Paginating responses should be done after applying the filters in a query, because it’s possible for there
-- to be no matches in the first page of results, and returning an empty page is a poor API when the user explicitly
-- requested a number of results."
--
-- NOTE: We have chosen have an approach such that we are sorting the whole dataset after filtering and using
-- lazyness to avoid work. This might not be optimal in terms of performances and we might need to swap sorting
-- and pagination.
--
respondWith :: (Monad m, Indexable a)
            => RequestParams
            -> FilterOperations ixs a
            -- ^ Filtering operations to perform on the data.
            -> SortOperations a
            -- ^ Sorting operations to perform on the data.
            -> m (IxSet a)
            -- ^ The monadic action which produces the results.
            -> m (WalletResponse [a])
respondWith RequestParams{..} fops sorts generator = do
    (theData, paginationMetadata) <- paginate rpPaginationParams . sortData sorts . applyFilters fops <$> generator
    return WalletResponse {
             wrData = theData
           , wrStatus = SuccessStatus
           , wrMeta = Metadata paginationMetadata
           }

paginate :: PaginationParams -> [a] -> ([a], PaginationMetadata)
paginate params@PaginationParams{..} rawResultSet =
    let totalEntries = length rawResultSet
        (PerPage pp) = ppPerPage
        (Page cp)    = ppPage
        metadata     = paginationParamsToMeta params totalEntries
        slice        = take pp . drop ((cp - 1) * pp)
    in (slice rawResultSet, metadata)

paginationParamsToMeta :: PaginationParams -> Int -> PaginationMetadata
paginationParamsToMeta PaginationParams{..} totalEntries =
    let perPage@(PerPage pp) = ppPerPage
        currentPage          = ppPage
        totalPages = max 1 $ ceiling (fromIntegral totalEntries / (fromIntegral pp :: Double))
    in PaginationMetadata {
      metaTotalPages = totalPages
    , metaPage = currentPage
    , metaPerPage = perPage
    , metaTotalEntries = totalEntries
    }

fromSlice :: PaginationParams -> SliceOf a -> WalletResponse [a]
fromSlice params (SliceOf theData totalEntries) = WalletResponse {
      wrData   = theData
    , wrStatus = SuccessStatus
    , wrMeta   = Metadata (paginationParamsToMeta params totalEntries)
    }


-- | Creates a 'WalletResponse' with just a single record into it.
single :: a -> WalletResponse a
single theData = WalletResponse {
      wrData   = theData
    , wrStatus = SuccessStatus
    , wrMeta   = Metadata (PaginationMetadata 1 (Page 1) (PerPage 1) 1)
    }

--
-- Creating a better user experience when it comes to errors.
--

--
-- Error from parsing / validating JSON inputs
--

newtype JSONValidationError
    = JSONValidationFailed Text
    deriving (Eq, Show, Generic)

deriveGeneric ''JSONValidationError

instance ToJSON JSONValidationError where
    toJSON =
        jsendErrorGenericToJSON

instance FromJSON JSONValidationError where
    parseJSON =
        jsendErrorGenericParseJSON

instance Exception JSONValidationError

instance Arbitrary JSONValidationError where
    arbitrary =
        pure (JSONValidationFailed "JSON validation failed.")

instance Buildable JSONValidationError where
    build _ =
        bprint "Couldn't decode a JSON input."

instance HasDiagnostic JSONValidationError where
    getDiagnosticKey _ =
        "validationError"

instance ToServantError JSONValidationError where
    declareServantError _ =
        err400


newtype UnsupportedMimeTypeError
    = UnsupportedMimeTypePresent Text
    deriving (Eq, Show, Generic)

deriveGeneric ''UnsupportedMimeTypeError

instance ToJSON UnsupportedMimeTypeError where
    toJSON =
        jsendErrorGenericToJSON

instance FromJSON UnsupportedMimeTypeError where
    parseJSON =
        jsendErrorGenericParseJSON

instance Exception UnsupportedMimeTypeError

instance Arbitrary UnsupportedMimeTypeError where
    arbitrary =
        pure (UnsupportedMimeTypePresent "Delivered MIME-type is not supported.")

instance Buildable UnsupportedMimeTypeError where
    build (UnsupportedMimeTypePresent txt) =
        bprint build txt

instance HasDiagnostic UnsupportedMimeTypeError where
    getDiagnosticKey _ =
        "mimeContentTypeError"

instance ToServantError UnsupportedMimeTypeError where
    declareServantError _ =
        err415
