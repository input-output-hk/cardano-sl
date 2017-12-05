{-# LANGUAGE DeriveGeneric #-}
module Cardano.Wallet.API.Response (
    Metadata (..)
  , OneOf(..)
  , ExtendedResponse(..)
  , respondWith
  ) where

import           Prelude

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Foldable
import           GHC.Generics (Generic)
import qualified Serokell.Aeson.Options as Serokell
import           Test.QuickCheck

import           Cardano.Wallet.API.Request (RequestParams (..), ResponseFormat (..))
import           Cardano.Wallet.API.Request.Pagination (Page (..), PaginationMetadata (..),
                                                        PaginationParams (..), PerPage (..))

-- | Extra information associated with an HTTP response.
data Metadata = Metadata
  { metaPagination   :: PaginationMetadata
    -- ^ Pagination-specific metadata
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''Metadata

instance Arbitrary Metadata where
  arbitrary = Metadata <$> arbitrary

-- | An `ExtendedResponse` allows the consumer of the API to ask for
-- more than simply the result of the RESTful endpoint, but also for
-- extra informations like pagination parameters etc.
data ExtendedResponse a = ExtendedResponse
  { extData :: a        -- ^ The wrapped domain object.
  , extMeta :: Metadata -- ^ Extra metadata to be returned.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''ExtendedResponse

instance Arbitrary a => Arbitrary (ExtendedResponse a) where
  arbitrary = ExtendedResponse <$> arbitrary <*> arbitrary

-- | `PaginationParams` is datatype which combines request params related
-- to pagination together


-- | Type introduced to mimick Swagger 3.0 'oneOf' keyword. It's used to model responses whose body can change
-- depending from some query or header parameters. In this context, this represents an HTTP Response which can
-- return the wrapped object OR the ExtendedResponse.
newtype OneOf a b = OneOf { oneOf :: Either a b } deriving (Show, Eq, Generic)

instance (ToJSON a, ToJSON b) => ToJSON (OneOf a b) where
  toJSON (OneOf (Left x))  = toJSON x -- Simply "unwrap" the type.
  toJSON (OneOf (Right x)) = toJSON x -- Simply "unwrap" the type.

instance (Arbitrary a, Arbitrary b) => Arbitrary (OneOf a b) where
  arbitrary = OneOf <$> oneof [ fmap Left  (arbitrary :: Gen a)
                              , fmap Right (arbitrary :: Gen b)]


-- | Inefficient function to build a response out of a @generator@ function. When the data layer will
-- be rewritten the obvious solution is to slice & dice the data as soon as possible (aka out of the DB), in this order:
--
-- 1. Query/Filtering operations (affects the number of total entries for pagination);
-- 2. Pagination
-- 3. Sorting operations
--
-- See only <https://specs.openstack.org/openstack/api-wg/guidelines/pagination_filter_sort.html this document> which
-- states:
-- "Paginating responses should be done after applying the filters in a query, because it’s possible for there
-- to be no matches in the first page of results, and returning an empty page is a poor API when the user explicitly
-- requested a number of results."
--
-- TODO(adinapoli): Sorting & filtering to be provided by CSL-2016.
respondWith :: (Foldable f, Monad m)
            => RequestParams
            -> (RequestParams -> m (f a))
            -> m (OneOf [a] (ExtendedResponse [a]))
respondWith params@RequestParams{..} generator = do
  (theData, paginationMetadata) <- paginate rpPaginationParams <$> generator params
  case rpResponseFormat of
    Extended -> return $ OneOf $ Right $
      ExtendedResponse {
        extData = theData
      , extMeta = Metadata paginationMetadata
      }
    _ -> return $ OneOf $ Left theData


paginate :: Foldable f => PaginationParams -> f a -> ([a], PaginationMetadata)
paginate PaginationParams{..} rawResultSet =
    let totalEntries = length rawResultSet
        perPage@(PerPage pp)   = ppPerPage
        currentPage@(Page cp)  = ppPage
        totalPages             = ceiling (fromIntegral totalEntries / (fromIntegral pp :: Double))
        metadata               = PaginationMetadata {
                                 metaTotalPages = totalPages
                               , metaPage = currentPage
                               , metaPerPage = perPage
                               , metaTotalEntries = totalEntries
                               }
        slice                  = take pp . drop ((cp - 1) * pp) . toList
    in (slice rawResultSet, metadata)
