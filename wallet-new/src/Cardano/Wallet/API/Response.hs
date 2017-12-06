{-# LANGUAGE DeriveGeneric #-}
module Cardano.Wallet.API.Response (
    Metadata (..)
  , ResponseStatus(..)
  , WalletResponse(..)
  -- * Generating responses for collections
  , respondWith
  -- * Generating responses for single resources
  , single
  ) where

import           Prelude

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Char as Char
import           Data.Foldable
import           GHC.Generics (Generic)
import qualified Serokell.Aeson.Options as Serokell
import           Test.QuickCheck

import           Cardano.Wallet.API.Request (RequestParams (..))
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

data ResponseStatus =
      SuccessStatus
    | FailStatus
    | ErrorStatus
    deriving (Show, Eq, Ord, Enum, Bounded)

deriveJSON defaultOptions { constructorTagModifier = map Char.toLower . reverse . drop 6 . reverse } ''ResponseStatus

instance Arbitrary ResponseStatus where
    arbitrary = elements [minBound .. maxBound]

-- | An `WalletResponse` models, unsurprisingly, a response (successful or not)
-- produced by the wallet backend.
-- Includes extra informations like pagination parameters etc.
data WalletResponse a = WalletResponse
  { resData   :: a
  -- ^ The wrapped domain object.
  , resStatus :: ResponseStatus
  -- ^ The <https://labs.omniti.com/labs/jsend jsend> status.
  , resMeta   :: Metadata
  -- ^ Extra metadata to be returned.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''WalletResponse

instance Arbitrary a => Arbitrary (WalletResponse a) where
  arbitrary = WalletResponse <$> arbitrary <*> arbitrary <*> arbitrary

-- | Inefficient function to build a response out of a @generator@ function. When the data layer will
-- be rewritten the obvious solution is to slice & dice the data as soon as possible (aka out of the DB), in this order:
--
-- 1. Query/Filtering operations (affects the number of total entries for pagination);
-- 2. Pagination
-- 3. Sorting operations
--
-- See also <https://specs.openstack.org/openstack/api-wg/guidelines/pagination_filter_sort.html this document>, which
-- states:
-- "Paginating responses should be done after applying the filters in a query, because it’s possible for there
-- to be no matches in the first page of results, and returning an empty page is a poor API when the user explicitly
-- requested a number of results."
--
-- TODO(adinapoli): Sorting & filtering to be provided by CSL-2016.
respondWith :: (Foldable f, Monad m)
            => RequestParams
            -> (RequestParams -> m (f a))
            -- ^ A callback-style function which, given the full set of `RequestParams`
            -- produces some form of results in some 'Monad' @m@.
            -> m (WalletResponse [a])
respondWith params@RequestParams{..} generator = do
    (theData, paginationMetadata) <- paginate rpPaginationParams <$> generator params
    return $ WalletResponse {
             resData = theData
           , resStatus = SuccessStatus
           , resMeta = Metadata paginationMetadata
           }


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


-- | Creates a 'WalletResponse' with just a single record into it.
single :: a -> WalletResponse a
single theData = WalletResponse {
      resData = theData
    , resStatus = SuccessStatus
    , resMeta = Metadata (PaginationMetadata 1 (Page 1) (PerPage 1) 1)
    }
