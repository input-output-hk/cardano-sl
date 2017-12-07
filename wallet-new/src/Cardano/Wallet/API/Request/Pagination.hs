{- | Support for resource pagination.
-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
module Cardano.Wallet.API.Request.Pagination (
      Page(..)
    , PerPage(..)
    , maxPerPageEntries
    , defaultPerPageEntries
    , PaginationMetadata(..)
    , PaginationParams(..)
    ) where

import           Universum

import           Data.Aeson.TH
import           Data.Default
import qualified Serokell.Aeson.Options as Serokell
import           Test.QuickCheck (Arbitrary (..), choose, getPositive)
import           Web.HttpApiData

-- | A `Page` is used in paginated endpoints to request access to a particular
-- subset of a collection.
newtype Page = Page Int
             deriving (Show, Eq, Ord, Num)

deriveJSON Serokell.defaultOptions ''Page

instance Arbitrary Page where
  arbitrary = Page . getPositive <$> arbitrary

instance FromHttpApiData Page where
    parseQueryParam qp = case parseQueryParam qp of
        Right (p :: Int) | p < 1 -> Left "A page number cannot be less than 1."
        Right (p :: Int) -> Right (Page p)
        Left e           -> Left e

instance ToHttpApiData Page where
    toQueryParam (Page p) = fromString (show p)

-- | If not specified otherwise, return first page.
instance Default Page where
    def = Page 1

-- | A `PerPage` is used to specify the number of entries which should be returned
-- as part of a paginated response.
newtype PerPage = PerPage Int
                deriving (Show, Eq, Num, Ord)

deriveJSON Serokell.defaultOptions ''PerPage

-- | The maximum number of entries a paginated request can return on a single call.
-- This value is currently arbitrary and it might need to be tweaked down to strike
-- the right balance between number of requests and load of each of them on the system.
maxPerPageEntries :: Int
maxPerPageEntries = 50

-- | If not specified otherwise, a default number of 10 entries from the collection will
-- be returned as part of each paginated response.
defaultPerPageEntries :: Int
defaultPerPageEntries = 10

instance Arbitrary PerPage where
  arbitrary = PerPage <$> choose (1, maxPerPageEntries)

instance FromHttpApiData PerPage where
    parseQueryParam qp = case parseQueryParam qp of
        Right (p :: Int) | p < 1 -> Left "per_page should be at least 1."
        Right (p :: Int) | p > maxPerPageEntries ->
                           Left $ fromString $ "per_page cannot be greater than " <> show maxPerPageEntries <> "."
        Right (p :: Int) -> Right (PerPage p)
        Left e           -> Left e

instance ToHttpApiData PerPage where
    toQueryParam (PerPage p) = fromString (show p)

instance Default PerPage where
    def = PerPage defaultPerPageEntries

-- | Extra information associated with pagination
data PaginationMetadata = PaginationMetadata
  { metaTotalPages   :: Int     -- ^ The total pages returned by this query.
  , metaPage         :: Page    -- ^ The current page number (index starts at 1).
  , metaPerPage      :: PerPage -- ^ The number of entries contained in this page.
  , metaTotalEntries :: Int     -- ^ The total number of entries in the collection.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''PaginationMetadata

instance Arbitrary PaginationMetadata where
  arbitrary = PaginationMetadata <$> fmap getPositive arbitrary
                                 <*> arbitrary
                                 <*> arbitrary
                                 <*> fmap getPositive arbitrary

-- | `PaginationParams` is datatype which combines request params related
-- to pagination together.
data PaginationParams = PaginationParams
    { ppPage    :: Page
    , ppPerPage :: PerPage
    } deriving (Show, Eq, Generic)
