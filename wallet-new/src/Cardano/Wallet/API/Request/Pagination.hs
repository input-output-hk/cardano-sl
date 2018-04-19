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

import           Control.Lens (at, ix, (?~))
import           Data.Aeson (Value (Number))
import           Data.Aeson.TH
import qualified Data.Char as Char
import           Data.Default
import           Data.Swagger as S
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
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

instance ToSchema Page where
    declareNamedSchema =
        pure . NamedSchema (Just "Page") . paramSchemaToSchema

instance ToParamSchema Page where
  toParamSchema _ = mempty
    & type_ .~ SwaggerInteger
    & default_ ?~ (Number 1) -- Always show the first page by default.
    & minimum_ ?~ 1

-- | If not specified otherwise, return first page.
instance Default Page where
    def = Page 1

instance Buildable Page where
  build (Page p) = bprint build p

-- | A `PerPage` is used to specify the number of entries which should be returned
-- as part of a paginated response.
newtype PerPage = PerPage Int
                deriving (Show, Eq, Num, Ord)

deriveJSON Serokell.defaultOptions ''PerPage

instance ToSchema PerPage where
    declareNamedSchema =
        pure . NamedSchema (Just "PerPage") . paramSchemaToSchema

instance ToParamSchema PerPage where
  toParamSchema _ = mempty
    & type_ .~ SwaggerInteger
    & default_ ?~ (Number $ fromIntegral defaultPerPageEntries)
    & minimum_ ?~ 1
    & maximum_ ?~ (fromIntegral maxPerPageEntries)
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

instance Buildable PerPage where
  build (PerPage p) = bprint build p

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

instance ToSchema PaginationMetadata where
  declareNamedSchema proxy = do
      schm <- genericDeclareNamedSchema defaultSchemaOptions
        { S.fieldLabelModifier =
            over (ix 0) Char.toLower . drop 4 -- length "meta"
        } proxy
      pure $ over schema (over properties adjustPropsSchema) schm
    where
      totalSchema = Inline $ mempty
        & type_ .~ SwaggerNumber
        & minimum_ ?~ 0
        & maximum_ ?~ fromIntegral (maxBound :: Int)
      adjustPropsSchema s = s
        & at "totalPages" ?~ totalSchema
        & at "totalEntries" ?~ totalSchema

instance Buildable PaginationMetadata where
    build PaginationMetadata{..} =
        bprint (build%"/"%build%" total="%build%" per_page="%build)
            metaPage
            metaTotalPages
            metaTotalEntries
            metaPerPage

-- | `PaginationParams` is datatype which combines request params related
-- to pagination together.
data PaginationParams = PaginationParams
    { ppPage    :: Page
    , ppPerPage :: PerPage
    } deriving (Show, Eq, Generic)

instance Buildable PaginationParams where
    build PaginationParams{..} =
      bprint ("page="%build%", per_page="%build)
          ppPage
          ppPerPage
