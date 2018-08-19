{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}
module Cardano.Wallet.API.Response (
    Metadata (..)
  , ResponseStatus(..)
  , WalletResponse(..)
  , JSONValidationError(..)
  -- * Generating responses for collections
  , respondWith
  , sliceList
  , fromSlice
  -- * Generating responses for single resources
  , single

  -- * A slice of a collection
  , SliceOf(..)

  , ValidJSON
  ) where

import           Prelude
import           Universum (Buildable, Exception, Text, decodeUtf8, toText,
                     (<>))

import           Control.Lens hiding (Indexable)
import           Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, encode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Options as Serokell
import           Data.Aeson.TH
import qualified Data.Char as Char
import           Data.Swagger as S hiding (Example, example)
import           Data.Typeable
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Generics.SOP.TH (deriveGeneric)
import           GHC.Generics (Generic)
import           Servant (err400)
import           Servant.API.ContentTypes (Accept (..), JSON, MimeRender (..),
                     MimeUnrender (..), OctetStream)
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
import           Cardano.Wallet.API.V1.Swagger.Example (Example, example)

-- | Extra information associated with an HTTP response.
data Metadata = Metadata
  { metaPagination   :: PaginationMetadata
    -- ^ Pagination-specific metadata
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''Metadata

instance Arbitrary Metadata where
  arbitrary = Metadata <$> arbitrary

instance ToSchema Metadata where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
        { S.fieldLabelModifier =
            over (ix 0) Char.toLower . drop 4 -- length "meta"
        }

instance Buildable Metadata where
  build Metadata{..} =
    bprint ("{ pagination="%build%" }") metaPagination

instance Example Metadata


-- | An `WalletResponse` models, unsurprisingly, a response (successful or not)
-- produced by the wallet backend.
-- Includes extra informations like pagination parameters etc.
data WalletResponse a = WalletResponse
  { wrData   :: a
  -- ^ The wrapped domain object.
  , wrStatus :: ResponseStatus
  -- ^ The <https://labs.omniti.com/labs/jsend jsend> status.
  , wrMeta   :: Metadata
  -- ^ Extra metadata to be returned.
  } deriving (Show, Eq, Generic, Functor)

data SliceOf a = SliceOf {
    paginatedSlice :: [a]
  -- ^ A paginated fraction of the resource
  , paginatedTotal :: Int
  -- ^ The total number of entries
  }

instance Arbitrary a => Arbitrary (SliceOf a) where
  arbitrary = SliceOf <$> arbitrary <*> arbitrary


deriveJSON Serokell.defaultOptions ''WalletResponse

instance Arbitrary a => Arbitrary (WalletResponse a) where
  arbitrary = WalletResponse <$> arbitrary <*> arbitrary <*> arbitrary

instance ToJSON a => MimeRender OctetStream (WalletResponse a) where
    mimeRender _ = encode

instance (ToSchema a, Typeable a) => ToSchema (WalletResponse a) where
    declareNamedSchema _ = do
        let a = Proxy @a
            tyName = toText . show $ typeRep a
        aRef <- declareSchemaRef a
        respRef <- declareSchemaRef (Proxy @ResponseStatus)
        metaRef <- declareSchemaRef (Proxy @Metadata)
        pure $ NamedSchema (Just $ "WalletResponse<" <> tyName <> ">") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["data", "status", "meta"]
            & properties .~
                [ ("data", aRef)
                , ("status", respRef)
                , ("meta", metaRef)
                ]

instance Buildable a => Buildable (WalletResponse a) where
    build WalletResponse{..} = bprint
        ("\n\tstatus="%build
        %"\n\tmeta="%build
        %"\n\tdata="%build
        )
        wrStatus
        wrMeta
        wrData

instance Example a => Example (WalletResponse a) where
    example = WalletResponse <$> example
                             <*> pure SuccessStatus
                             <*> example


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

sliceList :: PaginationParams -> [a] -> SliceOf a
sliceList pp xs = SliceOf {
      paginatedSlice = take perPage . drop (perPage * (currentPage - 1)) $ xs
    , paginatedTotal = length xs
    }
  where
    Page currentPage = ppPage    pp
    PerPage perPage  = ppPerPage pp

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

data ValidJSON deriving Typeable

instance FromJSON a => MimeUnrender ValidJSON a where
    mimeUnrender _ bs = case eitherDecode bs of
        Left err -> Left $ decodeUtf8 $ encodePretty (JSONValidationFailed $ toText err)
        Right v  -> return v

instance Accept ValidJSON where
    contentType _ = contentType (Proxy @ JSON)

instance ToJSON a => MimeRender ValidJSON a where
    mimeRender _ = mimeRender (Proxy @ JSON)


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
