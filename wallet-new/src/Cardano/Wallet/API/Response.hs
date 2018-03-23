{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedLists #-}
module Cardano.Wallet.API.Response (
    Metadata (..)
  , ResponseStatus(..)
  , WalletResponse(..)
  -- * Generating responses for collections
  , respondWith
  -- * Generating responses for single resources
  , single
  , ValidJSON
  ) where

import           Prelude
import           Universum (Buildable, decodeUtf8, toText, (<>))

import           Cardano.Wallet.API.Response.JSend (ResponseStatus (..))
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.TH
import qualified Data.Char as Char
import           Data.Swagger as S
import qualified Data.Text.Buildable
import           Data.Typeable
import           Formatting (bprint, build, (%))
import           GHC.Generics (Generic)
import qualified Serokell.Aeson.Options as Serokell
import           Servant.API.ContentTypes (Accept (..), JSON, MimeRender (..), MimeUnrender (..),
                                           OctetStream)
import           Test.QuickCheck

import           Cardano.Wallet.API.Indices (Indexable', IxSet')
import           Cardano.Wallet.API.Request (RequestParams (..))
import           Cardano.Wallet.API.Request.Filter (FilterOperations (..))
import           Cardano.Wallet.API.Request.Pagination (Page (..), PaginationMetadata (..),
                                                        PaginationParams (..), PerPage (..))
import           Cardano.Wallet.API.Request.Sort (SortOperations (..))
import           Cardano.Wallet.API.Response.Filter.IxSet as FilterBackend
import           Cardano.Wallet.API.Response.Sort.IxSet as SortBackend
import           Cardano.Wallet.API.V1.Errors (WalletError (JSONValidationFailed))

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
respondWith :: (Monad m, Indexable' a)
            => RequestParams
            -> FilterOperations a
            -- ^ Filtering operations to perform on the data.
            -> SortOperations a
            -- ^ Sorting operations to perform on the data.
            -> m (IxSet' a)
            -- ^ The monadic action which produces the results.
            -> m (WalletResponse [a])
respondWith RequestParams{..} fops sorts generator = do
    (theData, paginationMetadata) <- paginate rpPaginationParams . sortData sorts . applyFilters fops <$> generator
    return $ WalletResponse {
             wrData = theData
           , wrStatus = SuccessStatus
           , wrMeta = Metadata paginationMetadata
           }

paginate :: PaginationParams -> [a] -> ([a], PaginationMetadata)
paginate PaginationParams{..} rawResultSet =
    let totalEntries = length rawResultSet
        perPage@(PerPage pp)   = ppPerPage
        currentPage@(Page cp)  = ppPage
        totalPages             = max 1 $ ceiling (fromIntegral totalEntries / (fromIntegral pp :: Double))
        metadata               = PaginationMetadata {
                                 metaTotalPages = totalPages
                               , metaPage = currentPage
                               , metaPerPage = perPage
                               , metaTotalEntries = totalEntries
                               }
        slice                  = take pp . drop ((cp - 1) * pp)
    in (slice rawResultSet, metadata)


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
