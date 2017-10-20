{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Wallet.API.V1.Types (
  -- * Swagger & REST-related types
    ExtendedResponse (..)
  , Metadata (..)
  , Page(..)
  , PerPage(..)
  , maxPerPageEntries
  , defaultPerPageEntries
  , OneOf (..)
  -- * Error handling
  , WalletError (..)
  -- * Domain-specific types
  , WalletId (..)
  , Account (..)
  , Address (..)
  ) where

import           Universum

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text       (Text)
import           GHC.Generics    (Generic)
import           Test.QuickCheck
import           Web.HttpApiData

--
-- Swagger & REST-related types
--

-- | A `Page` is used in paginated endpoints to request access to a particular
-- subset of a collection.
newtype Page = Page Int
             deriving (Show, Eq, Ord, Num)

deriveJSON defaultOptions ''Page

instance Arbitrary Page where
  arbitrary = Page <$> fmap getPositive arbitrary

instance FromHttpApiData Page where
    parseQueryParam qp = case parseQueryParam qp of
        Right (p :: Int) | p < 1 -> Left "A page number cannot be less than 1."
        Right (p :: Int) -> Right (Page p)
        Left e           -> Left e

instance ToHttpApiData Page where
    toQueryParam (Page p) = fromString (show p)

-- | A `PerPage` is used to specify the number of entries which should be returned
-- as part of a paginated response.
newtype PerPage = PerPage Int
                deriving (Show, Eq, Num, Ord)

deriveJSON defaultOptions ''PerPage

-- | The maximum number of entries a paginated request can return on a single call.
-- This value is currently arbitrary and it might need to be tweaked down to strike
-- the right balance between number of requests and load of each of them on the system.
maxPerPageEntries :: Int
maxPerPageEntries = 500

-- | If not specified otherwise, a default number of 10 entries from the collection will
-- be returned as part of each paginated response.
defaultPerPageEntries :: Int
defaultPerPageEntries = 10

instance Arbitrary PerPage where
  arbitrary = PerPage <$> choose (1, 500)

instance FromHttpApiData PerPage where
    parseQueryParam qp = case parseQueryParam qp of
        Right (p :: Int) | p < 1 -> Left "per_page should be at least 1."
        Right (p :: Int) | p > maxPerPageEntries ->
                           Left $ fromString $ "per_page cannot be greater than " <> show maxPerPageEntries <> "."
        Right (p :: Int) -> Right (PerPage p)
        Left e           -> Left e

instance ToHttpApiData PerPage where
    toQueryParam (PerPage p) = fromString (show p)

-- | Extra information associated with an HTTP response.
data Metadata = Metadata
  { meta_total_pages   :: Int     -- ^ The total pages returned by this query.
  , meta_page          :: Page    -- ^ The current page number (index starts at 1).
  , meta_per_page      :: PerPage -- ^ The number of entries contained in this page.
  , meta_total_entries :: Int     -- ^ The total number of entries in the collection.
  } deriving (Show, Eq, Generic)

deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''Metadata

instance Arbitrary Metadata where
  arbitrary = Metadata <$> fmap getPositive arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> fmap getPositive arbitrary

-- | An `ExtendedResponse` allows the consumer of the API to ask for
-- more than simply the result of the RESTful endpoint, but also for
-- extra informations like pagination parameters etc.
data ExtendedResponse a = ExtendedResponse
  { ext_data :: a        -- ^ The wrapped domain object.
  , ext_meta :: Metadata -- ^ Extra metadata to be returned.
  } deriving (Show, Eq, Generic)

deriveJSON defaultOptions { fieldLabelModifier = drop 4 } ''ExtendedResponse

instance Arbitrary a => Arbitrary (ExtendedResponse a) where
  arbitrary = ExtendedResponse <$> arbitrary <*> arbitrary

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

--
-- Error handling
--

-- | Models a Wallet Error as a Jsend <https://labs.omniti.com/labs/jsend> response.
data WalletError = WalletError
  { err_code    :: !Int
  , err_message :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON WalletError where
    toJSON WalletError{..} = object [ "code"    .= toJSON err_code
                                    , "status"  .= String "error"
                                    , "message" .= toJSON err_message
                                    ]

instance Arbitrary WalletError where
    arbitrary = WalletError <$> choose (1,999) <*> pure "The given AccountId is not correct."

--
-- Domain-specific types, mostly placeholders.
--

newtype WalletId = WalletId Text deriving (Show, Eq, Generic)

deriveJSON defaultOptions ''WalletId

instance Arbitrary WalletId where
  arbitrary = WalletId . fromString <$> elements ["1Z1F10ADD10F9872"]

instance FromHttpApiData WalletId where
    parseQueryParam = Right . WalletId

-- Placeholder.
newtype Address = Address
  { add_id :: Text -- ^ A base58 Public Key.
  } deriving (Show, Eq, Generic)

deriveJSON defaultOptions { fieldLabelModifier = drop 4 } ''Address

instance Arbitrary Address where
  arbitrary = Address . fromString <$> elements ["DEADBeef", "123456"]

-- | A wallet 'Account'.
data Account = Account
  { acc_id        :: Text
  , acc_addresses :: [Address]
  , acc_amount    :: !Int
  -- | The Account name.
  , acc_name      :: Text
  -- | The parent Wallet Id.
  , acc_wallet_id :: WalletId
  } deriving (Show, Eq, Generic)

deriveJSON defaultOptions { fieldLabelModifier = drop 4 } ''Account

instance Arbitrary Account where
  arbitrary = Account . fromString <$> elements ["DEADBeef", "123456"]
                                   <*> listOf1 arbitrary
                                   <*> fmap getPositive arbitrary
                                   <*> fmap fromString arbitrary
                                   <*> arbitrary
