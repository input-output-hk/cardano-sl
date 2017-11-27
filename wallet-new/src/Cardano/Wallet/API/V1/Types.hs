{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Wallet.API.V1.Types (
  -- * Swagger & REST-related types
    ExtendedResponse (..)
  , Metadata (..)
  , Page(..)
  , PerPage(..)
  , ResponseFormat (..)
  , PaginationParams (..)
  , maxPerPageEntries
  , defaultPerPageEntries
  , OneOf (..)
  , PasswordUpdate (..)
  , AccountUpdate (..)
  , Update
  , New
  -- * Domain-specific types
  -- * Wallets
  , Wallet (..)
  , AssuranceLevel (..)
  , NewWallet (..)
  , WalletUpdate (..)
  , WalletId (..)
  , SpendingPassword
  -- * Accounts
  , Account (..)
  , AccountId
  -- * Payments
  , Payment (..)
  , PaymentDistribution (..)
  , Transaction (..)
  , TransactionType (..)
  , TransactionDirection (..)
  , TransactionGroupingPolicy (..)
  , EstimatedFees (..)
  -- * Updates
  , WalletSoftwareUpdate (..)
  -- * Core re-exports
  , Core.Address
  ) where

import           Universum

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Char as C
import           Data.Default (Default (def))
import           Data.Text (Text, dropEnd, toLower)
import qualified Data.Text.Buildable
import           Formatting (build, sformat)
import           GHC.Generics (Generic)
import qualified Serokell.Aeson.Options as Serokell
import           Test.QuickCheck
import           Web.HttpApiData

import           Cardano.Wallet.Orphans.Aeson ()

-- V0 logic
import           Pos.Util.BackupPhrase (BackupPhrase)

import           Pos.Aeson.Core ()
import           Pos.Arbitrary.Core ()
import qualified Pos.Core.Types as Core
import qualified Pos.Crypto.Signing as Core

--
-- Swagger & REST-related types
--

-- | A `Page` is used in paginated endpoints to request access to a particular
-- subset of a collection.
newtype Page = Page Int
             deriving (Show, Eq, Ord, Num)

deriveJSON Serokell.defaultOptions ''Page

instance Arbitrary Page where
  arbitrary = Page <$> fmap getPositive arbitrary

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

-- | Extra information associated with an HTTP response.
data Metadata = Metadata
  { metaTotalPages   :: Int     -- ^ The total pages returned by this query.
  , metaPage         :: Page    -- ^ The current page number (index starts at 1).
  , metaPerPage      :: PerPage -- ^ The number of entries contained in this page.
  , metaTotalEntries :: Int     -- ^ The total number of entries in the collection.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''Metadata

instance Arbitrary Metadata where
  arbitrary = Metadata <$> fmap getPositive arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> fmap getPositive arbitrary

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

-- | `PaginationParams` is datatype which combines request params related
-- to pagination together

data PaginationParams = PaginationParams
    { ppPage           :: Page
    , ppPerPage        :: PerPage
    , ppResponseFormat :: ResponseFormat
    } deriving (Show, Eq, Generic)

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
-- Domain-specific types, mostly placeholders.
--

-- | A 'SpendingPassword' represent a secret piece of information which can be
-- optionally supplied by the user to encrypt the private keys. As private keys
-- are needed to spend funds and this password secures spending, here the name
-- 'SpendingPassword'.
-- Practically speaking, it's just a type synonym for a PassPhrase, which is a
-- base16-encoded string.
type SpendingPassword = Core.PassPhrase

type WalletName = Text

data AssuranceLevel =
    NormalAssurance
  | StrictAssurance
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary AssuranceLevel where
    arbitrary = elements [minBound .. maxBound]

deriveJSON Serokell.defaultOptions { constructorTagModifier = toString . toLower . dropEnd 9 . fromString
                                   } ''AssuranceLevel

-- | A Wallet ID.
newtype WalletId = WalletId Text deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''WalletId

instance Arbitrary WalletId where
  arbitrary =
      let wid = "J7rQqaLLHBFPrgJXwpktaMB1B1kQBXAyc2uRSfRPzNVGiv6TdxBzkPNBUWysZZZdhFG9gRy3sQFfX5wfpLbi4XTFGFxTg"
          in WalletId . fromString <$> elements [wid]

instance FromHttpApiData WalletId where
    parseQueryParam = Right . WalletId

instance ToHttpApiData WalletId where
    toQueryParam (WalletId wid) = wid

-- | A type modelling the request for a new 'Wallet'.
data NewWallet = NewWallet {
      newwalBackupPhrase     :: !BackupPhrase
    -- ^ The backup phrase to restore the wallet.
    , newwalSpendingPassword :: !(Maybe SpendingPassword)
    -- ^ The spending password to encrypt the private keys.
    , newwalAssuranceLevel   :: !AssuranceLevel
    , newwalName             :: !WalletName
    } deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions  ''NewWallet

instance Arbitrary NewWallet where
  arbitrary = NewWallet <$> arbitrary
                        <*> pure Nothing
                        <*> arbitrary
                        <*> pure "My Wallet"

-- | A type modelling the update of an existing wallet.
data WalletUpdate = WalletUpdate {
      uwalAssuranceLevel :: !AssuranceLevel
    , uwalName           :: !Text
    } deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions  ''WalletUpdate

instance Arbitrary WalletUpdate where
  arbitrary = WalletUpdate <$> arbitrary
                           <*> pure "My Wallet"

-- | A 'Wallet'.
data Wallet = Wallet {
      walId      :: !WalletId
    , walName    :: !WalletName
    , walBalance :: !Core.Coin
    } deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions ''Wallet

instance Arbitrary Wallet where
  arbitrary = Wallet <$> arbitrary
                     <*> pure "My wallet"
                     <*> arbitrary

type AccountId = Text

-- | A wallet's 'Account'.
data Account = Account
  { accId        :: !AccountId
  , accAddresses :: [Core.Address]
  , accAmount    :: !Core.Coin
  , accName      :: !Text
  -- ^ The Account name.
  , accWalletId  :: WalletId
  -- ^ The 'WalletId' this 'Account' belongs to.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''Account

instance Arbitrary Account where
  arbitrary = Account . fromString <$> elements ["DEADBeef", "123456"]
                                   <*> listOf1 arbitrary
                                   <*> arbitrary
                                   <*> pure "My account"
                                   <*> arbitrary

data AccountUpdate = AccountUpdate {
    uaccName      :: !Text
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''AccountUpdate

instance Arbitrary AccountUpdate where
  arbitrary = AccountUpdate . fromString <$> pure "myAccount"

-- | A type incapsulating a password update request.
data PasswordUpdate = PasswordUpdate {
    pwdOld :: !SpendingPassword
    -- ^ The old 'SpendingPassword'.
  , pwdNew :: !SpendingPassword
    -- ^ The new 'SpendingPassword'.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''PasswordUpdate

instance Arbitrary PasswordUpdate where
  arbitrary = PasswordUpdate <$> arbitrary
                             <*> arbitrary

-- | 'EstimatedFees' represents the fees which would be generated
-- for a 'Payment' in case the latter would actually be performed.
data EstimatedFees = EstimatedFees {
    feeEstimatedAmount :: !Core.Coin
    -- ^ The estimated fees, as coins.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''EstimatedFees

instance Arbitrary EstimatedFees where
  arbitrary = EstimatedFees <$> arbitrary

-- | Maps an 'Address' to some 'Coin's, and it's
-- typically used to specify where to send money during a 'Payment'.
data PaymentDistribution = PaymentDistribution {
      pdAddress :: Core.Address
    , pdAmount  :: Core.Coin
    } deriving (Show, Eq)

deriveJSON Serokell.defaultOptions ''PaymentDistribution

instance Arbitrary PaymentDistribution where
  arbitrary = PaymentDistribution <$> arbitrary
                                  <*> arbitrary

-- | A policy to be passed to each new payment request to
-- determine how a 'Transaction' is assembled.
data TransactionGroupingPolicy =
    OptimiseForSizePolicy
  -- ^ Tries to minimise the size of the created transaction
  -- by choosing only the biggest value available up until
  -- the stake sum is greater or equal the payment amount.
  | OptimiseForSecurityPolicy
  -- ^ Tries to minimise the number of addresses left with
  -- unspent funds after the transaction has been created.
  deriving (Show, Ord, Eq, Enum, Bounded)

instance Arbitrary TransactionGroupingPolicy where
  arbitrary = elements [minBound .. maxBound]

-- Drops the @Policy@ suffix.
deriveJSON defaultOptions { constructorTagModifier = reverse . drop 6 . reverse } ''TransactionGroupingPolicy

-- | A 'Payment' from one source account to one or more 'PaymentDistribution'(s).
data Payment = Payment
  { pmtSourceWallet   :: !WalletId
    -- ^ The source Wallet.
  , pmtSourceAccount  :: !AccountId
    -- ^ The source Account.
  , pmtDestinations   :: !(NonEmpty PaymentDistribution)
    -- ^ The destinations for this payment.
  , pmtGroupingPolicy :: !(Maybe TransactionGroupingPolicy)
    -- ^ Which strategy use in grouping the input transactions.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''Payment

instance Arbitrary Payment where
  arbitrary = Payment <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary

type TxId = Text

-- | The 'Transaction' type.
data TransactionType =
    LocalTransaction
  -- ^ This transaction is local, which means all the inputs
  -- and all the outputs belongs to the wallet from which the
  -- transaction was originated.
  | ForeignTransaction
  -- ^ This transaction is not local to this wallet.
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary TransactionType where
  arbitrary = elements [minBound .. maxBound]

-- Drops the @Transaction@ suffix.
deriveJSON defaultOptions { constructorTagModifier = reverse . drop 11 . reverse . map C.toLower
                          } ''TransactionType

-- | The 'Transaction' @direction@
data TransactionDirection =
    IncomingTransaction
  -- ^ This represents an incoming transactions.
  | OutgoingTransaction
  -- ^ This qualifies external transactions.
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary TransactionDirection where
  arbitrary = elements [minBound .. maxBound]

-- Drops the @Transaction@ suffix.
deriveJSON defaultOptions { constructorTagModifier = reverse . drop 11 . reverse . map C.toLower
                          } ''TransactionDirection

-- | A 'Wallet''s 'Transaction'.
data Transaction = Transaction
  { txId            :: !TxId
    -- ^ The Tx Id.
  , txConfirmations :: !Word
    -- ^ The number of confirmations.
  , txAmount        :: !Core.Coin
    -- ^ The 'Coin' moved as part of this transaction.
  , txInputs        :: !(NonEmpty PaymentDistribution)
    -- ^ The input money distribution.
  , txOutputs       :: !(NonEmpty PaymentDistribution)
    -- ^ The output money distribution.
  , txType          :: TransactionType
    -- ^ The type for this transaction (e.g local, foreign, etc).
  , txDirection     :: TransactionDirection
    -- ^ The direction for this transaction (e.g incoming, outgoing).
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''Transaction

instance Arbitrary Transaction where
  arbitrary = Transaction <$> fmap fromString arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary

-- | A type representing an upcoming wallet update.
data WalletSoftwareUpdate = WalletSoftwareUpdate
  { updSoftwareVersion   :: !Text
  , updBlockchainVersion :: !Text
  , updScriptVersion     :: !Int
  -- Other types omitted for now.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''WalletSoftwareUpdate

instance Arbitrary WalletSoftwareUpdate where
  arbitrary = WalletSoftwareUpdate <$> fmap fromString arbitrary
                                   <*> fmap fromString arbitrary
                                   <*> fmap getPositive arbitrary

--
-- POST/PUT requests isomorphisms
--

type family Update (original :: *) :: * where
  Update Wallet  = WalletUpdate
  Update Account = AccountUpdate

type family New (original :: *) :: * where
  New Wallet  = NewWallet
  New Account = AccountUpdate -- POST == PUT
