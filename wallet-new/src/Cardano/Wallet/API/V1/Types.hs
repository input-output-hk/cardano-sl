{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- The hlint parser fails on the `pattern` function, so we disable the
-- language extension here.
{-# LANGUAGE NoPatternSynonyms          #-}

-- TODO: Banish NonEmpty orphan when https://github.com/GetShopTV/swagger2/pull/141
-- is merged
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.API.V1.Types (
    V1 (..)
  -- * Swagger & REST-related types
  , PasswordUpdate (..)
  , AccountUpdate (..)
  , NewAccount (..)
  , Update
  , New
  -- * Domain-specific types
  -- * Wallets
  , Wallet (..)
  , AssuranceLevel (..)
  , NewWallet (..)
  , WalletUpdate (..)
  , WalletId (..)
  , WalletOperation (..)
  , SpendingPassword
  -- * Addresses
  , AddressValidity (..)
  -- * Accounts
  , Account (..)
  , AccountIndex
  -- * Addresses
  , WalletAddress (..)
  , NewAddress (..)
  -- * Payments
  , Payment (..)
  , PaymentSource (..)
  , PaymentDistribution (..)
  , Transaction (..)
  , TransactionType (..)
  , TransactionDirection (..)
  , EstimatedFees (..)
  -- * Updates
  , WalletSoftwareUpdate (..)
  -- * Settings
  , NodeSettings (..)
  , SlotDuration
  , mkSlotDuration
  , BlockchainHeight
  , mkBlockchainHeight
  , LocalTimeDifference
  , mkLocalTimeDifference
  , SyncProgress
  , mkSyncProgress
  , NodeInfo (..)
  -- * Core re-exports
  , Core.Address
  ) where

import           Universum

import           Control.Lens (at, from, ix, makePrisms, (?~))
import           Data.Aeson
import           Data.Aeson.TH as A
import           Data.Aeson.Types (typeMismatch)
import qualified Data.Char as C
import           Data.Swagger as S hiding (constructorTagModifier)
import           Data.Swagger.Declare (Declare)
import           Data.Swagger.Internal.Schema (GToSchema)
import           Data.Text (Text, dropEnd, toLower)
import           Data.Version (Version)
import           Formatting (build, int, sformat, (%))
import           GHC.Generics (Generic, Rep)
import qualified Prelude
import qualified Serokell.Aeson.Options as Serokell
import qualified Serokell.Util.Base16 as Base16
import           Test.QuickCheck
import           Web.HttpApiData

import           Cardano.Wallet.API.Types.UnitOfMeasure (MeasuredIn (..), UnitOfMeasure (..))
import           Cardano.Wallet.Orphans.Aeson ()

-- V0 logic
import           Pos.Util.BackupPhrase (BackupPhrase (..))


import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import           Pos.Aeson.Core ()
import           Pos.Arbitrary.Core ()
import qualified Pos.Client.Txp.Util as Core
import           Pos.Core (addressF)
import qualified Pos.Core as Core
import           Pos.Crypto (decodeHash, hashHexF)
import qualified Pos.Crypto.Signing as Core

genericSchemaDroppingPrefix
    :: (Generic a, GToSchema (Rep a))
    => String
    -> proxy a
    -> Declare (Definitions Schema) NamedSchema
genericSchemaDroppingPrefix prfx = genericDeclareNamedSchema defaultSchemaOptions
    { S.fieldLabelModifier =
        over (ix 0) C.toLower . drop (length prfx)
    }
--
-- Versioning
--

-- This deceptively-simple newtype is a wrapper to virtually @all@ the types exposed as
-- part of this API. The reason is twofold:
--
-- 1. We want to version our API, and we want the types to reflect that, without us having
-- to manually write newtype wrappers for all the types.
--
-- 2. Shelter an API from serialisation changes. Across versions of an API types can change,
-- so can they JSON instances. But chances are we might want to reuse most of those for different
-- versions of an API. Think about 'Address' or 'Coin'. Those are core Cardano types we want to
-- probably use for the time being. But their serialisation format can change as it's not defined
-- as part of the API, but in the lower layers of the stack.
--
-- General rules for serialisation:
--
-- 1. Never define an instance on the inner type 'a'. Do it only on 'V1 a'.
newtype V1 a = V1 a deriving (Eq, Ord)

makePrisms ''V1

instance Show a => Show (V1 a) where
    show (V1 a) = Prelude.show a

instance Enum a => Enum (V1 a) where
    toEnum x = V1 (toEnum x)
    fromEnum (V1 a) = fromEnum a

instance Bounded a => Bounded (V1 a) where
    minBound = V1 $ minBound @a
    maxBound = V1 $ maxBound @a

--
-- Benign instances
--

instance ByteArray.ByteArrayAccess a => ByteArray.ByteArrayAccess (V1 a) where
   length (V1 a) = ByteArray.length a
   withByteArray (V1 a) callback = ByteArray.withByteArray a callback

-- TODO(adinapoli) Rewrite it properly under CSL-2048.
instance Arbitrary (V1 BackupPhrase) where
    arbitrary = pure . V1 . BackupPhrase $ [
          "shell"
        , "also"
        , "throw"
        , "ramp"
        , "grape"
        , "chest"
        , "setup"
        , "mandate"
        , "spare"
        , "verb"
        , "lemon"
        , "test"
        ]

instance ToJSON (V1 BackupPhrase) where
    toJSON (V1 (BackupPhrase wrds)) = toJSON wrds

instance FromJSON (V1 BackupPhrase) where
    parseJSON (Array wrds) = V1 . BackupPhrase . toList <$> traverse parseJSON wrds
    parseJSON x            = typeMismatch "parseJSON failed for BackupPhrase" x

instance ToSchema (V1 BackupPhrase) where
    declareNamedSchema _ = do
        return $ NamedSchema (Just "V1BackupPhrase") $ mempty
            & type_ .~ SwaggerArray
            & items .~ Just (SwaggerItemsObject (toSchemaRef (Proxy @Text)))

mkPassPhrase :: Text -> Either Text Core.PassPhrase
mkPassPhrase text =
    case Base16.decode text of
        Left e -> Left e
        Right bs -> do
            let bl = BS.length bs
            -- Currently passphrase may be either 32-byte long or empty (for
            -- unencrypted keys).
            if bl == 0 || bl == Core.passphraseLength
                then Right $ ByteArray.convert bs
                else Left $ sformat
                     ("Expected spending password to be of either length 0 or "%int%", not "%int)
                     Core.passphraseLength bl

instance ToJSON (V1 Core.PassPhrase) where
    toJSON = String . Base16.encode . ByteArray.convert

instance FromJSON (V1 Core.PassPhrase) where
    parseJSON (String pp) = case mkPassPhrase pp of
        Left e    -> fail (toString e)
        Right pp' -> pure (V1 pp')
    parseJSON x           = typeMismatch "parseJSON failed for PassPhrase" x

instance Arbitrary (V1 Core.PassPhrase) where
    arbitrary = fmap V1 arbitrary

instance ToSchema (V1 Core.PassPhrase) where
    declareNamedSchema _ = do
        pure $ NamedSchema (Just "V1PassPhrase") $ mempty
            & type_ .~ SwaggerString
            & pattern .~ Just "abcd"

instance ToJSON (V1 Core.Coin) where
    toJSON (V1 c) = toJSON . Core.unsafeGetCoin $ c

instance FromJSON (V1 Core.Coin) where
    parseJSON v = V1 . Core.mkCoin <$> parseJSON v

instance Arbitrary (V1 Core.Coin) where
    arbitrary = fmap V1 arbitrary

instance ToSchema (V1 Core.Coin) where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "V1Coin") $ mempty
            & type_ .~ SwaggerNumber
            & maximum_ .~ Just (fromIntegral Core.maxCoinVal)

-- Orphan instance copied from a PR.
--
-- TODO: remove and use upstream when this PR is merged
-- <https://github.com/GetShopTV/swagger2/pull/141>
instance ToSchema a => ToSchema (NonEmpty a) where
     declareNamedSchema _ = do
        listSchema <- declareSchema (Proxy :: Proxy [a])
        pure $ NamedSchema Nothing $ listSchema
            & minItems .~ Just 1

instance ToJSON (V1 Core.Address) where
    toJSON (V1 c) = String $ sformat addressF c

instance FromJSON (V1 Core.Address) where
    parseJSON (String a) = case Core.decodeTextAddress a of
        Left e     -> fail $ "Not a valid Cardano Address: " <> toString e
        Right addr -> pure (V1 addr)
    parseJSON x = typeMismatch "parseJSON failed for Address" x

instance Arbitrary (V1 Core.Address) where
    arbitrary = fmap V1 arbitrary

instance ToSchema (V1 Core.Address) where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "V1Address") $ mempty
            & type_ .~ SwaggerString
            -- TODO: any other constraints we can have here?

instance FromHttpApiData (V1 Core.Address) where
    parseQueryParam = fmap (fmap V1) Core.decodeTextAddress

instance ToHttpApiData (V1 Core.Address) where
    toQueryParam (V1 a) = sformat build a

-- | Represent as number of seconds with precision up to microseconds.
-- Example: @1519923192.346258@.
instance ToJSON (V1 Core.Timestamp) where
    toJSON timestamp = Number $ view (_V1 . Core.timestampSeconds) timestamp

instance FromJSON (V1 Core.Timestamp) where
    parseJSON = withScientific "V1 timestamp" $ \n ->
        pure . V1 $ view (from Core.timestampSeconds) n

instance Arbitrary (V1 Core.Timestamp) where
    arbitrary = fmap V1 arbitrary

instance ToSchema (V1 Core.Timestamp) where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "Timestamp") $ mempty
            & type_ .~ SwaggerString
            & description ?~ "Time in ISO 8601 format"

--
-- Domain-specific types, mostly placeholders.
--

-- | A 'SpendingPassword' represent a secret piece of information which can be
-- optionally supplied by the user to encrypt the private keys. As private keys
-- are needed to spend funds and this password secures spending, here the name
-- 'SpendingPassword'.
-- Practically speaking, it's just a type synonym for a PassPhrase, which is a
-- base16-encoded string.
type SpendingPassword = V1 Core.PassPhrase

type WalletName = Text

data AssuranceLevel =
    NormalAssurance
  | StrictAssurance
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary AssuranceLevel where
    arbitrary = elements [minBound .. maxBound]

deriveJSON Serokell.defaultOptions { constructorTagModifier = toString . toLower . dropEnd 9 . fromString
                                   } ''AssuranceLevel

instance ToSchema AssuranceLevel where
    declareNamedSchema _ = do
        pure $ NamedSchema (Just "AssuranceLevel") $ mempty
            & type_ .~ SwaggerString
            & enum_ .~ Just ["normal", "strict"]

-- | A Wallet ID.
newtype WalletId = WalletId Text deriving (Show, Eq, Ord, Generic)

deriveJSON Serokell.defaultOptions ''WalletId

instance ToSchema WalletId where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance Arbitrary WalletId where
  arbitrary =
      let wid = "J7rQqaLLHBFPrgJXwpktaMB1B1kQBXAyc2uRSfRPzNVGiv6TdxBzkPNBUWysZZZdhFG9gRy3sQFfX5wfpLbi4XTFGFxTg"
          in WalletId <$> elements [wid]

instance FromHttpApiData WalletId where
    parseQueryParam = Right . WalletId

instance ToHttpApiData WalletId where
    toQueryParam (WalletId wid) = wid

data WalletOperation =
    CreateWallet
  | RestoreWallet
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary WalletOperation where
    arbitrary = elements [minBound .. maxBound]

-- Drops the @Wallet@ suffix.
deriveJSON Serokell.defaultOptions  { constructorTagModifier = reverse . drop 6 . reverse . map C.toLower
                                    } ''WalletOperation

instance ToSchema WalletOperation where
    declareNamedSchema _ = do
        pure $ NamedSchema (Just "WalletOperation") $ mempty
            & type_ .~ SwaggerString
            & enum_ .~ Just ["create", "restore"]

-- | A type modelling the request for a new 'Wallet'.
data NewWallet = NewWallet {
      newwalBackupPhrase     :: !(V1 BackupPhrase)
    -- ^ The backup phrase to restore the wallet.
    , newwalSpendingPassword :: !(Maybe SpendingPassword)
    -- ^ The spending password to encrypt the private keys.
    , newwalAssuranceLevel   :: !AssuranceLevel
    , newwalName             :: !WalletName
    , newwalOperation        :: !WalletOperation
    -- ^ Operation to create or to restore a wallet
    } deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions  ''NewWallet

instance Arbitrary NewWallet where
  arbitrary = NewWallet <$> arbitrary
                        <*> pure Nothing
                        <*> arbitrary
                        <*> pure "My Wallet"
                        <*> arbitrary

instance ToSchema NewWallet where
    declareNamedSchema = genericSchemaDroppingPrefix "newwal"

-- | A type modelling the update of an existing wallet.
data WalletUpdate = WalletUpdate {
      uwalAssuranceLevel :: !AssuranceLevel
    , uwalName           :: !Text
    } deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions  ''WalletUpdate

instance ToSchema WalletUpdate where
  declareNamedSchema = genericSchemaDroppingPrefix "uwal"

instance Arbitrary WalletUpdate where
  arbitrary = WalletUpdate <$> arbitrary
                           <*> pure "My Wallet"

-- | A 'Wallet'.
data Wallet = Wallet {
      walId      :: !WalletId
    , walName    :: !WalletName
    , walBalance :: !(V1 Core.Coin)
    } deriving (Eq, Ord, Show, Generic)

deriveJSON Serokell.defaultOptions ''Wallet

instance ToSchema Wallet where
    declareNamedSchema = genericSchemaDroppingPrefix "wal"

instance Arbitrary Wallet where
  arbitrary = Wallet <$> arbitrary
                     <*> pure "My wallet"
                     <*> arbitrary

--------------------------------------------------------------------------------
-- Addresses
--------------------------------------------------------------------------------

-- | Whether an address is valid or not.
newtype AddressValidity = AddressValidity { isValid :: Bool }
  deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions ''AddressValidity

instance ToSchema AddressValidity where
    declareNamedSchema = genericSchemaDroppingPrefix "is"

instance Arbitrary AddressValidity where
  arbitrary = AddressValidity <$> arbitrary

--------------------------------------------------------------------------------
-- Accounts
--------------------------------------------------------------------------------

type AccountIndex = Word32

-- | A wallet 'Account'.
data Account = Account
  { accIndex     :: !AccountIndex
  , accAddresses :: [V1 Core.Address]  -- should be WalletAddress
  , accAmount    :: !(V1 Core.Coin)
  , accName      :: !Text
  -- ^ The Account name.
  , accWalletId  :: WalletId
  -- ^ The 'WalletId' this 'Account' belongs to.
  } deriving (Show, Ord, Eq, Generic)

deriveJSON Serokell.defaultOptions ''Account

instance ToSchema Account where
    declareNamedSchema = genericSchemaDroppingPrefix "acc"

instance Arbitrary Account where
  arbitrary = Account <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> pure "My account"
                      <*> arbitrary

data AccountUpdate = AccountUpdate {
    uaccName      :: !Text
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''AccountUpdate

instance ToSchema AccountUpdate where
    declareNamedSchema = genericSchemaDroppingPrefix "uacc"

instance Arbitrary AccountUpdate where
  arbitrary = AccountUpdate <$> pure "myAccount"

data NewAccount = NewAccount
  { naccSpendingPassword :: !(Maybe SpendingPassword)
  , naccName             :: !Text
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''NewAccount

instance Arbitrary NewAccount where
  arbitrary = NewAccount <$> arbitrary
                         <*> arbitrary

instance ToSchema NewAccount where
    declareNamedSchema = genericSchemaDroppingPrefix "nacc"

-- | Summary about single address.
data WalletAddress = WalletAddress
  { addrId            :: !(V1 Core.Address)
  , addrBalance       :: !(V1 Core.Coin)
  , addrUsed          :: !Bool
  , addrChangeAddress :: !Bool
  } deriving (Show, Generic)

deriveJSON Serokell.defaultOptions ''WalletAddress

instance ToSchema WalletAddress where
    declareNamedSchema =
        genericSchemaDroppingPrefix "addr"

instance Arbitrary WalletAddress where
  arbitrary = WalletAddress <$> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary

data NewAddress = NewAddress
  { newaddrSpendingPassword :: !(Maybe SpendingPassword)
  , newaddrAccountIndex     :: !AccountIndex
  , newaddrWalletId         :: !WalletId
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''NewAddress

instance ToSchema NewAddress where
    declareNamedSchema = genericSchemaDroppingPrefix "newaddr"

instance Arbitrary NewAddress where
  arbitrary = NewAddress <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary

-- | A type incapsulating a password update request.
data PasswordUpdate = PasswordUpdate {
    pwdOld :: !SpendingPassword
    -- ^ The old 'SpendingPassword'.
  , pwdNew :: !SpendingPassword
    -- ^ The new 'SpendingPassword'.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''PasswordUpdate

instance ToSchema PasswordUpdate where
    declareNamedSchema = genericSchemaDroppingPrefix "pwd"

instance Arbitrary PasswordUpdate where
  arbitrary = PasswordUpdate <$> arbitrary
                             <*> arbitrary

-- | 'EstimatedFees' represents the fees which would be generated
-- for a 'Payment' in case the latter would actually be performed.
data EstimatedFees = EstimatedFees {
    feeEstimatedAmount :: !(V1 Core.Coin)
    -- ^ The estimated fees, as coins.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''EstimatedFees

instance ToSchema EstimatedFees where
    declareNamedSchema = genericSchemaDroppingPrefix "fee"

instance Arbitrary EstimatedFees where
  arbitrary = EstimatedFees <$> arbitrary

-- | Maps an 'Address' to some 'Coin's, and it's
-- typically used to specify where to send money during a 'Payment'.
data PaymentDistribution = PaymentDistribution {
      pdAddress :: V1 (Core.Address)
    , pdAmount  :: V1 (Core.Coin)
    } deriving (Show, Ord, Eq, Generic)

deriveJSON Serokell.defaultOptions ''PaymentDistribution

instance ToSchema PaymentDistribution where
    declareNamedSchema = genericSchemaDroppingPrefix "pd"

instance Arbitrary PaymentDistribution where
  arbitrary = PaymentDistribution <$> arbitrary
                                  <*> arbitrary

-- | A 'PaymentSource' encapsulate two essentially piece of data to reach for some funds:
-- a 'WalletId' and an 'AccountIndex' within it.
data PaymentSource = PaymentSource
  { psWalletId     :: !WalletId
  , psAccountIndex :: !AccountIndex
  } deriving (Show, Ord, Eq, Generic)

deriveJSON Serokell.defaultOptions ''PaymentSource

instance ToSchema PaymentSource where
    declareNamedSchema =
        genericSchemaDroppingPrefix "ps"

instance Arbitrary PaymentSource where
  arbitrary = PaymentSource <$> arbitrary
                            <*> arbitrary

-- | A 'Payment' from one source account to one or more 'PaymentDistribution'(s).
data Payment = Payment
  { pmtSource           :: !PaymentSource
    -- ^ The source for the payment.
  , pmtDestinations     :: !(NonEmpty PaymentDistribution)
    -- ^ The destinations for this payment.
  , pmtGroupingPolicy   :: !(Maybe (V1 Core.InputSelectionPolicy))
    -- ^ Which strategy to use for selecting the transaction inputs.
  , pmtSpendingPassword :: !(Maybe SpendingPassword)
    -- ^ spending password to access the funds.
  } deriving (Show, Eq, Generic)

instance ToJSON (V1 Core.InputSelectionPolicy) where
    toJSON (V1 Core.OptimizeForSecurity)       = String "OptimizeForSecurity"
    toJSON (V1 Core.OptimizeForHighThroughput) = String "OptimizeForHighThroughput"

instance FromJSON (V1 Core.InputSelectionPolicy) where
    parseJSON (String "OptimizeForSecurity")       = pure (V1 Core.OptimizeForSecurity)
    parseJSON (String "OptimizeForHighThroughput") = pure (V1 Core.OptimizeForHighThroughput)
    parseJSON x = typeMismatch "Not a valid InputSelectionPolicy" x

instance ToSchema (V1 Core.InputSelectionPolicy) where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "V1InputSelectionPolicy") $ mempty
            & type_ .~ SwaggerString
            & enum_ .~ Just ["OptimizeForSecurity", "OptimizeForHighThroughput"]

instance Arbitrary (V1 Core.InputSelectionPolicy) where
    arbitrary = fmap V1 arbitrary

deriveJSON Serokell.defaultOptions ''Payment

instance Arbitrary Payment where
  arbitrary = Payment <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary

instance ToSchema Payment where
    declareNamedSchema = genericSchemaDroppingPrefix "pmt"

----------------------------------------------------------------------------
-- TxId
----------------------------------------------------------------------------
instance Arbitrary (V1 Core.TxId) where
  arbitrary = V1 <$> arbitrary

instance ToJSON (V1 Core.TxId) where
  toJSON (V1 t) = String (sformat hashHexF t)

instance FromHttpApiData (V1 Core.TxId) where
    parseQueryParam = fmap (fmap V1) decodeHash

instance ToHttpApiData (V1 Core.TxId) where
    toQueryParam (V1 txId) = sformat hashHexF txId

instance ToSchema (V1 Core.TxId) where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)

----------------------------------------------------------------------------
  -- Transaction types
----------------------------------------------------------------------------

-- | The 'Transaction' type.
data TransactionType =
    LocalTransaction
  -- ^ This transaction is local, which means all the inputs
  -- and all the outputs belongs to the wallet from which the
  -- transaction was originated.
  | ForeignTransaction
  -- ^ This transaction is not local to this wallet.
  deriving (Show, Ord, Eq, Enum, Bounded)

instance Arbitrary TransactionType where
  arbitrary = elements [minBound .. maxBound]

-- Drops the @Transaction@ suffix.
deriveJSON defaultOptions { constructorTagModifier = reverse . drop 11 . reverse . map C.toLower
                          } ''TransactionType

instance ToSchema TransactionType where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "TransactionType") $ mempty
            & type_ .~ SwaggerString
            & enum_ .~ Just ["local", "foreign"]

-- | The 'Transaction' @direction@
data TransactionDirection =
    IncomingTransaction
  -- ^ This represents an incoming transactions.
  | OutgoingTransaction
  -- ^ This qualifies external transactions.
  deriving (Show, Ord, Eq, Enum, Bounded)

instance Arbitrary TransactionDirection where
  arbitrary = elements [minBound .. maxBound]

-- Drops the @Transaction@ suffix.
deriveJSON defaultOptions { constructorTagModifier = reverse . drop 11 . reverse . map C.toLower
                          } ''TransactionDirection

instance ToSchema TransactionDirection where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "TransactionDirection") $ mempty
            & type_ .~ SwaggerString
            & enum_ .~ Just ["outgoing", "incoming"]

-- | A 'Wallet''s 'Transaction'.
data Transaction = Transaction
  { txId            :: !(V1 Core.TxId)
    -- ^ The Tx Id.
  , txConfirmations :: !Word
    -- ^ The number of confirmations.
  , txAmount        :: !(V1 Core.Coin)
    -- ^ The 'Coin' moved as part of this transaction.
  , txInputs        :: !(NonEmpty PaymentDistribution)
    -- ^ The input money distribution.
  , txOutputs       :: !(NonEmpty PaymentDistribution)
    -- ^ The output money distribution.
  , txType          :: !TransactionType
    -- ^ The type for this transaction (e.g local, foreign, etc).
  , txDirection     :: !TransactionDirection
    -- ^ The direction for this transaction (e.g incoming, outgoing).
  , txCreationTime  :: !(V1 Core.Timestamp)
    -- ^ The time when transaction was created.
  } deriving (Show, Ord, Eq, Generic)

deriveToJSON Serokell.defaultOptions ''Transaction

instance ToSchema Transaction where
    declareNamedSchema = genericSchemaDroppingPrefix "tx"

instance Arbitrary Transaction where
  arbitrary = Transaction <$> arbitrary
                          <*> arbitrary
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

instance ToSchema WalletSoftwareUpdate where
    declareNamedSchema = genericSchemaDroppingPrefix "upd"

instance Arbitrary WalletSoftwareUpdate where
  arbitrary = WalletSoftwareUpdate <$> arbitrary
                                   <*> arbitrary
                                   <*> fmap getPositive arbitrary

-- | How many milliseconds a slot lasts for.
newtype SlotDuration = SlotDuration (MeasuredIn 'Milliseconds Word)
                     deriving (Show, Eq)

mkSlotDuration :: Word -> SlotDuration
mkSlotDuration = SlotDuration . MeasuredIn

instance Arbitrary SlotDuration where
    arbitrary = mkSlotDuration <$> choose (0, 100)

instance ToJSON SlotDuration where
    toJSON (SlotDuration (MeasuredIn w)) =
            object [ "quantity" .= toJSON w
                   , "unit"     .= String "milliseconds"
                   ]

instance FromJSON SlotDuration where
    parseJSON = withObject "SlotDuration" $ \sl -> mkSlotDuration <$> sl .: "quantity"

instance ToSchema SlotDuration where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "SlotDuration") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["quantity"]
            & properties .~ (mempty
                & at "quantity" ?~ (Inline $ mempty
                    & type_ .~ SwaggerNumber
                    )
                & at "unit" ?~ (Inline $ mempty
                    & type_ .~ SwaggerString
                    & pattern .~ Just "milliseconds"
                    )
                )

-- | The @static@ settings for this wallet node. In particular, we could group
-- here protocol-related settings like the slot duration, the transaction max size,
-- the current software version running on the node, etc.
data NodeSettings = NodeSettings {
     setSlotDuration   :: !SlotDuration
   , setSoftwareInfo   :: !(V1 Core.SoftwareVersion)
   , setProjectVersion :: !Version
   , setGitRevision    :: !Text
   } deriving (Show, Eq, Generic)

-- ORPHAN! TODO: Newtype this?
instance ToSchema Version where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "Version") $ mempty
            & type_ .~ SwaggerString


instance ToJSON (V1 Core.ApplicationName) where
    toJSON (V1 svAppName) = toJSON (Core.getApplicationName svAppName)

instance FromJSON (V1 Core.ApplicationName) where
    parseJSON (String svAppName) = pure (V1 (Core.ApplicationName svAppName))
    parseJSON x                  = typeMismatch "Not a valid ApplicationName" x

instance ToJSON (V1 Core.SoftwareVersion) where
    toJSON (V1 Core.SoftwareVersion{..}) =
        object [ "applicationName" .= toJSON (V1 svAppName)
               -- svNumber is just a type alias to Word32
               -- so that's fine.
               , "version" .=  toJSON svNumber
               ]

instance ToSchema (V1 Core.SoftwareVersion) where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "V1SoftwareVersion") $ mempty
            & type_ .~ SwaggerObject
            & properties .~ (mempty
                & at "applicationName" ?~ Inline (toSchema (Proxy @Text))
                & at "version" ?~ Inline (toSchema (Proxy @Word32))
            )
            & required .~ ["applicationName", "version"]

instance Arbitrary (V1 Core.SoftwareVersion) where
    arbitrary = fmap V1 arbitrary

deriveToJSON Serokell.defaultOptions ''NodeSettings

instance ToSchema NodeSettings where
    declareNamedSchema = genericSchemaDroppingPrefix "set"

instance Arbitrary NodeSettings where
    arbitrary = NodeSettings <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> pure "0e1c9322a"

-- | The different between the local time and the remote NTP server.
newtype LocalTimeDifference = LocalTimeDifference (MeasuredIn 'Microseconds Integer)
                            deriving (Show, Eq)

mkLocalTimeDifference :: Integer -> LocalTimeDifference
mkLocalTimeDifference = LocalTimeDifference . MeasuredIn

instance Arbitrary LocalTimeDifference where
    arbitrary = mkLocalTimeDifference <$> arbitrary

instance ToJSON LocalTimeDifference where
    toJSON (LocalTimeDifference (MeasuredIn w)) =
        object [ "quantity" .= toJSON w
               , "unit"     .= String "microseconds"
               ]

instance FromJSON LocalTimeDifference where
    parseJSON = withObject "LocalTimeDifference" $ \sl -> mkLocalTimeDifference <$> sl .: "quantity"

instance ToSchema LocalTimeDifference where
    declareNamedSchema _ = do
        pure $ NamedSchema (Just "LocalTimeDifference") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["quantity"]
            & properties .~ (mempty
                & at "quantity" ?~ (Inline $ mempty
                    & type_ .~ SwaggerNumber
                    )
                & at "unit" ?~ (Inline $ mempty
                    & type_ .~ SwaggerString
                    & pattern .~ Just "microseconds"
                    )
                )


-- | The sync progress with the blockchain.
newtype SyncProgress = SyncProgress (MeasuredIn 'Percentage100 Word8)
                     deriving (Show, Eq)

mkSyncProgress :: Word8 -> SyncProgress
mkSyncProgress = SyncProgress . MeasuredIn

instance Arbitrary SyncProgress where
    arbitrary = mkSyncProgress <$> choose (0, 100)

instance ToJSON SyncProgress where
    toJSON (SyncProgress (MeasuredIn w)) =
        object [ "quantity" .= toJSON w
               , "unit"     .= String "percent"
               ]

instance FromJSON SyncProgress where
    parseJSON = withObject "SyncProgress" $ \sl -> mkSyncProgress <$> sl .: "quantity"

instance ToSchema SyncProgress where
    declareNamedSchema _ = do
        pure $ NamedSchema (Just "SyncProgress") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["quantity"]
            & properties .~ (mempty
                & at "quantity" ?~ (Inline $ mempty
                    & type_ .~ SwaggerNumber
                    & maximum_ .~ Just 100
                    & minimum_ .~ Just 0
                    )
                & at "unit" ?~ (Inline $ mempty
                    & type_ .~ SwaggerString
                    & pattern .~ Just "percent"
                    )
                )

-- | The absolute or relative height of the blockchain, measured in number
-- of blocks.
newtype BlockchainHeight = BlockchainHeight (MeasuredIn 'Blocks Core.BlockCount)
                         deriving (Show, Eq)

mkBlockchainHeight :: Core.BlockCount -> BlockchainHeight
mkBlockchainHeight = BlockchainHeight . MeasuredIn

instance Arbitrary BlockchainHeight where
    arbitrary = mkBlockchainHeight . Core.BlockCount <$> choose (minBound, maxBound)

instance ToJSON BlockchainHeight where
    toJSON (BlockchainHeight (MeasuredIn w)) = object [ "quantity" .= toJSON (Core.getBlockCount w)
                                                      , "unit"     .= String "blocks"
                                                      ]

instance FromJSON BlockchainHeight where
    parseJSON = withObject "BlockchainHeight" $ \sl ->
        mkBlockchainHeight . Core.BlockCount <$> sl .: "quantity"

instance ToSchema BlockchainHeight where
    declareNamedSchema _ = do
        pure $ NamedSchema (Just "BlockchainHeight") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["quantity"]
            & properties .~ (mempty
                & at "quantity" ?~ (Inline $ mempty
                    & type_ .~ SwaggerNumber
                    & maximum_ .~ Just (fromIntegral (maxBound :: Word64))
                    & minimum_ .~ Just (fromIntegral (minBound :: Word64))
                    )
                & at "unit" ?~ (Inline $ mempty
                    & type_ .~ SwaggerString
                    & pattern .~ Just "blocks"
                    )
                )

-- | The @dynamic@ information for this node.
data NodeInfo = NodeInfo {
     nfoSyncProgress          :: !SyncProgress
   , nfoBlockchainHeight      :: !(Maybe BlockchainHeight)
   -- ^ The current blockchain "height", in blocks, if we know it.
   , nfoLocalBlockchainHeight :: !BlockchainHeight
   -- ^ The local blockchain "height", in blocks.
   , nfoLocalTimeDifference   :: !LocalTimeDifference
   } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''NodeInfo

instance ToSchema NodeInfo where
    declareNamedSchema = genericSchemaDroppingPrefix "nfo"

instance Arbitrary NodeInfo where
    arbitrary = NodeInfo <$> arbitrary
                         -- TODO: The JSON validation stuff is currently
    -- failing because
                         <*> map Just arbitrary
                         <*> arbitrary
                         <*> arbitrary

--
-- POST/PUT requests isomorphisms
--

type family Update (original :: *) :: * where
  Update Wallet        = WalletUpdate
  Update Account       = AccountUpdate
  Update WalletAddress = () -- read-only

type family New (original :: *) :: * where
  New Wallet  = NewWallet
  New Account = NewAccount
  New WalletAddress = NewAddress
