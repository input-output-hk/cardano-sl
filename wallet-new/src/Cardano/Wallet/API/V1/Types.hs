{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}
-- The hlint parser fails on the `pattern` function, so we disable the
-- language extension here.
{-# LANGUAGE NoPatternSynonyms          #-}

-- Needed for the `Buildable`, `SubscriptionStatus` and `NodeId` orphans.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.API.V1.Types (
    V1 (..)
  , unV1
  -- * Swagger & REST-related types
  , PasswordUpdate (..)
  , AccountUpdate (..)
  , NewAccount (..)
  , Update
  , New
  , ForceNtpCheck (..)
  -- * Domain-specific types
  -- * Wallets
  , Wallet (..)
  , AssuranceLevel (..)
  , NewWallet (..)
  , WalletUpdate (..)
  , WalletId (..)
  , exampleWalletId
  , WalletType (..)
  , WalletOperation (..)
  , SpendingPassword
  , ExternalWallet (..)
  , PublicKeyAsBase58
  , mkPublicKeyAsBase58
  , mkPublicKeyFromBase58
  , NewExternalWallet (..)
  , WalletAndTxHistory (..)
  -- * Addresses
  , AddressOwnership (..)
  , AddressIndex
  , AddressValidity (..)
  , AddressAsBase58
  , mkAddressAsBase58
  , mkAddressFromBase58
  -- * Accounts
  , Account (..)
  , accountsHaveSameId
  , AccountIndex
  , AccountAddresses (..)
  , AccountBalance (..)
  , getAccIndex
  , mkAccountIndex
  , mkAccountIndexM
  , unsafeMkAccountIndex
  , AccountIndexError(..)
  -- * Addresses
  , WalletAddress (..)
  , NewAddress (..)
  , AddressPath
  , AddressLevel
  , addressLevelToWord32
  , word32ToAddressLevel
  , IsChangeAddress (..)
  , mkAddressPathBIP44
  -- * Payments
  , Payment (..)
  , PaymentSource (..)
  , PaymentDistribution (..)
  , Transaction (..)
  , TransactionType (..)
  , TransactionDirection (..)
  , TransactionStatus(..)
  , TransactionAsBase16
  , mkTransactionAsBase16
  , rawTransactionAsBase16
  , TransactionSignatureAsBase16
  , mkTransactionSignatureAsBase16
  , rawTransactionSignatureAsBase16
  , EstimatedFees (..)
  , AddressAndPath (..)
  , UnsignedTransaction (..)
  , AddressWithProof (..)
  , SignedTransaction (..)
  -- * Updates
  , WalletSoftwareUpdate (..)
  -- * Importing a wallet from a backup
  , WalletImport (..)
  -- * Settings
  , NodeSettings (..)
  , SlotDuration
  , mkSlotDuration
  , BlockchainHeight
  , mkBlockchainHeight
  , LocalTimeDifference
  , mkLocalTimeDifference
  , EstimatedCompletionTime
  , mkEstimatedCompletionTime
  , SyncThroughput
  , mkSyncThroughput
  , SyncState (..)
  , SyncProgress (..)
  , SyncPercentage
  , mkSyncPercentage
  , NodeInfo (..)
  , TimeInfo(..)
  , SubscriptionStatus(..)
  , Redemption(..)
  , RedemptionMnemonic(..)
  , BackupPhrase(..)
  , ShieldedRedemptionCode(..)
  , WAddressMeta (..)
  -- * Some types for the API
  , CaptureWalletId
  , CaptureAccountId
  -- * Core re-exports
  , Core.Address
  -- * Wallet Errors
  , WalletError(..)
  , ErrNotEnoughMoney(..)
  , toServantError
  , toHttpErrorStatus

  , module Cardano.Wallet.Types.UtxoStatistics
  ) where

import qualified Prelude
import           Universum

import qualified Cardano.Crypto.Wallet as CC
-- import           Cardano.Wallet.API.V1.Swagger.Example (Example, example)
import           Control.Lens (At, Index, IxValue, at, ix, makePrisms, to, (?~))
import           Data.Aeson
import qualified Data.Aeson.Options as Serokell
import           Data.Aeson.TH as A
import           Data.Aeson.Types (Parser, Value (..), toJSONKeyText,
                     typeMismatch)
import           Data.Bifunctor (first)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import           Data.ByteString.Base58 (bitcoinAlphabet, decodeBase58,
                     encodeBase58)
import qualified Data.Char as C
import           Data.Default (Default (def))
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Semigroup)
import           Data.Swagger hiding (Example, example)
import qualified Data.Swagger as S
import           Data.Swagger.Declare (Declare, look)
import           Data.Swagger.Internal.Schema (GToSchema)
import           Data.Swagger.Internal.TypeShape (GenericHasSimpleShape,
                     GenericShape)
import           Data.Text (Text, dropEnd, toLower)
import qualified Data.Text as T
import           Data.Version (Version (..), parseVersion, showVersion)
import           Formatting (bprint, build, fconst, int, sformat, shown, stext,
                     (%))
import qualified Formatting.Buildable
import           Generics.SOP.TH (deriveGeneric)
import           GHC.Generics (Generic, Rep)
import           Network.Transport (EndPointAddress (..))
import           Node (NodeId (..))
import           Serokell.Util (listJson)
import qualified Serokell.Util.Base16 as Base16
import           Servant
import           Test.QuickCheck
import           Test.QuickCheck.Gen (Gen (..))
import qualified Test.QuickCheck.Gen as Gen
import qualified Test.QuickCheck.Modifiers as Gen

import           Cardano.Wallet.API.Response.JSend (HasDiagnostic (..),
                     noDiagnosticKey)
import           Cardano.Wallet.API.Types.UnitOfMeasure (MeasuredIn (..),
                     UnitOfMeasure (..))
import           Cardano.Wallet.API.V1.Errors (ToHttpErrorStatus (..),
                     ToServantError (..))
import           Cardano.Wallet.API.V1.Generic (jsendErrorGenericParseJSON,
                     jsendErrorGenericToJSON)
import           Cardano.Wallet.API.V1.Swagger.Example (Example, example,
                     genExample)
import           Cardano.Wallet.Types.UtxoStatistics
import           Cardano.Wallet.Util (mkJsonKey, showApiUtcTime)

import           Cardano.Wallet.Kernel.BIP39 (Mnemonic)
import qualified Pos.Binary.Class as Bi
import qualified Pos.Chain.Txp as Txp
import qualified Pos.Chain.Update as Core
import qualified Pos.Client.Txp.Util as Core
import qualified Pos.Core as Core
import           Pos.Crypto (PublicKey (..), decodeHash, hashHexF)
import qualified Pos.Crypto.Signing as Core
import           Pos.Infra.Communication.Types.Protocol ()
import           Pos.Infra.Diffusion.Subscription.Status
                     (SubscriptionStatus (..))
import           Pos.Infra.Util.LogSafe (BuildableSafeGen (..), SecureLog (..),
                     buildSafe, buildSafeList, buildSafeMaybe,
                     deriveSafeBuildable, plainOrSecureF)
import           Pos.Util.Servant (Flaggable (..))
import           Test.Pos.Chain.Update.Arbitrary ()
import           Test.Pos.Core.Arbitrary ()
import           Text.ParserCombinators.ReadP (readP_to_S)

-- | Declare generic schema, while documenting properties
--   For instance:
--
--    data MyData = MyData
--      { myDataField1 :: String
--      , myDataField2 :: String
--      } deriving (Generic)
--
--   instance ToSchema MyData where
--     declareNamedSchema =
--       genericSchemaDroppingPrefix "myData" (\(--^) props -> props
--         & ("field1" --^ "Description 1")
--         & ("field2" --^ "Description 2")
--       )
--
--   -- or, if no descriptions are added to the underlying properties
--
--   instance ToSchema MyData where
--     declareNamedSchema =
--       genericSchemaDroppingPrefix "myData" (\_ -> id)
--
type IsPropertiesMap m =
  (IxValue m ~ Referenced Schema, Index m ~ Text, At m, HasProperties Schema m)

genericSchemaDroppingPrefix
    :: forall a m proxy.
    ( Generic a, ToJSON a, Example a, GToSchema (Rep a), IsPropertiesMap m
    , GenericHasSimpleShape
        a
        "genericDeclareNamedSchemaUnrestricted"
        (GenericShape (Rep a))
    )
    => String -- ^ Prefix to drop on each constructor tag
    -> ((Index m -> Text -> m -> m) -> m -> m) -- ^ Callback update to attach descriptions to underlying properties
    -> proxy a -- ^ Underlying data-type proxy
    -> Declare (Definitions Schema) NamedSchema
genericSchemaDroppingPrefix prfx extraDoc proxy = do
    let opts = defaultSchemaOptions
          { S.fieldLabelModifier = over (ix 0) C.toLower . drop (length prfx) }
    s <- genericDeclareNamedSchema opts proxy
    defs <- look
    pure $ s
      & over schema (over properties (extraDoc (addFieldDescription defs)))
      & schema . S.example ?~ toJSON (genExample :: a)
  where
    addFieldDescription defs field desc =
      over (at field) (addDescription defs field desc)

    addDescription defs field desc ms =
      let
        rewrap s = Just (Inline (s & description ?~ desc))
        err = error ("Unknown field in schema: " <> field <> " " <> desc)
      in
        case ms of
          Just (Inline s) -> rewrap s
          Just (Ref ref)  -> maybe err rewrap (defs ^. at (getReference ref))
          _               -> err


optsADTCamelCase :: A.Options
optsADTCamelCase = defaultOptions
    { A.constructorTagModifier = mkJsonKey
    , A.sumEncoding            = A.ObjectWithSingleField
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

-- | Unwrap the 'V1' newtype to give the underlying type.
unV1 :: V1 a -> a
unV1 (V1 a) = a

makePrisms ''V1

instance Show a => Show (V1 a) where
    show (V1 a) = Prelude.show a

instance Enum a => Enum (V1 a) where
    toEnum x = V1 (toEnum x)
    fromEnum (V1 a) = fromEnum a

instance Bounded a => Bounded (V1 a) where
    minBound = V1 $ minBound @a
    maxBound = V1 $ maxBound @a

instance {-# OVERLAPPABLE #-} Buildable a => Buildable (V1 a) where
    build (V1 x) = bprint build x

instance Buildable (SecureLog a) => Buildable (SecureLog (V1 a)) where
    build (SecureLog (V1 x)) = bprint build (SecureLog x)

instance (Buildable a, Buildable b) => Buildable (a, b) where
    build (a, b) = bprint ("("%build%", "%build%")") a b

--
-- Benign instances
--

instance ByteArray.ByteArrayAccess a => ByteArray.ByteArrayAccess (V1 a) where
   length (V1 a) = ByteArray.length a
   withByteArray (V1 a) = ByteArray.withByteArray a

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
    declareNamedSchema _ =
        pure $ NamedSchema (Just "V1PassPhrase") $ mempty
            & type_ .~ SwaggerString
            & format ?~ "hex|base16"

instance ToJSON (V1 Core.Coin) where
    toJSON (V1 c) = toJSON . Core.unsafeGetCoin $ c

instance FromJSON (V1 Core.Coin) where
    parseJSON v = do
        i <- Core.Coin <$> parseJSON v
        either (fail . toString) (const (pure (V1 i)))
            $ Core.checkCoin i

instance Arbitrary (V1 Core.Coin) where
    arbitrary = fmap V1 arbitrary

instance ToSchema (V1 Core.Coin) where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "V1Coin") $ mempty
            & type_ .~ SwaggerNumber
            & maximum_ .~ Just (fromIntegral Core.maxCoinVal)

instance ToHttpApiData Core.Coin where
    toQueryParam = pretty . Core.coinToInteger

instance FromHttpApiData Core.Coin where
    parseUrlPiece p = do
        c <- Core.Coin <$> parseQueryParam p
        Core.checkCoin c
        pure c

instance ToJSON (V1 Core.Address) where
    toJSON (V1 c) = String $ sformat Core.addressF c

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

deriving instance Hashable (V1 Core.Address)
deriving instance NFData (V1 Core.Address)

-- | Represents according to 'apiTimeFormat' format.
instance ToJSON (V1 Core.Timestamp) where
    toJSON timestamp =
        let utcTime = timestamp ^. _V1 . Core.timestampToUTCTimeL
        in  String $ showApiUtcTime utcTime

instance ToHttpApiData (V1 Core.Timestamp) where
    toQueryParam = view (_V1 . Core.timestampToUTCTimeL . to showApiUtcTime)

instance FromHttpApiData (V1 Core.Timestamp) where
    parseQueryParam t =
        maybe
            (Left ("Couldn't parse timestamp or datetime out of: " <> t))
            (Right . V1)
            (Core.parseTimestamp t)

-- | Parses from both UTC time in 'apiTimeFormat' format and a fractional
-- timestamp format.
instance FromJSON (V1 Core.Timestamp) where
    parseJSON = withText "Timestamp" $ \t ->
        maybe
            (fail ("Couldn't parse timestamp or datetime out of: " <> toString t))
            (pure . V1)
            (Core.parseTimestamp t)

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

instance Semigroup (V1 Core.PassPhrase) where
    V1 a <> V1 b = V1 (a <> b)

instance Monoid (V1 Core.PassPhrase) where
    mempty = V1 mempty
    mappend = (<>)

instance BuildableSafeGen SpendingPassword where
    buildSafeGen sl pwd =
        bprint (plainOrSecureF sl build (fconst "<spending password>")) pwd

type WalletName = Text

-- | Wallet's Assurance Level
data AssuranceLevel =
    NormalAssurance
  | StrictAssurance
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Arbitrary AssuranceLevel where
    arbitrary = elements [minBound .. maxBound]

deriveJSON Serokell.defaultOptions { A.constructorTagModifier = toString . toLower . dropEnd 9 . fromString
                                   } ''AssuranceLevel

instance ToSchema AssuranceLevel where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "AssuranceLevel") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["normal", "strict"]

deriveSafeBuildable ''AssuranceLevel
instance BuildableSafeGen AssuranceLevel where
    buildSafeGen _ NormalAssurance = "normal"
    buildSafeGen _ StrictAssurance = "strict"

-- | A Wallet ID.
newtype WalletId = WalletId Text deriving (Show, Eq, Ord, Generic)

exampleWalletId :: WalletId
exampleWalletId = WalletId "J7rQqaLLHBFPrgJXwpktaMB1B1kQBXAyc2uRSfRPzNVGiv6TdxBzkPNBUWysZZZdhFG9gRy3sQFfX5wfpLbi4XTFGFxTg"

deriveJSON Serokell.defaultOptions ''WalletId

instance ToSchema WalletId where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToJSONKey WalletId

instance Arbitrary WalletId where
    arbitrary = elements [exampleWalletId]

deriveSafeBuildable ''WalletId
instance BuildableSafeGen WalletId where
    buildSafeGen sl (WalletId wid) =
        bprint (plainOrSecureF sl build (fconst "<wallet id>")) wid

instance FromHttpApiData WalletId where
    parseQueryParam = Right . WalletId

instance ToHttpApiData WalletId where
    toQueryParam (WalletId wid) = wid

instance Hashable WalletId
instance NFData WalletId

-- | A Wallet Operation
data WalletOperation =
    CreateWallet
  | RestoreWallet
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary WalletOperation where
    arbitrary = elements [minBound .. maxBound]

-- Drops the @Wallet@ suffix.
deriveJSON Serokell.defaultOptions  { A.constructorTagModifier = reverse . drop 6 . reverse . map C.toLower
                                    } ''WalletOperation

instance ToSchema WalletOperation where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "WalletOperation") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["create", "restore"]

deriveSafeBuildable ''WalletOperation
instance BuildableSafeGen WalletOperation where
    buildSafeGen _ CreateWallet  = "create"
    buildSafeGen _ RestoreWallet = "restore"


newtype BackupPhrase = BackupPhrase
    { unBackupPhrase :: Mnemonic 12
    }
    deriving stock (Eq, Show)
    deriving newtype (ToJSON, FromJSON, Arbitrary)

deriveSafeBuildable ''BackupPhrase
instance BuildableSafeGen BackupPhrase where
    buildSafeGen _ _  = "<backup phrase>"

instance ToSchema BackupPhrase where
    declareNamedSchema _ =
        pure
            . NamedSchema (Just "V1BackupPhrase")
            $ toSchema (Proxy @(Mnemonic 12))

-- | A type modelling the request for a new 'Wallet'.
data NewWallet = NewWallet {
      newwalBackupPhrase     :: !BackupPhrase
    , newwalSpendingPassword :: !(Maybe SpendingPassword)
    , newwalAssuranceLevel   :: !AssuranceLevel
    , newwalName             :: !WalletName
    , newwalOperation        :: !WalletOperation
    } deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions  ''NewWallet

instance Arbitrary NewWallet where
  arbitrary = NewWallet <$> arbitrary
                        <*> pure Nothing
                        <*> arbitrary
                        <*> pure "My Wallet"
                        <*> arbitrary

instance ToSchema NewWallet where
  declareNamedSchema =
    genericSchemaDroppingPrefix "newwal" (\(--^) props -> props
      & ("backupPhrase"     --^ "Backup phrase to restore the wallet.")
      & ("spendingPassword" --^ "Optional (but recommended) password to protect the wallet on sensitive operations.")
      & ("assuranceLevel"   --^ "Desired assurance level based on the number of confirmations counter of each transaction.")
      & ("name"             --^ "Wallet's name.")
      & ("operation"        --^ "Create a new wallet or Restore an existing one.")
    )


deriveSafeBuildable ''NewWallet
instance BuildableSafeGen NewWallet where
    buildSafeGen sl NewWallet{..} = bprint ("{"
        %" backupPhrase="%buildSafe sl
        %" spendingPassword="%(buildSafeMaybe mempty sl)
        %" assuranceLevel="%buildSafe sl
        %" name="%buildSafe sl
        %" operation"%buildSafe sl
        %" }")
        newwalBackupPhrase
        newwalSpendingPassword
        newwalAssuranceLevel
        newwalName
        newwalOperation

-- | Type for representation of extended public key in Base58-format.
-- We use it for external wallets:
-- 1. As a root PK to identify external wallet.
-- 2. As a derived PK, please see 'AddressWithProof' type.
newtype PublicKeyAsBase58 = PublicKeyAsBase58Unsafe
    { ewalPublicKeyAsBase58 :: Text
    } deriving (Eq, Generic, Ord, Show)

deriveJSON Serokell.defaultOptions ''PublicKeyAsBase58
instance Arbitrary PublicKeyAsBase58 where
    arbitrary = PublicKeyAsBase58Unsafe <$> pure
        "bNfKyG8UxD5aoHyn9snKKhfyVcEnMGSFYtuiapmfD23BQMD9op65gbMoy1EXwxzzkuVqPLkD1s12MXFdfLF8ZCfnPh"

instance ToSchema PublicKeyAsBase58 where
    declareNamedSchema =
        genericSchemaDroppingPrefix "ewal" (\(--^) props -> props
            & ("publicKeyAsBase58" --^ "Extended public key in Base58-format.")
        )

deriveSafeBuildable ''PublicKeyAsBase58
instance BuildableSafeGen PublicKeyAsBase58 where
    buildSafeGen sl PublicKeyAsBase58Unsafe{..} = bprint ("{"
        %" publicKeyAsBase58="%buildSafe sl
        %" }")
        ewalPublicKeyAsBase58

instance FromHttpApiData PublicKeyAsBase58 where
    parseQueryParam someText =
        case decodeBase58 bitcoinAlphabet (encodeUtf8 someText) of
            Nothing -> Left "Must be extended public key encoded in Base58-format."
            Just _  -> Right $ PublicKeyAsBase58Unsafe someText

instance ToHttpApiData PublicKeyAsBase58 where
    toQueryParam (PublicKeyAsBase58Unsafe pk) = pk

-- | Smart constructor for 'PublicKeyAsBase58'.
mkPublicKeyAsBase58 :: PublicKey -> PublicKeyAsBase58
mkPublicKeyAsBase58 (PublicKey xPub) = PublicKeyAsBase58Unsafe encodedXPub
  where
    encodedXPub = decodeUtf8 $ encodeBase58 bitcoinAlphabet (CC.unXPub xPub)

-- | Possible problems with Base58-encoded extended public key.
data Base58PublicKeyError
    = PublicKeyNotInBase58Form
    | NotAPublicKey !Text
    deriving Show

instance Buildable Base58PublicKeyError where
    build PublicKeyNotInBase58Form =
        "Extended public key is not in Base58-format."
    build (NotAPublicKey msg) =
        bprint ("It is not an extended public key: "%stext) msg

-- | Decoder for 'PublicKey' in Base58-format.
mkPublicKeyFromBase58 :: PublicKeyAsBase58 -> Either Base58PublicKeyError PublicKey
mkPublicKeyFromBase58 (PublicKeyAsBase58Unsafe encodedXPub) = do
    case (decodeBase58 bitcoinAlphabet . encodeUtf8 $ encodedXPub) of
        Nothing -> Left PublicKeyNotInBase58Form
        Just rawKey -> case CC.xpub rawKey of
            Left problem -> Left $ NotAPublicKey (toText problem)
            Right xPub   -> Right $ PublicKey xPub

-- | Type for representation address in Base58-format.
-- We use it for external wallets.
newtype AddressAsBase58 = AddressAsBase58Unsafe
    { ewalAddressAsBase58 :: Text
    } deriving (Eq, Generic, Ord, Show)

deriveJSON Serokell.defaultOptions ''AddressAsBase58
instance Arbitrary AddressAsBase58 where
    arbitrary = AddressAsBase58Unsafe <$> pure
        "DdzFFzCqrhsxqFTw2ENzvwisYmS2DcUTujXDoMXtMrCvMFAa3DikLj8YYTWNZaEjthKZpMNWKo9RUoq3gP797yP8MP4g9qiEegvGEY9w"

instance ToSchema AddressAsBase58 where
    declareNamedSchema =
        genericSchemaDroppingPrefix "ewal" (\(--^) props -> props
            & ("addressAsBase58" --^ "Address in Base58-format.")
        )

deriveSafeBuildable ''AddressAsBase58
instance BuildableSafeGen AddressAsBase58 where
    buildSafeGen sl AddressAsBase58Unsafe{..} = bprint ("{"
        %" addressAsBase58="%buildSafe sl
        %" }")
        ewalAddressAsBase58

-- | Smart constructor for 'AddressAsBase58'.
mkAddressAsBase58 :: Core.Address -> AddressAsBase58
mkAddressAsBase58 = AddressAsBase58Unsafe . decodeUtf8 . Core.addrToBase58

-- | Decode Base58-address to real Address.
mkAddressFromBase58 :: AddressAsBase58 -> Either Text Core.Address
mkAddressFromBase58 (AddressAsBase58Unsafe txtAddr) = Core.decodeTextAddress txtAddr

-- | Type for representation of serialized transaction in Base16-format.
-- We use it for external wallets (to send/receive raw transaction during
-- external signing).
newtype TransactionAsBase16 = TransactionAsBase16Unsafe
    { ewalTransactionAsBase16 :: Text
    } deriving (Eq, Generic, Ord, Show)

deriveJSON Serokell.defaultOptions ''TransactionAsBase16
instance Arbitrary TransactionAsBase16 where
    arbitrary = TransactionAsBase16Unsafe <$> pure
        "839f8200d8185826825820e981442c2be40475bb42193ca35907861d90715854de6fcba767b98f1789b51219439aff9f8282d818584a83581ce7fe8e468d2249f18cd7bf9aec0d4374b7d3e18609ede8589f82f7f0a20058208200581c240596b9b63fc010c06fbe92cf6f820587406534795958c411e662dc014443c0688e001a6768cc861b0037699e3ea6d064ffa0"

instance ToSchema TransactionAsBase16 where
    declareNamedSchema =
        genericSchemaDroppingPrefix "ewal" (\(--^) props -> props
            & ("transactionAsBase16" --^ "Serialized transaction in Base16-format.")
        )

deriveSafeBuildable ''TransactionAsBase16
instance BuildableSafeGen TransactionAsBase16 where
    buildSafeGen sl TransactionAsBase16Unsafe{..} = bprint ("{"
        %" transactionAsBase16="%buildSafe sl
        %" }")
        ewalTransactionAsBase16

-- | Type for representation of transaction signature in Base16-format.
-- We use it for external wallet. Please note that technically there's no
-- signature of transaction, but signature of the hash of transaction.
newtype TransactionSignatureAsBase16 = TransactionSignatureAsBase16Unsafe
    { rawTransactionSignatureAsBase16 :: Text
    } deriving (Eq, Generic, Ord, Show)

deriveJSON Serokell.defaultOptions ''TransactionSignatureAsBase16
instance Arbitrary TransactionSignatureAsBase16 where
    arbitrary = TransactionSignatureAsBase16Unsafe <$> pure
        "5840709cc240ac9ad78cbf47c3eec76df917423943e34339277593e8e2b8c9f9f2e59583023bfbd8e26c40dff6a7fa424600f9b942819533d8afee37a5ac6d813207"

instance ToSchema TransactionSignatureAsBase16 where
    declareNamedSchema =
        genericSchemaDroppingPrefix "raw" (\(--^) props -> props
            & ("transactionSignatureAsBase16" --^ "Signature of the hash of transaction in Base16-format.")
        )

deriveSafeBuildable ''TransactionSignatureAsBase16
instance BuildableSafeGen TransactionSignatureAsBase16 where
    buildSafeGen sl TransactionSignatureAsBase16Unsafe{..} = bprint ("{"
        %" transactionSignatureAsBase16="%buildSafe sl
        %" }")
        rawTransactionSignatureAsBase16

-- | Makes tx signature as Base16-text.
mkTransactionSignatureAsBase16 :: Core.Signature Txp.TxSigData -> TransactionSignatureAsBase16
mkTransactionSignatureAsBase16 (Core.Signature txSig) =
    TransactionSignatureAsBase16Unsafe . Base16.encode . CC.unXSignature $ txSig

-- | A type modelling the request for a new 'ExternalWallet',
-- on the mobile client or hardware wallet.
data NewExternalWallet = NewExternalWallet
    { newewalRootPK         :: !PublicKeyAsBase58
    , newewalAssuranceLevel :: !AssuranceLevel
    , newewalName           :: !WalletName
    , newewalOperation      :: !WalletOperation
    } deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions ''NewExternalWallet
instance Arbitrary NewExternalWallet where
    arbitrary = NewExternalWallet <$> arbitrary
                                  <*> arbitrary
                                  <*> pure "My external Wallet"
                                  <*> arbitrary

instance ToSchema NewExternalWallet where
    declareNamedSchema =
        genericSchemaDroppingPrefix "newewal" (\(--^) props -> props
            & ("rootPK"         --^ "Root public key to identify external wallet.")
            & ("assuranceLevel" --^ "Desired assurance level based on the number of confirmations counter of each transaction.")
            & ("name"           --^ "External wallet's name.")
            & ("operation"      --^ "Create a new external wallet or Restore an existing one.")
        )

deriveSafeBuildable ''NewExternalWallet
instance BuildableSafeGen NewExternalWallet where
    buildSafeGen sl NewExternalWallet{..} = bprint ("{"
        %" rootPK="%buildSafe sl
        %" assuranceLevel="%buildSafe sl
        %" name="%buildSafe sl
        %" operation"%buildSafe sl
        %" }")
        newewalRootPK
        newewalAssuranceLevel
        newewalName
        newewalOperation

-- | A type modelling the update of an existing wallet.
data WalletUpdate = WalletUpdate {
      uwalAssuranceLevel :: !AssuranceLevel
    , uwalName           :: !Text
    } deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions  ''WalletUpdate

instance ToSchema WalletUpdate where
  declareNamedSchema =
    genericSchemaDroppingPrefix "uwal" (\(--^) props -> props
      & ("assuranceLevel" --^ "New assurance level.")
      & ("name"           --^ "New wallet's name.")
    )

instance Arbitrary WalletUpdate where
  arbitrary = WalletUpdate <$> arbitrary
                           <*> pure "My Wallet"

deriveSafeBuildable ''WalletUpdate
instance BuildableSafeGen WalletUpdate where
    buildSafeGen sl WalletUpdate{..} = bprint ("{"
        %" assuranceLevel="%buildSafe sl
        %" name="%buildSafe sl
        %" }")
        uwalAssuranceLevel
        uwalName

-- | The sync progress with the blockchain.
newtype SyncPercentage = SyncPercentage (MeasuredIn 'Percentage100 Word8)
                     deriving (Show, Eq)

mkSyncPercentage :: Word8 -> SyncPercentage
mkSyncPercentage = SyncPercentage . MeasuredIn

instance Ord SyncPercentage where
    compare (SyncPercentage (MeasuredIn p1))
            (SyncPercentage (MeasuredIn p2)) = compare p1 p2

instance Arbitrary SyncPercentage where
    arbitrary = mkSyncPercentage <$> choose (0, 100)

instance Example SyncPercentage where
    example =
        pure (mkSyncPercentage 14)

instance ToJSON SyncPercentage where
    toJSON (SyncPercentage (MeasuredIn w)) =
        object [ "quantity" .= toJSON w
               , "unit"     .= String "percent"
               ]

instance FromJSON SyncPercentage where
    parseJSON = withObject "SyncPercentage" $ \sl -> mkSyncPercentage <$> sl .: "quantity"

instance ToSchema SyncPercentage where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "SyncPercentage") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["quantity", "unit"]
            & properties .~ (mempty
                & at "quantity" ?~ (Inline $ mempty
                    & type_ .~ SwaggerNumber
                    & maximum_ .~ Just 100
                    & minimum_ .~ Just 0
                    )
                & at "unit" ?~ (Inline $ mempty
                    & type_ .~ SwaggerString
                    & enum_ ?~ ["percent"]
                    )
                )

deriveSafeBuildable ''SyncPercentage
instance BuildableSafeGen SyncPercentage where
    buildSafeGen _ (SyncPercentage (MeasuredIn w)) =
        bprint (build%"%") w


newtype EstimatedCompletionTime = EstimatedCompletionTime (MeasuredIn 'Milliseconds Word)
  deriving (Show, Eq)

mkEstimatedCompletionTime :: Word -> EstimatedCompletionTime
mkEstimatedCompletionTime = EstimatedCompletionTime . MeasuredIn

instance Ord EstimatedCompletionTime where
    compare (EstimatedCompletionTime (MeasuredIn w1))
            (EstimatedCompletionTime (MeasuredIn w2)) = compare w1 w2

instance Arbitrary EstimatedCompletionTime where
    arbitrary = EstimatedCompletionTime . MeasuredIn <$> arbitrary

deriveSafeBuildable ''EstimatedCompletionTime
instance BuildableSafeGen EstimatedCompletionTime where
    buildSafeGen _ (EstimatedCompletionTime (MeasuredIn w)) = bprint ("{"
        %" quantity="%build
        %" unit=milliseconds"
        %" }")
        w

instance ToJSON EstimatedCompletionTime where
    toJSON (EstimatedCompletionTime (MeasuredIn w)) =
        object [ "quantity" .= toJSON w
               , "unit"     .= String "milliseconds"
               ]

instance FromJSON EstimatedCompletionTime where
    parseJSON = withObject "EstimatedCompletionTime" $ \sl -> mkEstimatedCompletionTime <$> sl .: "quantity"

instance ToSchema EstimatedCompletionTime where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "EstimatedCompletionTime") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["quantity", "unit"]
            & properties .~ (mempty
                & at "quantity" ?~ (Inline $ mempty
                    & type_ .~ SwaggerNumber
                    & minimum_ .~ Just 0
                    )
                & at "unit" ?~ (Inline $ mempty
                    & type_ .~ SwaggerString
                    & enum_ ?~ ["milliseconds"]
                    )
                )

newtype SyncThroughput
    = SyncThroughput (MeasuredIn 'BlocksPerSecond Core.BlockCount)
  deriving (Show, Eq)

mkSyncThroughput :: Core.BlockCount -> SyncThroughput
mkSyncThroughput = SyncThroughput . MeasuredIn

instance Ord SyncThroughput where
    compare (SyncThroughput (MeasuredIn (Core.BlockCount b1)))
            (SyncThroughput (MeasuredIn (Core.BlockCount b2))) =
        compare b1 b2

instance Arbitrary SyncThroughput where
    arbitrary = SyncThroughput . MeasuredIn <$> arbitrary

deriveSafeBuildable ''SyncThroughput
instance BuildableSafeGen SyncThroughput where
    buildSafeGen _ (SyncThroughput (MeasuredIn (Core.BlockCount blocks))) = bprint ("{"
        %" quantity="%build
        %" unit=blocksPerSecond"
        %" }")
        blocks

instance ToJSON SyncThroughput where
    toJSON (SyncThroughput (MeasuredIn (Core.BlockCount blocks))) =
      object [ "quantity" .= toJSON blocks
             , "unit"     .= String "blocksPerSecond"
             ]

instance FromJSON SyncThroughput where
    parseJSON = withObject "SyncThroughput" $ \sl -> mkSyncThroughput . Core.BlockCount <$> sl .: "quantity"

instance ToSchema SyncThroughput where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "SyncThroughput") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["quantity", "unit"]
            & properties .~ (mempty
                & at "quantity" ?~ (Inline $ mempty
                    & type_ .~ SwaggerNumber
                    )
                & at "unit" ?~ (Inline $ mempty
                    & type_ .~ SwaggerString
                    & enum_ ?~ ["blocksPerSecond"]
                    )
                )

data SyncProgress = SyncProgress {
    spEstimatedCompletionTime :: !EstimatedCompletionTime
  , spThroughput              :: !SyncThroughput
  , spPercentage              :: !SyncPercentage
  } deriving (Show, Eq, Ord, Generic)

deriveJSON Serokell.defaultOptions ''SyncProgress

instance ToSchema SyncProgress where
    declareNamedSchema =
        genericSchemaDroppingPrefix "sp" (\(--^) props -> props
            & "estimatedCompletionTime"
            --^ "The estimated time the wallet is expected to be fully sync, based on the information available."
            & "throughput"
            --^ "The sync throughput, measured in blocks/s."
            & "percentage"
            --^ "The sync percentage, from 0% to 100%."
        )

deriveSafeBuildable ''SyncProgress
-- Nothing secret to redact for a SyncProgress.
instance BuildableSafeGen SyncProgress where
    buildSafeGen sl SyncProgress {..} = bprint ("{"
        %" estimatedCompletionTime="%buildSafe sl
        %" throughput="%buildSafe sl
        %" percentage="%buildSafe sl
        %" }")
        spEstimatedCompletionTime
        spThroughput
        spPercentage

instance Example SyncProgress where
    example = do
        exPercentage <- example
        pure $ SyncProgress
            { spEstimatedCompletionTime = mkEstimatedCompletionTime 3000
            , spThroughput              = mkSyncThroughput (Core.BlockCount 400)
            , spPercentage              = exPercentage
            }

instance Arbitrary SyncProgress where
  arbitrary = SyncProgress <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary

data SyncState =
      Restoring SyncProgress
    -- ^ Restoring from seed or from backup.
    | Synced
    -- ^ Following the blockchain.
    deriving (Eq, Show, Ord)

instance ToJSON SyncState where
    toJSON ss = object [ "tag"  .= toJSON (renderAsTag ss)
                       , "data" .= renderAsData ss
                       ]
      where
        renderAsTag :: SyncState -> Text
        renderAsTag (Restoring _) = "restoring"
        renderAsTag Synced        = "synced"

        renderAsData :: SyncState -> Value
        renderAsData (Restoring sp) = toJSON sp
        renderAsData Synced         = Null

instance FromJSON SyncState where
    parseJSON = withObject "SyncState" $ \ss -> do
        t <- ss .: "tag"
        case (t :: Text) of
            "synced"    -> pure Synced
            "restoring" -> Restoring <$> ss .: "data"
            _           -> typeMismatch "unrecognised tag" (Object ss)

instance ToSchema SyncState where
    declareNamedSchema _ = do
      syncProgress <- declareSchemaRef @SyncProgress Proxy
      pure $ NamedSchema (Just "SyncState") $ mempty
          & type_ .~ SwaggerObject
          & required .~ ["tag"]
          & properties .~ (mempty
              & at "tag" ?~ (Inline $ mempty
                  & type_ .~ SwaggerString
                  & enum_ ?~ ["restoring", "synced"]
                  )
              & at "data" ?~ syncProgress
              )

instance Arbitrary SyncState where
  arbitrary = oneof [ Restoring <$> arbitrary
                    , pure Synced
                    ]

-- | 'Wallet' type.
data WalletType
    = WalletRegular
    -- ^ Regular Cardano wallet.
    | WalletExternal
    -- ^ External wallet (mobile app or hardware wallet).
    deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance Arbitrary WalletType where
    arbitrary = elements [minBound .. maxBound]

-- Drops the @Wallet@ prefix.
deriveJSON Serokell.defaultOptions { A.constructorTagModifier = drop 6 . map C.toLower
                                   } ''WalletType

instance ToSchema WalletType where
    declareNamedSchema _ = do
        pure $ NamedSchema (Just "WalletType") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["regular", "external"]

deriveSafeBuildable ''WalletType
instance BuildableSafeGen WalletType where
    buildSafeGen _ WalletRegular  = "regular"
    buildSafeGen _ WalletExternal = "external"

-- | A 'Wallet'.
data Wallet = Wallet {
      walId                         :: !WalletId
    , walName                       :: !WalletName
    , walBalance                    :: !(V1 Core.Coin)
    , walHasSpendingPassword        :: !Bool
    , walSpendingPasswordLastUpdate :: !(V1 Core.Timestamp)
    , walCreatedAt                  :: !(V1 Core.Timestamp)
    , walAssuranceLevel             :: !AssuranceLevel
    , walSyncState                  :: !SyncState
    , walType                       :: !WalletType
    } deriving (Eq, Ord, Show, Generic)

--
-- IxSet indices
--





deriveJSON Serokell.defaultOptions ''Wallet

instance ToSchema Wallet where
    declareNamedSchema =
        genericSchemaDroppingPrefix "wal" (\(--^) props -> props
            & "id"
            --^ "Unique wallet identifier."
            & "name"
            --^ "Wallet's name."
            & "balance"
            --^ "Current balance, in Lovelace."
            & "hasSpendingPassword"
            --^ "Whether or not the wallet has a passphrase."
            & "spendingPasswordLastUpdate"
            --^ "The timestamp that the passphrase was last updated."
            & "createdAt"
            --^ "The timestamp that the wallet was created."
            & "assuranceLevel"
            --^ "The assurance level of the wallet."
            & "syncState"
            --^ "The sync state for this wallet."
            & "type"
            --^ "Wallet type: regular wallet or external one (mobile app or hardware wallet)."
        )

instance Arbitrary Wallet where
  arbitrary = Wallet <$> arbitrary
                     <*> pure "My wallet"
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

deriveSafeBuildable ''Wallet
instance BuildableSafeGen Wallet where
  buildSafeGen sl Wallet{..} = bprint ("{"
    %" id="%buildSafe sl
    %" name="%buildSafe sl
    %" balance="%buildSafe sl
    %" type="%buildSafe sl
    %" }")
    walId
    walName
    walBalance
    walType

instance Buildable [Wallet] where
    build = bprint listJson

-- | An external wallet (mobile client or hardware wallet).
data ExternalWallet = ExternalWallet
    { ewalId      :: !WalletId
    , ewalName    :: !WalletName
    , ewalBalance :: !(V1 Core.Coin)
    } deriving (Eq, Ord, Show, Generic)

deriveJSON Serokell.defaultOptions ''ExternalWallet
instance ToSchema ExternalWallet where
    declareNamedSchema =
        genericSchemaDroppingPrefix "ewal" (\(--^) props -> props
            & ("id"      --^ "Unique wallet identifier.")
            & ("name"    --^ "Wallet's name.")
            & ("balance" --^ "Current balance, in Lovelaces.")
        )

instance Arbitrary ExternalWallet where
    arbitrary = ExternalWallet <$> arbitrary
                               <*> pure "My external wallet"
                               <*> arbitrary

--------------------------------------------------------------------------------
-- Addresses
--------------------------------------------------------------------------------

-- | Whether an address is valid or not.
newtype AddressValidity = AddressValidity { isValid :: Bool }
  deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions ''AddressValidity

instance ToSchema AddressValidity where
    declareNamedSchema = genericSchemaDroppingPrefix "is" (const identity)

instance Arbitrary AddressValidity where
  arbitrary = AddressValidity <$> arbitrary

deriveSafeBuildable ''AddressValidity
instance BuildableSafeGen AddressValidity where
    buildSafeGen _ AddressValidity{..} =
        bprint ("{ valid="%build%" }") isValid

-- | An address is either recognised as "ours" or not. An address that is not
--   recognised may still be ours e.g. an address generated by another wallet instance
--   will not be considered "ours" until the relevant transaction is confirmed.
--
--   In other words, `AddressAmbiguousOwnership` makes an inconclusive statement about
--   an address, whereas `AddressOwnership` is unambiguous.
data AddressOwnership
    = AddressIsOurs
    | AddressAmbiguousOwnership
    deriving (Show, Eq, Generic, Ord)

instance ToJSON (V1 AddressOwnership) where
    toJSON = genericToJSON optsADTCamelCase . unV1

instance FromJSON (V1 AddressOwnership) where
    parseJSON = fmap V1 . genericParseJSON optsADTCamelCase

instance ToSchema (V1 AddressOwnership) where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "V1AddressOwnership") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["isOurs", "ambiguousOwnership"]

instance Arbitrary (V1 AddressOwnership) where
    arbitrary = fmap V1 $ oneof
        [ pure AddressIsOurs
        , pure AddressAmbiguousOwnership
        ]

-- | Address with associated metadata locating it in an account in a wallet.
data WAddressMeta = WAddressMeta
    { _wamWalletId     :: !WalletId
    , _wamAccountIndex :: !Word32
    , _wamAddressIndex :: !Word32
    , _wamAddress      :: !(V1 Core.Address)
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance Hashable WAddressMeta
instance NFData WAddressMeta

instance Buildable WAddressMeta where
    build WAddressMeta{..} =
        bprint (build%"@"%build%"@"%build%" ("%build%")")
        _wamWalletId _wamAccountIndex _wamAddressIndex _wamAddress
--------------------------------------------------------------------------------
-- Accounts
--------------------------------------------------------------------------------

-- | Summary about single address.
data WalletAddress = WalletAddress
    { addrId            :: !(V1 Core.Address)
    , addrUsed          :: !Bool
    , addrChangeAddress :: !Bool
    , addrOwnership     :: !(V1 AddressOwnership)
    } deriving (Show, Eq, Generic, Ord)

deriveJSON Serokell.defaultOptions ''WalletAddress

instance ToSchema WalletAddress where
    declareNamedSchema =
        genericSchemaDroppingPrefix "addr" (\(--^) props -> props
            & ("id"            --^ "Actual address.")
            & ("used"          --^ "True if this address has been used.")
            & ("changeAddress" --^ "True if this address stores change from a previous transaction.")
            & ("ownership"     --^ "'isOurs' if this address is recognised as ours, 'ambiguousOwnership' if the node doesn't have information to make a unambiguous statement.")
        )

instance Arbitrary WalletAddress where
    arbitrary = WalletAddress <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary

newtype AccountIndex = AccountIndex { getAccIndex :: Word32 }
    deriving (Show, Eq, Ord, Generic)

newtype AccountIndexError = AccountIndexError Word32
    deriving (Eq, Show)

instance Buildable AccountIndexError where
    build (AccountIndexError i) =
        bprint
            ("Account index should be in range ["%int%".."%int%"], but "%int%" was provided.")
            (getAccIndex minBound)
            (getAccIndex maxBound)
            i

mkAccountIndex :: Word32 -> Either AccountIndexError AccountIndex
mkAccountIndex index
    | index >= getAccIndex minBound = Right $ AccountIndex index
    | otherwise = Left $ AccountIndexError index

mkAccountIndexM :: MonadFail m => Word32 -> m AccountIndex
mkAccountIndexM =
    either (fail . toString . sformat build) pure . mkAccountIndex

unsafeMkAccountIndex :: Word32 -> AccountIndex
unsafeMkAccountIndex =
    either (error . sformat build) identity . mkAccountIndex

instance Bounded AccountIndex where
    -- NOTE: minimum for hardened key. See https://iohk.myjetbrains.com/youtrack/issue/CO-309
    minBound = AccountIndex 2147483648
    maxBound = AccountIndex maxBound

instance ToJSON AccountIndex where
    toJSON = toJSON . getAccIndex

instance FromJSON AccountIndex where
    parseJSON =
        mkAccountIndexM <=< parseJSON

instance Arbitrary AccountIndex where
    arbitrary =
        AccountIndex <$> choose (getAccIndex minBound, getAccIndex maxBound)

deriveSafeBuildable ''AccountIndex
-- Nothing secret to redact for a AccountIndex.
instance BuildableSafeGen AccountIndex where
    buildSafeGen _ =
        bprint build . getAccIndex

instance ToParamSchema AccountIndex where
    toParamSchema _ = mempty
        & type_ .~ SwaggerNumber
        & minimum_ .~ Just (fromIntegral $ getAccIndex minBound)
        & maximum_ .~ Just (fromIntegral $ getAccIndex maxBound)

instance ToSchema AccountIndex where
    declareNamedSchema =
        pure . paramSchemaToNamedSchema defaultSchemaOptions

instance FromHttpApiData AccountIndex where
    parseQueryParam =
        first (sformat build) . mkAccountIndex <=< parseQueryParam

instance ToHttpApiData AccountIndex where
    toQueryParam =
        fromString . show . getAccIndex


-- | A wallet 'Account'.
data Account = Account
    { accIndex     :: !AccountIndex
    , accAddresses :: ![WalletAddress]
    , accAmount    :: !(V1 Core.Coin)
    , accName      :: !Text
    , accWalletId  :: !WalletId
    } deriving (Show, Ord, Eq, Generic)


--
-- IxSet indices
--





-- | Datatype wrapping addresses for per-field endpoint
newtype AccountAddresses = AccountAddresses
    { acaAddresses :: [WalletAddress]
    } deriving (Show, Ord, Eq, Generic)

-- | Datatype wrapping balance for per-field endpoint
newtype AccountBalance = AccountBalance
    { acbAmount    :: V1 Core.Coin
    } deriving (Show, Ord, Eq, Generic)

accountsHaveSameId :: Account -> Account -> Bool
accountsHaveSameId a b =
    accWalletId a == accWalletId b
    &&
    accIndex a == accIndex b

deriveJSON Serokell.defaultOptions ''Account
deriveJSON Serokell.defaultOptions ''AccountAddresses
deriveJSON Serokell.defaultOptions ''AccountBalance

instance ToSchema Account where
    declareNamedSchema =
        genericSchemaDroppingPrefix "acc" (\(--^) props -> props
            & ("index"     --^ "Account's index in the wallet, starting at 0.")
            & ("addresses" --^ "Public addresses pointing to this account.")
            & ("amount"    --^ "Available funds, in Lovelace.")
            & ("name"      --^ "Account's name.")
            & ("walletId"  --^ "Id of the wallet this account belongs to.")
          )

instance ToSchema AccountAddresses where
    declareNamedSchema =
        genericSchemaDroppingPrefix "aca" (\(--^) props -> props
            & ("addresses" --^ "Public addresses pointing to this account.")
          )

instance ToSchema AccountBalance where
    declareNamedSchema =
        genericSchemaDroppingPrefix "acb" (\(--^) props -> props
            & ("amount"    --^ "Available funds, in Lovelace.")
          )

instance Arbitrary Account where
    arbitrary = Account <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> pure "My account"
                        <*> arbitrary

instance Arbitrary AccountAddresses where
    arbitrary =
        AccountAddresses <$> arbitrary

instance Arbitrary AccountBalance where
    arbitrary =
        AccountBalance <$> arbitrary

deriveSafeBuildable ''Account
instance BuildableSafeGen Account where
    buildSafeGen sl Account{..} = bprint ("{"
        %" index="%buildSafe sl
        %" name="%buildSafe sl
        %" addresses="%buildSafe sl
        %" amount="%buildSafe sl
        %" walletId="%buildSafe sl
        %" }")
        accIndex
        accName
        accAddresses
        accAmount
        accWalletId

instance Buildable AccountAddresses where
    build =
        bprint listJson . acaAddresses

instance Buildable AccountBalance where
    build =
        bprint build . acbAmount

instance Buildable [Account] where
    build =
        bprint listJson

-- | Account Update
data AccountUpdate = AccountUpdate {
    uaccName      :: !Text
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''AccountUpdate

instance ToSchema AccountUpdate where
  declareNamedSchema =
    genericSchemaDroppingPrefix "uacc" (\(--^) props -> props
      & ("name" --^ "New account's name.")
    )

instance Arbitrary AccountUpdate where
  arbitrary = AccountUpdate <$> pure "myAccount"

deriveSafeBuildable ''AccountUpdate
instance BuildableSafeGen AccountUpdate where
    buildSafeGen sl AccountUpdate{..} =
        bprint ("{ name="%buildSafe sl%" }") uaccName


-- | New Account
data NewAccount = NewAccount
  { naccSpendingPassword :: !(Maybe SpendingPassword)
  , naccName             :: !Text
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''NewAccount

instance Arbitrary NewAccount where
  arbitrary = NewAccount <$> arbitrary
                         <*> arbitrary

instance ToSchema NewAccount where
  declareNamedSchema =
    genericSchemaDroppingPrefix "nacc" (\(--^) props -> props
      & ("spendingPassword" --^ "Wallet's protection password, required if defined.")
      & ("name"             --^ "Account's name.")
    )

deriveSafeBuildable ''NewAccount
instance BuildableSafeGen NewAccount where
    buildSafeGen sl NewAccount{..} = bprint ("{"
        %" spendingPassword="%(buildSafeMaybe mempty sl)
        %" name="%buildSafe sl
        %" }")
        naccSpendingPassword
        naccName

deriveSafeBuildable ''WalletAddress
instance BuildableSafeGen WalletAddress where
    buildSafeGen sl WalletAddress{..} = bprint ("{"
        %" id="%buildSafe sl
        %" used="%build
        %" changeAddress="%build
        %" }")
        addrId
        addrUsed
        addrChangeAddress

instance Buildable [WalletAddress] where
    build = bprint listJson


-- | Create a new Address
data NewAddress = NewAddress
  { newaddrSpendingPassword :: !(Maybe SpendingPassword)
  , newaddrAccountIndex     :: !AccountIndex
  , newaddrWalletId         :: !WalletId
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''NewAddress

instance ToSchema NewAddress where
  declareNamedSchema =
    genericSchemaDroppingPrefix "newaddr" (\(--^) props -> props
      & ("spendingPassword" --^ "Wallet's protection password, required if defined.")
      & ("accountIndex"     --^ "Target account's index to store this address in.")
      & ("walletId"         --^ "Corresponding wallet identifier.")
    )

instance Arbitrary NewAddress where
  arbitrary = NewAddress <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary

deriveSafeBuildable ''NewAddress
instance BuildableSafeGen NewAddress where
    buildSafeGen sl NewAddress{..} = bprint("{"
        %" spendingPassword="%(buildSafeMaybe mempty sl)
        %" accountIndex="%buildSafe sl
        %" walletId="%buildSafe sl
        %" }")
        newaddrSpendingPassword
        newaddrAccountIndex
        newaddrWalletId


type AddressIndex = Word32

-- | If we're talking about "hardened" indexes - we mean values above
-- 2^31 == maxBound `div` 2 + 1
hardenedValue :: Word32
hardenedValue = maxBound `div` 2 + 1

addressLevelToWord32 :: AddressLevel -> Word32
addressLevelToWord32 (AddressLevelNormal lvl)   = lvl
addressLevelToWord32 (AddressLevelHardened lvl) = lvl + hardenedValue

word32ToAddressLevel :: Word32 -> AddressLevel
word32ToAddressLevel lvl =
    if lvl <= hardenedValue then
        AddressLevelNormal lvl
    else
        AddressLevelHardened (lvl - hardenedValue)

data AddressLevel
    = AddressLevelHardened Word32
    | AddressLevelNormal Word32
    deriving (Eq, Generic, Ord, Show)

instance ToSchema AddressLevel where
    declareNamedSchema _ = do
        NamedSchema _ word32Schema <- declareNamedSchema (Proxy @Word32)
        pure $ NamedSchema (Just "AddressLevel") $ word32Schema
            & description ?~ mconcat
                [ "Address path level according to BIP-32 definition. "
                , "Levels in the (0..2^31-1) range are treated as normal, "
                , "those in the (2^31..2^32-1) range are treated as hardened."
                ]

instance Arbitrary AddressLevel where
    arbitrary = oneof
        [ AddressLevelHardened <$> arbitrary
        , AddressLevelNormal   <$> arbitrary
        ]

deriveSafeBuildable ''AddressLevel
instance BuildableSafeGen AddressLevel where
    buildSafeGen sl = \case
        AddressLevelNormal lvl   -> bprint (buildSafe sl) lvl
        AddressLevelHardened lvl -> bprint (buildSafe sl%"'") lvl

instance ToJSON AddressLevel where
    toJSON = toJSON . addressLevelToWord32

instance FromJSON AddressLevel where
    parseJSON = fmap word32ToAddressLevel . parseJSON

newtype IsChangeAddress = IsChangeAddress Bool deriving (Show, Eq)

-- | BIP44 derivation path, for work with external wallets, for example:
-- m / purpose' / coin_type' / account' / change / address_index
-- m /      44' /      1815' /       0' /      0 /             1
--
-- NOTE See:
--   - https://github.com/satoshilabs/slips/blob/master/slip-0044.md
--   - https://github.com/satoshilabs/slips/pull/123
data AddressPath = AddressPath
    { addrpathPurpose      :: AddressLevel
    , addrpathCoinType     :: AddressLevel
    , addrpathAccount      :: AddressLevel
    , addrpathChange       :: AddressLevel
    , addrpathAddressIndex :: AddressLevel
    } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''AddressPath

instance ToSchema AddressPath where
    declareNamedSchema =
        genericSchemaDroppingPrefix "addrpath" (\(--^) props -> props
            & ("purpose"      --^ "44, refers to BIP-44.")
            & ("coinType"     --^ "1815 for ADA (Ada Lovelace's birthdate).")
            & ("account"      --^ "Account index, used as child index in BIP-32 derivation.")
            & ("change"       --^ "0 if external (e.g. payment addr), 1 if internal (e.g. change addr).")
            & ("addressIndex" --^ "Address index counter, incremental.")
        )

instance Arbitrary AddressPath where
    arbitrary = AddressPath <$> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary

deriveSafeBuildable ''AddressPath
instance BuildableSafeGen AddressPath where
    buildSafeGen sl AddressPath{..} = bprint (""
        %"{ purpose="%buildSafe sl
        %", coin_type="%buildSafe sl
        %", account="%buildSafe sl
        %", change="%buildSafe sl
        %", address_index="%buildSafe sl
        %" }")
        addrpathPurpose
        addrpathCoinType
        addrpathAccount
        addrpathChange
        addrpathAddressIndex

-- Smart constructor to create BIP44 derivation paths
--
-- NOTE Our account indexes are already referred to as "hardened" and
-- are therefore referred to as indexes above 2^31 == maxBound `div` 2 + 1
-- AddressPath makes it explicit whether a path should be hardened or not
-- instead of relying on a convention on number.
mkAddressPathBIP44
    :: IsChangeAddress   -- ^ Whether this is an internal (for change) or external address (for payments)
    -> Account           -- ^ Underlying account
    -> Maybe AddressPath -- ^ If 'Nothing' - address level is out-of-bound (31-byte unsigned integer).
mkAddressPathBIP44 (IsChangeAddress isChange) acct = AddressPath
    <$> pure (AddressLevelHardened 44)
    <*> pure (AddressLevelHardened 1815)
    <*> pure (word32ToAddressLevel actualIndex)
    <*> pure (AddressLevelNormal $ if isChange then 1 else 0)
    <*> checkWord31 AddressLevelNormal (getAddressIndex acct)
  where
    getAddressIndex = fromIntegral . length . accAddresses
    checkWord31 mkLevel n =
        if n >= hardenedValue then
            Nothing
        else
            Just (mkLevel n)
    AccountIndex actualIndex = accIndex acct


-- | A type incapsulating a password update request.
data PasswordUpdate = PasswordUpdate {
    pwdOld :: !SpendingPassword
  , pwdNew :: !SpendingPassword
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''PasswordUpdate

instance ToSchema PasswordUpdate where
  declareNamedSchema =
    genericSchemaDroppingPrefix "pwd" (\(--^) props -> props
      & ("old" --^ "Old password.")
      & ("new" --^ "New passowrd.")
    )

instance Arbitrary PasswordUpdate where
  arbitrary = PasswordUpdate <$> arbitrary
                             <*> arbitrary

deriveSafeBuildable ''PasswordUpdate
instance BuildableSafeGen PasswordUpdate where
    buildSafeGen sl PasswordUpdate{..} = bprint("{"
        %" old="%buildSafe sl
        %" new="%buildSafe sl
        %" }")
        pwdOld
        pwdNew


-- | 'EstimatedFees' represents the fees which would be generated
-- for a 'Payment' in case the latter would actually be performed.
data EstimatedFees = EstimatedFees {
    feeEstimatedAmount :: !(V1 Core.Coin)
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''EstimatedFees

instance ToSchema EstimatedFees where
  declareNamedSchema =
    genericSchemaDroppingPrefix "fee" (\(--^) props -> props
      & ("estimatedAmount" --^ "Estimated fees, in Lovelace.")
    )

instance Arbitrary EstimatedFees where
  arbitrary = EstimatedFees <$> arbitrary

deriveSafeBuildable ''EstimatedFees
instance BuildableSafeGen EstimatedFees where
    buildSafeGen sl EstimatedFees{..} = bprint("{"
        %" estimatedAmount="%buildSafe sl
        %" }")
        feeEstimatedAmount


-- | Maps an 'Address' to some 'Coin's, and it's
-- typically used to specify where to send money during a 'Payment'.
data PaymentDistribution = PaymentDistribution {
      pdAddress :: !(V1 Core.Address)
    , pdAmount  :: !(V1 Core.Coin)
    } deriving (Show, Ord, Eq, Generic)

deriveJSON Serokell.defaultOptions ''PaymentDistribution

instance ToSchema PaymentDistribution where
  declareNamedSchema =
    genericSchemaDroppingPrefix "pd" (\(--^) props -> props
      & ("address" --^ "Address to map coins to.")
      & ("amount"  --^ "Amount of coin to bind, in Lovelace.")
    )

instance Arbitrary PaymentDistribution where
  arbitrary = PaymentDistribution <$> arbitrary
                                  <*> arbitrary

deriveSafeBuildable ''PaymentDistribution
instance BuildableSafeGen PaymentDistribution where
    buildSafeGen sl PaymentDistribution{..} = bprint ("{"
        %" address="%buildSafe sl
        %" amount="%buildSafe sl
        %" }")
        pdAddress
        pdAmount


-- | A 'PaymentSource' encapsulate two essentially piece of data to reach for some funds:
-- a 'WalletId' and an 'AccountIndex' within it.
data PaymentSource = PaymentSource
  { psWalletId     :: !WalletId
  , psAccountIndex :: !AccountIndex
  } deriving (Show, Ord, Eq, Generic)

deriveJSON Serokell.defaultOptions ''PaymentSource

instance ToSchema PaymentSource where
  declareNamedSchema =
    genericSchemaDroppingPrefix "ps" (\(--^) props -> props
      & ("walletId"     --^ "Target wallet identifier to reach.")
      & ("accountIndex" --^ "Corresponding account's index on the wallet.")
    )

instance Arbitrary PaymentSource where
  arbitrary = PaymentSource <$> arbitrary
                            <*> arbitrary

deriveSafeBuildable ''PaymentSource
instance BuildableSafeGen PaymentSource where
    buildSafeGen sl PaymentSource{..} = bprint ("{"
        %" walletId="%buildSafe sl
        %" accountIndex="%buildSafe sl
        %" }")
        psWalletId
        psAccountIndex


-- | A 'Payment' from one source account to one or more 'PaymentDistribution'(s).
data Payment = Payment
  { pmtSource           :: !PaymentSource
  , pmtDestinations     :: !(NonEmpty PaymentDistribution)
  , pmtGroupingPolicy   :: !(Maybe (V1 Core.InputSelectionPolicy))
  , pmtSpendingPassword :: !(Maybe SpendingPassword)
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
            & enum_ ?~ ["OptimizeForSecurity", "OptimizeForHighThroughput"]

instance Arbitrary (V1 Core.InputSelectionPolicy) where
    arbitrary = fmap V1 arbitrary


deriveJSON Serokell.defaultOptions ''Payment

instance Arbitrary Payment where
  arbitrary = Payment <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary

instance ToSchema Payment where
  declareNamedSchema =
    genericSchemaDroppingPrefix "pmt" (\(--^) props -> props
      & ("source"           --^ "Source for the payment.")
      & ("destinations"     --^ "One or more destinations for the payment.")
      & ("groupingPolicy"   --^ "Optional strategy to use for selecting the transaction inputs.")
      & ("spendingPassword" --^ "Wallet's protection password, required to spend funds if defined.")
    )

deriveSafeBuildable ''Payment
instance BuildableSafeGen Payment where
    buildSafeGen sl (Payment{..}) = bprint ("{"
        %" source="%buildSafe sl
        %" destinations="%buildSafeList sl
        %" groupingPolicty="%build
        %" spendingPassword="%(buildSafeMaybe mempty sl)
        %" }")
        pmtSource
        (toList pmtDestinations)
        pmtGroupingPolicy
        pmtSpendingPassword

----------------------------------------------------------------------------
-- TxId
----------------------------------------------------------------------------
instance Arbitrary (V1 Txp.TxId) where
  arbitrary = V1 <$> arbitrary

instance ToJSON (V1 Txp.TxId) where
  toJSON (V1 t) = String (sformat hashHexF t)

instance FromJSON (V1 Txp.TxId) where
    parseJSON = withText "TxId" $ \t -> do
       case decodeHash t of
           Left err -> fail $ "Failed to parse transaction ID: " <> toString err
           Right a  -> pure (V1 a)

instance FromHttpApiData (V1 Txp.TxId) where
    parseQueryParam = fmap (fmap V1) decodeHash

instance ToHttpApiData (V1 Txp.TxId) where
    toQueryParam (V1 txId) = sformat hashHexF txId

instance ToSchema (V1 Txp.TxId) where
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
deriveJSON defaultOptions { A.constructorTagModifier = reverse . drop 11 . reverse . map C.toLower
                          } ''TransactionType

instance ToSchema TransactionType where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "TransactionType") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["local", "foreign"]
            & description ?~ mconcat
                [ "A transaction is 'local' if all the inputs and outputs "
                , "belong to the current wallet. A transaction is foreign "
                , "if the transaction is not local to this wallet."
                ]

deriveSafeBuildable ''TransactionType
instance BuildableSafeGen TransactionType where
    buildSafeGen _ LocalTransaction   = "local"
    buildSafeGen _ ForeignTransaction = "foreign"


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
deriveJSON defaultOptions { A.constructorTagModifier = reverse . drop 11 . reverse . map C.toLower
                          } ''TransactionDirection

instance ToSchema TransactionDirection where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "TransactionDirection") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["outgoing", "incoming"]

-- | This is an information-less variant of 'PtxCondition'.
data TransactionStatus
    = Applying
    | InNewestBlocks
    | Persisted
    | WontApply
    | Creating
    deriving (Eq, Show, Ord)

allTransactionStatuses :: [TransactionStatus]
allTransactionStatuses =
    [Applying, InNewestBlocks, Persisted, WontApply, Creating]

transactionStatusToText :: TransactionStatus -> Text
transactionStatusToText x = case x of
    Applying {} ->
        "applying"
    InNewestBlocks {} ->
        "inNewestBlocks"
    Persisted {} ->
        "persisted"
    WontApply {} ->
        "wontApply"
    Creating {} ->
        "creating"

instance ToJSON TransactionStatus where
    toJSON x = object
        [ "tag" .= transactionStatusToText x
        , "data" .= Object mempty
        ]

instance ToSchema TransactionStatus where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "TransactionStatus") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["tag", "data"]
            & properties .~ (mempty
                & at "tag" ?~ Inline (mempty
                    & type_ .~ SwaggerString
                    & enum_ ?~
                        map (String . transactionStatusToText)
                            allTransactionStatuses
                )
                & at "data" ?~ Inline (mempty
                    & type_ .~ SwaggerObject
                )
            )

instance FromJSON TransactionStatus where
    parseJSON = withObject "TransactionStatus" $ \o -> do
       tag <- o .: "tag"
       case tag of
           "applying" ->
                pure Applying
           "inNewestBlocks" ->
                pure InNewestBlocks
           "persisted" ->
                pure Persisted
           "wontApply" ->
                pure WontApply
           "creating" ->
                pure Creating
           _ ->
                fail $ "Couldn't parse out of " ++ toString (tag :: Text)

instance Arbitrary TransactionStatus where
    arbitrary = elements allTransactionStatuses

deriveSafeBuildable ''TransactionDirection
instance BuildableSafeGen TransactionDirection where
    buildSafeGen _ IncomingTransaction = "incoming"
    buildSafeGen _ OutgoingTransaction = "outgoing"

-- | A 'Wallet''s 'Transaction'.
data Transaction = Transaction
  { txId            :: !(V1 Txp.TxId)
  , txConfirmations :: !Word
  , txAmount        :: !(V1 Core.Coin)
  , txInputs        :: !(NonEmpty PaymentDistribution)
  , txOutputs       :: !(NonEmpty PaymentDistribution)
    -- ^ The output money distribution.
  , txType          :: !TransactionType
    -- ^ The type for this transaction (e.g local, foreign, etc).
  , txDirection     :: !TransactionDirection
    -- ^ The direction for this transaction (e.g incoming, outgoing).
  , txCreationTime  :: !(V1 Core.Timestamp)
    -- ^ The time when transaction was created.
  , txStatus        :: !TransactionStatus
  } deriving (Show, Ord, Eq, Generic)

deriveJSON Serokell.defaultOptions ''Transaction

instance ToSchema Transaction where
  declareNamedSchema =
    genericSchemaDroppingPrefix "tx" (\(--^) props -> props
      & ("id"            --^ "Transaction's id.")
      & ("confirmations" --^ "Number of confirmations.")
      & ("amount"        --^ "Coins moved as part of the transaction, in Lovelace.")
      & ("inputs"        --^ "One or more input money distributions.")
      & ("outputs"       --^ "One or more ouputs money distributions.")
      & ("type"          --^ "Whether the transaction is entirely local or foreign.")
      & ("direction"     --^ "Direction for this transaction.")
      & ("creationTime"  --^ "Timestamp indicating when the transaction was created.")
      & ("status"        --^ "Shows whether or not the transaction is accepted.")
    )

instance Arbitrary Transaction where
  arbitrary = Transaction <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary

deriveSafeBuildable ''Transaction
instance BuildableSafeGen Transaction where
    buildSafeGen sl Transaction{..} = bprint ("{"
        %" id="%buildSafe sl
        %" confirmations="%build
        %" amount="%buildSafe sl
        %" inputs="%buildSafeList sl
        %" outputs="%buildSafeList sl
        %" type="%buildSafe sl
        %" direction"%buildSafe sl
        %" }")
        txId
        txConfirmations
        txAmount
        (toList txInputs)
        (toList txOutputs)
        txType
        txDirection

instance Buildable [Transaction] where
    build = bprint listJson

-- | Technically we have serialized 'Tx' here, from the core.
mkTransactionAsBase16 :: Txp.Tx -> TransactionAsBase16
mkTransactionAsBase16 =
    TransactionAsBase16Unsafe . Base16.encode . Bi.serialize'

rawTransactionAsBase16 :: TransactionAsBase16 -> Text
rawTransactionAsBase16 (TransactionAsBase16Unsafe txtTx) = txtTx

instance Buildable [AddressLevel] where
    build = bprint listJson

-- | Source address and corresponding derivation path, for external wallet.
data AddressAndPath = AddressAndPath
    { aapSrcAddress     :: !AddressAsBase58
    -- ^ Source address in Base58-format.
    , aapDerivationPath :: ![AddressLevel]
    -- ^ Derivation path used during generation of this address.
    } deriving (Show, Ord, Eq, Generic)

deriveJSON Serokell.defaultOptions ''AddressAndPath

instance ToSchema AddressAndPath where
    declareNamedSchema =
        genericSchemaDroppingPrefix "aap" (\(--^) props -> props
            & ("srcAddress"     --^ "Source address that corresponds to transaction input.")
            & ("derivationPath" --^ "Derivation path corresponding to this address.")
        )

instance Arbitrary AddressAndPath where
    arbitrary = AddressAndPath <$> arbitrary
                               <*> arbitrary

deriveSafeBuildable ''AddressAndPath
instance BuildableSafeGen AddressAndPath where
    buildSafeGen sl AddressAndPath{..} = bprint ("{"
        %" srcAddress="%buildSafe sl
        %" derivationPath="%buildSafe sl
        %" }")
        aapSrcAddress
        aapDerivationPath

instance Buildable [AddressAndPath] where
    build = bprint listJson

-- | A 'Wallet''s 'UnsignedTransaction'.
data UnsignedTransaction = UnsignedTransaction
    { rtxHex          :: !TransactionAsBase16
    -- ^ Serialized transaction in Base16-format.
    , rtxSrcAddresses :: ![AddressAndPath]
    -- ^ Addresses (with derivation paths) which will be used as a sources of money.
    } deriving (Show, Ord, Eq, Generic)

deriveJSON Serokell.defaultOptions ''UnsignedTransaction

instance ToSchema UnsignedTransaction where
    declareNamedSchema =
        genericSchemaDroppingPrefix "rtx" (\(--^) props -> props
            & ("hex"          --^ "New raw transaction in Base16-format.")
            & ("srcAddresses" --^ "Source addresses (with derivation paths), correspond to transaction inputs.")
        )

instance Arbitrary UnsignedTransaction where
    arbitrary = UnsignedTransaction <$> arbitrary
                                    <*> arbitrary

deriveSafeBuildable ''UnsignedTransaction
instance BuildableSafeGen UnsignedTransaction where
    buildSafeGen sl UnsignedTransaction{..} = bprint ("{"
        %" hex="%buildSafe sl
        %" srcAddresses="%buildSafe sl
        %" }")
        rtxHex
        rtxSrcAddresses

-- | To proof that the external wallet has the right to spend the input,
-- it returns the source address, the signature and the derived PK of
-- the transaction input.
data AddressWithProof = AddressWithProof
    { awpSrcAddress  :: !AddressAsBase58
    -- ^ Base58-encoded source address.
    , awpTxSignature :: !TransactionSignatureAsBase16
    -- ^ Base16-encoded signature of transaction (made by derived SK).
    , awpDerivedPK   :: !PublicKeyAsBase58
    -- ^ Base58-encoded derived PK (corresponding to derived SK).
    } deriving (Show, Ord, Eq, Generic)

deriveJSON Serokell.defaultOptions ''AddressWithProof

instance ToSchema AddressWithProof where
    declareNamedSchema =
        genericSchemaDroppingPrefix "awp" (\(--^) props -> props
            & ("srcAddress"  --^ "Source address in Base58-format.")
            & ("txSignature" --^ "Transaction signature by derived SK.")
            & ("derivedPK"   --^ "Derived PK in Base58-format.")
        )

instance Arbitrary AddressWithProof where
    arbitrary = AddressWithProof <$> arbitrary
                                 <*> arbitrary
                                 <*> arbitrary

deriveSafeBuildable ''AddressWithProof
instance BuildableSafeGen AddressWithProof where
    buildSafeGen sl AddressWithProof{..} = bprint ("{"
        %" srcAddress="%buildSafe sl
        %" txSignature="%buildSafe sl
        %" derivedPK="%buildSafe sl
        %" }")
        awpSrcAddress
        awpTxSignature
        awpDerivedPK

instance Buildable [AddressWithProof] where
    build = bprint listJson

-- | A 'Wallet''s 'SignedTransaction'. It is assumed
-- that this transaction was signed on the client-side
-- (mobile client or hardware wallet).
data SignedTransaction = SignedTransaction
    { stxTransaction     :: !TransactionAsBase16
    -- ^ Serialized transaction in base16-format.
    , stxAddrsWithProofs :: ![AddressWithProof]
    -- ^ Addresses with proofs for inputs of this transaction.
    } deriving (Show, Ord, Eq, Generic)

deriveJSON Serokell.defaultOptions ''SignedTransaction
instance ToSchema SignedTransaction where
    declareNamedSchema =
        genericSchemaDroppingPrefix "stx" (\(--^) props -> props
            & ("transaction"     --^ "New transaction that wasn't submitted in the blockchain yet.")
            & ("addrsWithProofs" --^ "Source addresses with proofs for inputs.")
        )

instance Arbitrary SignedTransaction where
    arbitrary = SignedTransaction <$> arbitrary
                                  <*> arbitrary

deriveSafeBuildable ''SignedTransaction
instance BuildableSafeGen SignedTransaction where
    buildSafeGen sl SignedTransaction{..} = bprint ("{"
        %" transaction="%buildSafe sl
        %" addrsWithProofs="%buildSafe sl
        %" }")
        stxTransaction
        stxAddrsWithProofs

-- | We use it for external wallets: if it's already presented in wallet db,
-- we return a wallet info and complete transactions history as well.
data WalletAndTxHistory = WalletAndTxHistory
    { waltxsWallet       :: !Wallet
    , waltxsTransactions :: ![Transaction]
    } deriving (Eq, Ord, Show, Generic)

deriveJSON Serokell.defaultOptions ''WalletAndTxHistory

instance ToSchema WalletAndTxHistory where
    declareNamedSchema =
        genericSchemaDroppingPrefix "waltxs" (\(--^) props -> props
            & ("wallet"       --^ "Wallet information.")
            & ("transactions" --^ "List of all transactions related to this wallet.")
        )

instance Arbitrary WalletAndTxHistory where
    arbitrary = WalletAndTxHistory <$> arbitrary
                                   <*> arbitrary

deriveSafeBuildable ''WalletAndTxHistory
instance BuildableSafeGen WalletAndTxHistory where
    buildSafeGen sl WalletAndTxHistory{..} = bprint ("{"
        %" wallet="%buildSafe sl
        %" transactions="%buildSafe sl
        %" }")
        waltxsWallet
        waltxsTransactions


-- | A type representing an upcoming wallet update.
data WalletSoftwareUpdate = WalletSoftwareUpdate
  { updSoftwareVersion   :: !Text
  , updBlockchainVersion :: !Text
  , updScriptVersion     :: !Int
  -- Other types omitted for now.
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''WalletSoftwareUpdate

instance ToSchema WalletSoftwareUpdate where
  declareNamedSchema =
    genericSchemaDroppingPrefix "upd" (\(--^) props -> props
      & ("softwareVersion"   --^ "Current software (wallet) version.")
      & ("blockchainVersion" --^ "Version of the underlying blockchain.")
      & ("scriptVersion"     --^ "Update script version.")
    )

instance Arbitrary WalletSoftwareUpdate where
  arbitrary = WalletSoftwareUpdate <$> arbitrary
                                   <*> arbitrary
                                   <*> fmap getPositive arbitrary

deriveSafeBuildable ''WalletSoftwareUpdate
instance BuildableSafeGen WalletSoftwareUpdate where
    buildSafeGen _ WalletSoftwareUpdate{..} = bprint("{"
        %" softwareVersion="%build
        %" blockchainVersion="%build
        %" scriptVersion="%build
        %" }")
        updSoftwareVersion
        updBlockchainVersion
        updScriptVersion

-- | A type encapsulating enough information to import a wallet from a
-- backup file.
data WalletImport = WalletImport
  { wiSpendingPassword :: !(Maybe SpendingPassword)
  , wiFilePath         :: !FilePath
  } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''WalletImport

instance ToSchema WalletImport where
  declareNamedSchema =
    genericSchemaDroppingPrefix "wi" (\(--^) props -> props
      & ("spendingPassword"   --^ "An optional spending password to set for the imported wallet.")
      & ("filePath" --^ "The path to the .key file holding the backup.")
    )

instance Arbitrary WalletImport where
  arbitrary = WalletImport <$> arbitrary
                           <*> arbitrary

deriveSafeBuildable ''WalletImport
instance BuildableSafeGen WalletImport where
    buildSafeGen sl WalletImport{..} = bprint("{"
        %" spendingPassword="%build
        %" filePath="%build
        %" }")
        (maybe "null" (buildSafeGen sl) wiSpendingPassword)
        wiFilePath


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
                    & enum_ ?~ ["milliseconds"]
                    )
                )

deriveSafeBuildable ''SlotDuration
instance BuildableSafeGen SlotDuration where
    buildSafeGen _ (SlotDuration (MeasuredIn w)) =
        bprint (build%"ms") w


-- | The @static@ settings for this wallet node. In particular, we could group
-- here protocol-related settings like the slot duration, the transaction max size,
-- the current software version running on the node, etc.
data NodeSettings = NodeSettings {
     setSlotDuration   :: !SlotDuration
   , setSoftwareInfo   :: !(V1 Core.SoftwareVersion)
   , setProjectVersion :: !(V1 Version)
   , setGitRevision    :: !Text
   } deriving (Show, Eq, Generic)

#if !(MIN_VERSION_swagger2(2,2,2))
-- See note [Version Orphan]
instance ToSchema (V1 Version) where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "V1Version") $ mempty
            & type_ .~ SwaggerString

instance Buildable (V1 Version) where
    build (V1 v) = bprint shown v

instance Buildable (SecureLog (V1 Version)) where
    build (SecureLog x) = Formatting.Buildable.build x

instance ToJSON (V1 Version) where
    toJSON (V1 v) = toJSON (showVersion v)

instance FromJSON (V1 Version) where
    parseJSON (String v) = case readP_to_S parseVersion (T.unpack v) of
        (reverse -> ((ver,_):_)) -> pure (V1 ver)
        _                        -> mempty
    parseJSON x                  = typeMismatch "Not a valid Version" x

instance Arbitrary (V1 Version) where
    arbitrary = fmap V1 arbitrary


-- Note [Version Orphan]
-- I have opened a PR to add an instance of 'Version' to the swagger2
-- library. When the PR is merged, we can delete the instance here and remove the warning from the file.
-- PR: https://github.com/GetShopTV/swagger2/pull/152
#endif

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

instance FromJSON (V1 Core.SoftwareVersion) where
    parseJSON = withObject "V1SoftwareVersion" $ \o -> do
        V1 svAppName <- o .: "applicationName"
        svNumber <- o .: "version"
        pure $ V1 Core.SoftwareVersion{..}

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

deriveJSON Serokell.defaultOptions ''NodeSettings

instance ToSchema NodeSettings where
  declareNamedSchema =
    genericSchemaDroppingPrefix "set" (\(--^) props -> props
      & ("slotDuration"   --^ "Duration of a slot.")
      & ("softwareInfo"   --^ "Various pieces of information about the current software.")
      & ("projectVersion" --^ "Current project's version.")
      & ("gitRevision"    --^ "Git revision of this deployment.")
    )

instance Arbitrary NodeSettings where
    arbitrary = NodeSettings <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> pure "0e1c9322a"

deriveSafeBuildable ''NodeSettings
instance BuildableSafeGen NodeSettings where
    buildSafeGen _ NodeSettings{..} = bprint ("{"
        %" slotDuration="%build
        %" softwareInfo="%build
        %" projectRevision="%build
        %" gitRevision="%build
        %" }")
        setSlotDuration
        setSoftwareInfo
        setProjectVersion
        setGitRevision


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
    declareNamedSchema _ =
        pure $ NamedSchema (Just "LocalTimeDifference") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["quantity"]
            & properties .~ (mempty
                & at "quantity" ?~ (Inline $ mempty
                    & type_ .~ SwaggerNumber
                    )
                & at "unit" ?~ (Inline $ mempty
                    & type_ .~ SwaggerString
                    & enum_ ?~ ["microseconds"]
                    )
                )

deriveSafeBuildable ''LocalTimeDifference
instance BuildableSafeGen LocalTimeDifference where
    buildSafeGen _ (LocalTimeDifference (MeasuredIn w)) =
        bprint (build%"μs") w


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
    declareNamedSchema _ =
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
                    & enum_ ?~ ["blocks"]
                    )
                )

deriveSafeBuildable ''BlockchainHeight
instance BuildableSafeGen BlockchainHeight where
    buildSafeGen _ (BlockchainHeight (MeasuredIn w)) =
        bprint (build%" blocks") w

newtype TimeInfo
    = TimeInfo
    { timeDifferenceFromNtpServer :: Maybe LocalTimeDifference
    } deriving (Eq, Show, Generic)

instance ToSchema TimeInfo where
    declareNamedSchema = genericSchemaDroppingPrefix "time" $ \(--^) p -> p &
        "differenceFromNtpServer"
        --^ ("The difference in microseconds between the node time and the NTP "
          <> "server. This value will be null if the NTP server is "
          <> "unavailable.")

instance Arbitrary TimeInfo where
    arbitrary = TimeInfo <$> arbitrary

deriveSafeBuildable ''TimeInfo
instance BuildableSafeGen TimeInfo where
    buildSafeGen _ TimeInfo{..} = bprint ("{"
        %" differenceFromNtpServer="%build
        %" }")
        timeDifferenceFromNtpServer

deriveJSON Serokell.defaultOptions ''TimeInfo


availableSubscriptionStatus :: [SubscriptionStatus]
availableSubscriptionStatus = [Subscribed, Subscribing]

deriveSafeBuildable ''SubscriptionStatus
instance BuildableSafeGen SubscriptionStatus where
    buildSafeGen _ = \case
        Subscribed  -> "Subscribed"
        Subscribing -> "Subscribing"

deriveJSON Serokell.defaultOptions ''SubscriptionStatus

instance Arbitrary SubscriptionStatus where
    arbitrary =
        elements availableSubscriptionStatus

instance ToSchema SubscriptionStatus where
    declareNamedSchema _ = do
        let enum = toJSON <$> availableSubscriptionStatus
        pure $ NamedSchema (Just "SubscriptionStatus") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ enum

instance FromJSONKey NodeId where
    fromJSONKey =
        FromJSONKeyText (NodeId . EndPointAddress . encodeUtf8)

instance ToJSONKey NodeId where
    toJSONKey =
        toJSONKeyText (decodeUtf8 . getAddress)
      where
        getAddress (NodeId (EndPointAddress x)) = x

instance ToSchema NodeId where
    declareNamedSchema _ = pure $ NamedSchema (Just "NodeId") $ mempty
        & type_ .~ SwaggerString

instance Arbitrary NodeId where
    arbitrary = do
        ipv4  <- genIPv4
        port_ <- genPort
        idx   <- genIdx
        return . toNodeId $ ipv4 <> ":" <> port_ <> ":" <> idx
      where
        toNodeId = NodeId . EndPointAddress . encodeUtf8
        showT    = show :: Int -> Text
        genIdx   = showT <$> choose (0, 9)
        genPort  = showT <$> choose (1000, 8000)
        genIPv4  = T.intercalate "." <$> replicateM 4 (showT <$> choose (0, 255))


-- | The @dynamic@ information for this node.
data NodeInfo = NodeInfo {
     nfoSyncProgress          :: !SyncPercentage
   , nfoBlockchainHeight      :: !(Maybe BlockchainHeight)
   , nfoLocalBlockchainHeight :: !BlockchainHeight
   , nfoLocalTimeInformation  :: !TimeInfo
   , nfoSubscriptionStatus    :: Map NodeId SubscriptionStatus
   } deriving (Show, Eq, Generic)

deriveJSON Serokell.defaultOptions ''NodeInfo

instance ToSchema NodeInfo where
  declareNamedSchema =
    genericSchemaDroppingPrefix "nfo" (\(--^) props -> props
      & ("syncProgress"          --^ "Syncing progression, in percentage.")
      & ("blockchainHeight"      --^ "If known, the current blockchain height, in number of blocks.")
      & ("localBlockchainHeight" --^ "Local blockchain height, in number of blocks.")
      & ("localTimeInformation"  --^ "Information about the clock on this node.")
      & ("subscriptionStatus"    --^ "Is the node connected to the network?")
    )

instance Arbitrary NodeInfo where
    arbitrary = NodeInfo <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary

deriveSafeBuildable ''NodeInfo
instance BuildableSafeGen NodeInfo where
    buildSafeGen _ NodeInfo{..} = bprint ("{"
        %" syncProgress="%build
        %" blockchainHeight="%build
        %" localBlockchainHeight="%build
        %" localTimeDifference="%build
        %" subscriptionStatus="%listJson
        %" }")
        nfoSyncProgress
        nfoBlockchainHeight
        nfoLocalBlockchainHeight
        nfoLocalTimeInformation
        (Map.toList nfoSubscriptionStatus)

-- | A redemption mnemonic.
newtype RedemptionMnemonic = RedemptionMnemonic
    { unRedemptionMnemonic :: Mnemonic 9
    }
    deriving stock (Eq, Show, Generic)
    deriving newtype (ToJSON, FromJSON, Arbitrary)

instance ToSchema RedemptionMnemonic where
    declareNamedSchema _ = pure $
        NamedSchema (Just "RedemptionMnemonic") (toSchema (Proxy @(Mnemonic 9)))

-- | A shielded redemption code.
newtype ShieldedRedemptionCode = ShieldedRedemptionCode
    { unShieldedRedemptionCode :: Text
    } deriving (Eq, Show, Generic)
      deriving newtype (ToJSON, FromJSON)

-- | This instance could probably be improved. A 'ShieldedRedemptionCode' is
-- a hash of the redemption key.
instance Arbitrary ShieldedRedemptionCode where
    arbitrary = ShieldedRedemptionCode <$> arbitrary

instance ToSchema ShieldedRedemptionCode where
    declareNamedSchema _ =
        pure
            $ NamedSchema (Just "ShieldedRedemptionCode") $ mempty
            & type_ .~ SwaggerString

deriveSafeBuildable ''ShieldedRedemptionCode
instance BuildableSafeGen ShieldedRedemptionCode where
    buildSafeGen _ _ =
        bprint "<shielded redemption code>"

-- | The request body for redeeming some Ada.
data Redemption = Redemption
    { redemptionRedemptionCode   :: ShieldedRedemptionCode
    -- ^ The redemption code associated with the Ada to redeem.
    , redemptionMnemonic         :: Maybe RedemptionMnemonic
    -- ^ An optional mnemonic. This mnemonic was included with paper
    -- certificates, and the presence of this field indicates that we're
    -- doing a paper vend.
    , redemptionSpendingPassword :: SpendingPassword
    -- ^ The user must provide a spending password that matches the wallet that
    -- will be receiving the redemption funds.
    , redemptionWalletId         :: WalletId
    -- ^ Redeem to this wallet
    , redemptionAccountIndex     :: AccountIndex
    -- ^ Redeem to this account index in the wallet
    } deriving (Eq, Show, Generic)

deriveSafeBuildable ''Redemption
instance BuildableSafeGen Redemption where
    buildSafeGen sl r = bprint ("{"
        %" redemptionCode="%buildSafe sl
        %" mnemonic=<mnemonic>"
        %" spendingPassword="%buildSafe sl
        %" }")
        (redemptionRedemptionCode r)
        (redemptionSpendingPassword r)

deriveJSON Serokell.defaultOptions  ''Redemption

instance ToSchema Redemption where
    declareNamedSchema =
        genericSchemaDroppingPrefix "redemption" (\(--^) props -> props
            & "redemptionCode"
            --^ "The redemption code associated with the Ada to redeem."
            & "mnemonic"
            --^ ( "An optional mnemonic. This must be provided for a paper "
                <> "certificate redemption."
                )
            & "spendingPassword"
            --^ ( "An optional spending password. This must match the password "
                <> "for the provided wallet ID and account index."
                )
        )

instance Arbitrary Redemption where
    arbitrary = Redemption <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

data ForceNtpCheck
    = ForceNtpCheck
    | NoNtpCheck
    deriving (Eq)

instance Flaggable ForceNtpCheck where
    toBool ForceNtpCheck = True
    toBool NoNtpCheck    = False
    fromBool True  = ForceNtpCheck
    fromBool False = NoNtpCheck

deriveSafeBuildable ''ForceNtpCheck
instance BuildableSafeGen ForceNtpCheck where
    buildSafeGen _ ForceNtpCheck = "force ntp check"
    buildSafeGen _ NoNtpCheck    = "no ntp check"

--
-- POST/PUT requests isomorphisms
--

type family Update (original :: *) :: * where
    Update Wallet =
        WalletUpdate
    Update Account =
        AccountUpdate
    Update WalletAddress =
        () -- read-only

type family New (original :: *) :: * where
    New Wallet =
        NewWallet
    New Account =
        NewAccount
    New WalletAddress =
        NewAddress
    New ExternalWallet =
        NewExternalWallet

type CaptureWalletId = Capture "walletId" WalletId

type CaptureAccountId = Capture "accountId" AccountIndex

--
-- Example typeclass instances
--

instance Example Core.Address
instance Example AccountIndex
instance Example AccountBalance
instance Example AccountAddresses
instance Example AddressLevel
instance Example AddressPath
instance Example WalletId
instance Example AssuranceLevel
instance Example BlockchainHeight
instance Example LocalTimeDifference
instance Example PaymentDistribution
instance Example AccountUpdate
instance Example Wallet
instance Example ExternalWallet
instance Example WalletUpdate
instance Example WalletOperation
instance Example PasswordUpdate
instance Example EstimatedFees
instance Example Transaction
instance Example WalletSoftwareUpdate
instance Example NodeSettings
instance Example SlotDuration
instance Example WalletAddress
instance Example NewAccount
instance Example TimeInfo
instance Example AddressValidity
instance Example NewAddress
instance Example SubscriptionStatus
instance Example NodeId
instance Example ShieldedRedemptionCode
instance Example (V1 Core.PassPhrase)
instance Example (V1 Core.Coin)

-- | We have a specific 'Example' instance for @'V1' 'Address'@ because we want
-- to control the length of the examples. It is possible for the encoded length
-- to become huge, up to 1000+ bytes, if the 'UnsafeMultiKeyDistr' constructor
-- is used. We do not use this constructor, which keeps the address between
-- ~80-150 bytes long.
instance Example (V1 Core.Address) where
    example = fmap V1 . Core.makeAddress
        <$> arbitrary
        <*> arbitraryAttributes
      where
        arbitraryAttributes =
            Core.AddrAttributes
                <$> arbitrary
                <*> oneof
                    [ pure Core.BootstrapEraDistr
                    , Core.SingleKeyDistr <$> arbitrary
                    ]
                <*> arbitrary

instance Example BackupPhrase where
    example = pure (BackupPhrase def)

instance Example Core.InputSelectionPolicy where
    example = pure Core.OptimizeForHighThroughput

instance Example (V1 Core.InputSelectionPolicy) where
    example = pure (V1 Core.OptimizeForHighThroughput)

instance Example Account where
    example = Account <$> example
                      <*> example -- NOTE: this will produce non empty list
                      <*> example
                      <*> pure "My account"
                      <*> example

instance Example NewWallet where
    example = NewWallet <$> example
                        <*> example -- Note: will produce `Just a`
                        <*> example
                        <*> pure "My Wallet"
                        <*> example

instance Example AddressAsBase58 where
    example = AddressAsBase58Unsafe <$> pure
        "DdzFFzCqrhszBQmhPAbCwSrk5EcZjgRurtNzrGqxU3QYrT8NyGujgzoF1z3eS8CJBJJ71hFL4MXvBQ64j3jGU4UJBDHb4KKoLtiVsUvf"

instance Example PublicKeyAsBase58 where
    example = PublicKeyAsBase58Unsafe <$> pure
        "bNfWjshJG9xxy6VkpV2KurwGah3jQWjGb4QveDGZteaCwupdKWAi371r8uS5yFCny5i5EQuSNSLKqvRHmWEoHe45pZ"

instance Example TransactionAsBase16 where
    example = TransactionAsBase16Unsafe <$> pure
        "839f8200d8185826825820de3151a2d9cd8e2bbe292a6153d679d123892ddcfbee869c4732a5c504a7554d19386cff9f8282d818582183581caeb153a5809a084507854c9f3e5795bcca89781f9c386d957748cd42a0001a87236a1f1b00780aa6c7d62110ffa0"

instance Example TransactionSignatureAsBase16 where
    example = TransactionSignatureAsBase16Unsafe <$> pure
        "5840709cc240ac9ad78cbf47c3eec76df917423943e34339277593e8e2b8c9f9f2e59583023bfbd8e26c40dff6a7fa424600f9b942819533d8afee37a5ac6d813207"

instance Example NewExternalWallet where
    example = NewExternalWallet <$> example
                                <*> example
                                <*> pure "My external Wallet"
                                <*> example

instance Example NodeInfo where
    example = NodeInfo <$> example
                       <*> example  -- NOTE: will produce `Just a`
                       <*> example
                       <*> example
                       <*> example

instance Example PaymentSource where
    example = PaymentSource <$> example
                            <*> example

instance Example Payment where
    example = Payment <$> example
                      <*> example
                      <*> example -- TODO: will produce `Just groupingPolicy`
                      <*> example

instance Example Redemption where
    example = Redemption <$> example
                         <*> pure Nothing
                         <*> example
                         <*> example
                         <*> example

instance Example AddressAndPath where
    example = AddressAndPath <$> example
                             <*> example

instance Example UnsignedTransaction where
    example = UnsignedTransaction <$> example
                                  <*> example

instance Example WalletAndTxHistory where
    example = WalletAndTxHistory <$> example
                                 <*> example

instance Example AddressWithProof where
    example = AddressWithProof <$> example
                               <*> example
                               <*> example

instance Example SignedTransaction where
    example = SignedTransaction <$> example
                                <*> example

instance Example WalletImport where
    example = WalletImport <$> example
                           <*> pure "/Users/foo/Documents/wallet_to_import.key"

--
-- Wallet Errors
--

-- | Details about what 'NotEnoughMoney' means
data ErrNotEnoughMoney
    -- | UTxO exhausted whilst trying to pick inputs to cover remaining fee
    = ErrCannotCoverFee

    -- | UTxO exhausted during input selection
    --
    -- We record the available balance of the UTxO
    | ErrAvailableBalanceIsInsufficient Int

    deriving (Eq, Show, Generic)

instance Buildable ErrNotEnoughMoney where
    build = \case
        ErrCannotCoverFee ->
             bprint "Not enough coins to cover fee."
        ErrAvailableBalanceIsInsufficient _ ->
             bprint "Not enough available coins to proceed."

instance ToJSON ErrNotEnoughMoney where
    toJSON = \case
        e@ErrCannotCoverFee -> object
            [ "msg" .= sformat build e
            ]
        e@(ErrAvailableBalanceIsInsufficient balance) -> object
            [ "msg"              .= sformat build e
            , "availableBalance" .= balance
            ]

instance FromJSON ErrNotEnoughMoney where
    parseJSON v =
            withObject "AvailableBalanceIsInsufficient" availableBalanceIsInsufficientParser v
        <|> withObject "CannotCoverFee" cannotCoverFeeParser v
      where
        cannotCoverFeeParser :: Object -> Parser ErrNotEnoughMoney
        cannotCoverFeeParser o = do
            msg <- o .: "msg"
            when (msg /= sformat build ErrCannotCoverFee) mempty
            pure ErrCannotCoverFee

        availableBalanceIsInsufficientParser :: Object -> Parser ErrNotEnoughMoney
        availableBalanceIsInsufficientParser o = do
            msg <- o .: "msg"
            when (msg /= sformat build (ErrAvailableBalanceIsInsufficient 0)) mempty
            ErrAvailableBalanceIsInsufficient <$> (o .: "availableBalance")


-- | Type representing any error which might be thrown by wallet.
--
-- Errors are represented in JSON in the JSend format (<https://labs.omniti.com/labs/jsend>):
-- ```
-- {
--     "status": "error"
--     "message" : <constr_name>,
--     "diagnostic" : <data>
-- }
-- ```
-- where `<constr_name>` is a string containing name of error's constructor (e. g. `NotEnoughMoney`),
-- and `<data>` is an object containing additional error data.
-- Additional data contains constructor fields, field names are record field names without
-- a `we` prefix, e. g. for `OutputIsRedeem` error "diagnostic" field will be the following:
-- ```
-- {
--     "address" : <address>
-- }
-- ```
--
-- Additional data in constructor should be represented as record fields.
-- Otherwise TemplateHaskell will raise an error.
--
-- If constructor does not have additional data (like in case of `WalletNotFound` error),
-- then "diagnostic" field will be empty object.
--
-- TODO: change fields' types to actual Cardano core types, like `Coin` and `Address`
data WalletError =
    -- | NotEnoughMoney weNeedMore
      NotEnoughMoney !ErrNotEnoughMoney
    -- | OutputIsRedeem weAddress
    | OutputIsRedeem !(V1 Core.Address)
    -- | UnknownError weMsg
    | UnknownError !Text
    -- | InvalidAddressFormat weMsg
    | InvalidAddressFormat !Text
    | WalletNotFound
    | WalletAlreadyExists !WalletId
    | AddressNotFound
    | TxFailedToStabilize
    | InvalidPublicKey !Text
    | UnsignedTxCreationError
    | TooBigTransaction
    -- ^ Size of transaction (in bytes) is greater than maximum.
    | SignedTxSubmitError !Text
    | TxRedemptionDepleted
    -- | TxSafeSignerNotFound weAddress
    | TxSafeSignerNotFound !(V1 Core.Address)
    -- | MissingRequiredParams requiredParams
    | MissingRequiredParams !(NonEmpty (Text, Text))
    -- | WalletIsNotReadyToProcessPayments weStillRestoring
    | CannotCreateAddress !Text
    -- ^ Cannot create derivation path for new address (for external wallet).
    | WalletIsNotReadyToProcessPayments !SyncProgress
    -- ^ The @Wallet@ where a @Payment@ is being originated is not fully
    -- synced (its 'WalletSyncState' indicates it's either syncing or
    -- restoring) and thus cannot accept new @Payment@ requests.
    -- | NodeIsStillSyncing wenssStillSyncing
    | NodeIsStillSyncing !SyncPercentage
    -- ^ The backend couldn't process the incoming request as the underlying
    -- node is still syncing with the blockchain.
    | RequestThrottled !Word64
    -- ^ The request has been throttled. The 'Word64' is a count of microseconds
    -- until the user should retry.
    deriving (Generic, Show, Eq)

-- deriveWalletErrorJSON ''WalletError
deriveGeneric ''WalletError

instance Exception WalletError

instance ToHttpErrorStatus WalletError

instance ToJSON WalletError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON WalletError where
    parseJSON = jsendErrorGenericParseJSON

instance Arbitrary WalletError where
    arbitrary = Gen.oneof
        [ NotEnoughMoney <$> Gen.oneof
            [ pure ErrCannotCoverFee
            , ErrAvailableBalanceIsInsufficient <$> Gen.choose (1, 1000)
            ]
        , OutputIsRedeem . V1 <$> arbitrary
        , UnknownError <$> arbitraryText
        , InvalidAddressFormat <$> arbitraryText
        , pure WalletNotFound
        , WalletAlreadyExists <$> arbitrary
        , pure AddressNotFound
        , InvalidPublicKey <$> arbitraryText
        , pure UnsignedTxCreationError
        , SignedTxSubmitError <$> arbitraryText
        , pure TooBigTransaction
        , pure TxFailedToStabilize
        , pure TxRedemptionDepleted
        , TxSafeSignerNotFound . V1 <$> arbitrary
        , MissingRequiredParams <$> Gen.oneof
            [ unsafeMkNonEmpty <$> Gen.vectorOf 1 arbitraryParam
            , unsafeMkNonEmpty <$> Gen.vectorOf 2 arbitraryParam
            , unsafeMkNonEmpty <$> Gen.vectorOf 3 arbitraryParam
            ]
        , WalletIsNotReadyToProcessPayments <$> arbitrary
        , NodeIsStillSyncing <$> arbitrary
        , CannotCreateAddress <$> arbitraryText
        , RequestThrottled <$> arbitrary
        ]
      where
        arbitraryText :: Gen Text
        arbitraryText =
            toText . Gen.getASCIIString <$> arbitrary

        arbitraryParam :: Gen (Text, Text)
        arbitraryParam =
            (,) <$> arbitrary <*> arbitrary

        unsafeMkNonEmpty :: [a] -> NonEmpty a
        unsafeMkNonEmpty (h:q) = h :| q
        unsafeMkNonEmpty _     = error "unsafeMkNonEmpty called with empty list"


-- | Give a short description of an error
instance Buildable WalletError where
    build = \case
        NotEnoughMoney x ->
             bprint build x
        OutputIsRedeem _ ->
             bprint "One of the TX outputs is a redemption address."
        UnknownError _ ->
             bprint "Unexpected internal error."
        InvalidAddressFormat _ ->
             bprint "Provided address format is not valid."
        WalletNotFound ->
             bprint "Reference to an unexisting wallet was given."
        WalletAlreadyExists _ ->
             bprint "Can't create or restore a wallet. The wallet already exists."
        AddressNotFound ->
             bprint "Reference to an unexisting address was given."
        InvalidPublicKey _ ->
            bprint "Extended public key (for external wallet) is invalid."
        UnsignedTxCreationError ->
            bprint "Unable to create unsigned transaction for an external wallet."
        TooBigTransaction ->
            bprint "Transaction size is greater than 4096 bytes."
        SignedTxSubmitError _ ->
            bprint "Unable to submit externally-signed transaction."
        MissingRequiredParams _ ->
            bprint "Missing required parameters in the request payload."
        WalletIsNotReadyToProcessPayments _ ->
            bprint "This wallet is restoring, and it cannot send new transactions until restoration completes."
        NodeIsStillSyncing _ ->
            bprint "The node is still syncing with the blockchain, and cannot process the request yet."
        TxRedemptionDepleted ->
            bprint "The redemption address was already used."
        TxSafeSignerNotFound _ ->
            bprint "The safe signer at the specified address was not found."
        TxFailedToStabilize ->
            bprint "We were unable to find a set of inputs to satisfy this transaction."
        CannotCreateAddress _ ->
            bprint "Cannot create derivation path for new address, for external wallet."
        RequestThrottled _ ->
            bprint "You've made too many requests too soon, and this one was throttled."


-- | Convert wallet errors to Servant errors
instance ToServantError WalletError where
    declareServantError = \case
        NotEnoughMoney{} ->
            err403
        OutputIsRedeem{} ->
            err403
        UnknownError{} ->
            err500
        WalletNotFound{} ->
            err404
        WalletAlreadyExists{} ->
            err403
        InvalidAddressFormat{} ->
            err401
        AddressNotFound{} ->
            err404
        InvalidPublicKey{} ->
            err400
        UnsignedTxCreationError{} ->
            err500
        TooBigTransaction{} ->
            err400
        SignedTxSubmitError{} ->
            err500
        MissingRequiredParams{} ->
            err400
        WalletIsNotReadyToProcessPayments{} ->
            err403
        NodeIsStillSyncing{} ->
            err412 -- Precondition failed
        TxFailedToStabilize{} ->
            err500
        TxRedemptionDepleted{} ->
            err400
        TxSafeSignerNotFound{} ->
            err400
        CannotCreateAddress{} ->
            err500
        RequestThrottled{} ->
            err400 { errHTTPCode = 429 }

-- | Declare the key used to wrap the diagnostic payload, if any
instance HasDiagnostic WalletError where
    getDiagnosticKey = \case
        NotEnoughMoney{} ->
            "details"
        OutputIsRedeem{} ->
            "address"
        UnknownError{} ->
            "msg"
        WalletNotFound{} ->
            noDiagnosticKey
        WalletAlreadyExists{} ->
            "walletId"
        InvalidAddressFormat{} ->
            "msg"
        AddressNotFound{} ->
            noDiagnosticKey
        InvalidPublicKey{} ->
            "msg"
        UnsignedTxCreationError{} ->
            noDiagnosticKey
        TooBigTransaction{} ->
            noDiagnosticKey
        SignedTxSubmitError{} ->
            "msg"
        MissingRequiredParams{} ->
            "params"
        WalletIsNotReadyToProcessPayments{} ->
            "stillRestoring"
        NodeIsStillSyncing{} ->
            "stillSyncing"
        TxFailedToStabilize{} ->
            noDiagnosticKey
        TxRedemptionDepleted{} ->
            noDiagnosticKey
        TxSafeSignerNotFound{} ->
            "address"
        CannotCreateAddress{} ->
            "msg"
        RequestThrottled{} ->
            "microsecondsUntilRetry"
