{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}

-- The hlint parser fails on the `pattern` function, so we disable the
-- language extension here.
{-# LANGUAGE NoPatternSynonyms          #-}

-- See note [Version orphan]
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
  , accountsHaveSameId
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
  , TransactionStatus(..)
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
  -- * Some types for the API
  , CaptureWalletId
  , CaptureAccountId
  -- * Core re-exports
  , Core.Address
  ) where

import           Universum

import           Control.Lens (At, Index, IxValue, at, ix, makePrisms, to, (?~))
import           Data.Aeson
import           Data.Aeson.TH as A
import           Data.Aeson.Types (toJSONKeyText, typeMismatch)
import qualified Data.Char as C
import           Data.Swagger as S
import           Data.Swagger.Declare (Declare, look)
import           Data.Swagger.Internal.Schema (GToSchema)
import           Data.Swagger.Internal.TypeShape (GenericHasSimpleShape, GenericShape)
import           Data.Text (Text, dropEnd, toLower)
import qualified Data.Text as T
import qualified Data.Text.Buildable
import           Data.Version (Version)
import           Formatting (bprint, build, fconst, int, sformat, (%))
import           GHC.Generics (Generic, Rep)
import           Network.Transport (EndPointAddress (..))
import           Node (NodeId (..))
import qualified Prelude
import qualified Serokell.Aeson.Options as Serokell
import           Serokell.Util (listJson)
import qualified Serokell.Util.Base16 as Base16
import           Servant
import           Test.QuickCheck
import           Test.QuickCheck.Gen (Gen (..))
import           Test.QuickCheck.Random (mkQCGen)

import           Cardano.Wallet.API.Types.UnitOfMeasure (MeasuredIn (..), UnitOfMeasure (..))
import           Cardano.Wallet.Orphans.Aeson ()

-- V0 logic
import           Pos.Util.BackupPhrase (BackupPhrase (..))

-- importing for orphan instances for Coin
import           Pos.Wallet.Web.ClientTypes.Instances ()

import           Cardano.Wallet.Util (showApiUtcTime)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Pos.Aeson.Core ()
import           Pos.Arbitrary.Core ()
import qualified Pos.Client.Txp.Util as Core
import           Pos.Core (addressF)
import qualified Pos.Core as Core
import           Pos.Crypto (decodeHash, hashHexF)
import qualified Pos.Crypto.Signing as Core
import           Pos.Diffusion.Types (SubscriptionStatus (..))
import           Pos.Util.LogSafe (BuildableSafeGen (..), SecureLog (..), buildSafe, buildSafeList,
                                   buildSafeMaybe, deriveSafeBuildable, plainOrSecureF)
import qualified Pos.Wallet.Web.State.Storage as OldStorage



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
    ( Generic a, ToJSON a, Arbitrary a, GToSchema (Rep a), IsPropertiesMap m
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
      & schema . example ?~ toJSON (genExample :: a)
  where
    genExample =
      (unGen (resize 3 arbitrary)) (mkQCGen 42) 42

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

instance Buildable a => Buildable (V1 a) where
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
   withByteArray (V1 a) callback = ByteArray.withByteArray a callback

-- TODO(adinapoli) Rewrite it properly under CSL-2048.
instance Arbitrary (V1 BackupPhrase) where
    arbitrary = V1 <$> arbitrary

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

instance Monoid (V1 Core.PassPhrase) where
    mempty = V1 mempty
    mappend (V1 a) (V1 b) = V1 (a `mappend` b)

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
    declareNamedSchema _ = do
        pure $ NamedSchema (Just "AssuranceLevel") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["normal", "strict"]

deriveSafeBuildable ''AssuranceLevel
instance BuildableSafeGen AssuranceLevel where
    buildSafeGen _ NormalAssurance = "normal"
    buildSafeGen _ StrictAssurance = "strict"

-- | A Wallet ID.
newtype WalletId = WalletId Text deriving (Show, Eq, Ord, Generic)

deriveJSON Serokell.defaultOptions ''WalletId

instance ToSchema WalletId where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance Arbitrary WalletId where
  arbitrary =
      let wid = "J7rQqaLLHBFPrgJXwpktaMB1B1kQBXAyc2uRSfRPzNVGiv6TdxBzkPNBUWysZZZdhFG9gRy3sQFfX5wfpLbi4XTFGFxTg"
          in WalletId <$> elements [wid]

deriveSafeBuildable ''WalletId
instance BuildableSafeGen WalletId where
    buildSafeGen sl (WalletId wid) =
        bprint (plainOrSecureF sl build (fconst "<wallet id>")) wid

instance FromHttpApiData WalletId where
    parseQueryParam = Right . WalletId

instance ToHttpApiData WalletId where
    toQueryParam (WalletId wid) = wid


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
    declareNamedSchema _ = do
        pure $ NamedSchema (Just "WalletOperation") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["create", "restore"]

deriveSafeBuildable ''WalletOperation
instance BuildableSafeGen WalletOperation where
    buildSafeGen _ CreateWallet  = "create"
    buildSafeGen _ RestoreWallet = "restore"


-- | A type modelling the request for a new 'Wallet'.
data NewWallet = NewWallet {
      newwalBackupPhrase     :: !(V1 BackupPhrase)
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
      & ("spendingPassword" --^ "Optional spending password to encrypt / decrypt private keys.")
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

instance ToJSON SyncPercentage where
    toJSON (SyncPercentage (MeasuredIn w)) =
        object [ "quantity" .= toJSON w
               , "unit"     .= String "percent"
               ]

instance FromJSON SyncPercentage where
    parseJSON = withObject "SyncPercentage" $ \sl -> mkSyncPercentage <$> sl .: "quantity"

instance ToSchema SyncPercentage where
    declareNamedSchema _ = do
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

instance ToJSON EstimatedCompletionTime where
    toJSON (EstimatedCompletionTime (MeasuredIn w)) =
        object [ "quantity" .= toJSON w
               , "unit"     .= String "milliseconds"
               ]

instance FromJSON EstimatedCompletionTime where
    parseJSON = withObject "EstimatedCompletionTime" $ \sl -> mkEstimatedCompletionTime <$> sl .: "quantity"

instance ToSchema EstimatedCompletionTime where
    declareNamedSchema _ = do
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


newtype SyncThroughput = SyncThroughput (MeasuredIn 'BlocksPerSecond OldStorage.SyncThroughput)
  deriving (Show, Eq)

mkSyncThroughput :: Core.BlockCount -> SyncThroughput
mkSyncThroughput = SyncThroughput . MeasuredIn . OldStorage.SyncThroughput

instance Ord SyncThroughput where
    compare (SyncThroughput (MeasuredIn (OldStorage.SyncThroughput (Core.BlockCount b1))))
            (SyncThroughput (MeasuredIn (OldStorage.SyncThroughput (Core.BlockCount b2)))) =
        compare b1 b2

instance Arbitrary SyncThroughput where
    arbitrary = SyncThroughput . MeasuredIn . OldStorage.SyncThroughput <$> arbitrary

instance ToJSON SyncThroughput where
    toJSON (SyncThroughput (MeasuredIn (OldStorage.SyncThroughput (Core.BlockCount blocks)))) =
      object [ "quantity" .= toJSON blocks
             , "unit"     .= String "blocksPerSecond"
             ]

instance FromJSON SyncThroughput where
    parseJSON = withObject "SyncThroughput" $ \sl -> mkSyncThroughput . Core.BlockCount <$> sl .: "quantity"

instance ToSchema SyncThroughput where
    declareNamedSchema _ = do
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
    buildSafeGen _ sp = bprint build sp

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
    } deriving (Eq, Ord, Show, Generic)

deriveJSON Serokell.defaultOptions ''Wallet

instance ToSchema Wallet where
    declareNamedSchema =
        genericSchemaDroppingPrefix "wal" (\(--^) props -> props
            & "id"
            --^ "Unique wallet identifier."
            & "name"
            --^ "Wallet's name."
            & "balance"
            --^ "Current balance, in ADA."
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

deriveSafeBuildable ''Wallet
instance BuildableSafeGen Wallet where
  buildSafeGen sl Wallet{..} = bprint ("{"
    %" id="%buildSafe sl
    %" name="%buildSafe sl
    %" balance="%buildSafe sl
    %" }")
    walId
    walName
    walBalance

instance Buildable [Wallet] where
    build = bprint listJson

--------------------------------------------------------------------------------
-- Addresses
--------------------------------------------------------------------------------

-- | Whether an address is valid or not.
newtype AddressValidity = AddressValidity { isValid :: Bool }
  deriving (Eq, Show, Generic)

deriveJSON Serokell.defaultOptions ''AddressValidity

instance ToSchema AddressValidity where
    declareNamedSchema = genericSchemaDroppingPrefix "is" (\_ -> identity)

instance Arbitrary AddressValidity where
  arbitrary = AddressValidity <$> arbitrary

deriveSafeBuildable ''AddressValidity
instance BuildableSafeGen AddressValidity where
    buildSafeGen _ AddressValidity{..} =
        bprint ("{ valid="%build%" }") isValid

--------------------------------------------------------------------------------
-- Accounts
--------------------------------------------------------------------------------

-- | Summary about single address.
data WalletAddress = WalletAddress
    { addrId            :: !(V1 Core.Address)
    , addrUsed          :: !Bool
    , addrChangeAddress :: !Bool
    } deriving (Show, Eq, Generic, Ord)

deriveJSON Serokell.defaultOptions ''WalletAddress

instance ToSchema WalletAddress where
    declareNamedSchema =
        genericSchemaDroppingPrefix "addr" (\(--^) props -> props
            & ("id"            --^ "Actual address.")
            & ("used"          --^ "True if this address has been used.")
            & ("changeAddress" --^ "True if this address stores change from a previous transaction.")
        )

instance Arbitrary WalletAddress where
    arbitrary = WalletAddress <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary

type AccountIndex = Word32

-- | A wallet 'Account'.
data Account = Account
    { accIndex     :: !AccountIndex
    , accAddresses :: ![WalletAddress]
    , accAmount    :: !(V1 Core.Coin)
    , accName      :: !Text
    , accWalletId  :: !WalletId
    } deriving (Show, Ord, Eq, Generic)

accountsHaveSameId :: Account -> Account -> Bool
accountsHaveSameId a b =
    accWalletId a == accWalletId b
    &&
    accIndex a == accIndex b

deriveJSON Serokell.defaultOptions ''Account

instance ToSchema Account where
    declareNamedSchema =
        genericSchemaDroppingPrefix "acc" (\(--^) props -> props
            & ("index"     --^ "Account's index in the wallet, starting at 0.")
            & ("addresses" --^ "Public addresses pointing to this account.")
            & ("amount"    --^ "Available funds, in ADA.")
            & ("name"      --^ "Account's name.")
            & ("walletId"  --^ "Id of the wallet this account belongs to.")
          )

instance Arbitrary Account where
    arbitrary = Account <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> pure "My account"
                        <*> arbitrary

deriveSafeBuildable ''Account
instance BuildableSafeGen Account where
    buildSafeGen sl Account{..} = bprint ("{"
        %" index="%buildSafe sl
        %" name="%buildSafe sl
        %" addresses="%buildSafeList sl
        %" amount="%buildSafe sl
        %" walletId="%buildSafe sl
        %" }")
        accIndex
        accName
        accAddresses
        accAmount
        accWalletId

instance Buildable [Account] where
    build = bprint listJson

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
      & ("spendingPassword" --^ "Optional spending password to unlock funds.")
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
      & ("spendingPassword" --^ "Optional spending password to unlock funds.")
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
      & ("estimatedAmount" --^ "Estimated fees, in ADA.")
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
      & ("amount"  --^ "Amount of coin to bind, in ADA.")
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
      & ("spendingPassword" --^ "Optional spending password to access funds.")
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
instance Arbitrary (V1 Core.TxId) where
  arbitrary = V1 <$> arbitrary

instance ToJSON (V1 Core.TxId) where
  toJSON (V1 t) = String (sformat hashHexF t)

instance FromJSON (V1 Core.TxId) where
    parseJSON = withText "TxId" $ \t -> do
       case decodeHash t of
           Left err -> fail $ "Failed to parse transaction ID: " <> toString err
           Right a  -> pure (V1 a)

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
  { txId            :: !(V1 Core.TxId)
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
      & ("amount"        --^ "Coins moved as part of the transaction, in ADA.")
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
   , setProjectVersion :: !Version
   , setGitRevision    :: !Text
   } deriving (Show, Eq, Generic)

-- See note [Version Orphan]
instance ToSchema Version where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "Version") $ mempty
            & type_ .~ SwaggerString

-- Note [Version Orphan]
-- I have opened a PR to add an instance of 'Version' to the swagger2
-- library. When the PR is merged, we can delete the instance here and remove the warning from the file.
-- PR: https://github.com/GetShopTV/swagger2/pull/152

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
          <> "server. This value will be null if the NTP server is pending or "
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

type CaptureWalletId = Capture "walletId" WalletId

type CaptureAccountId = Capture "accountId" AccountIndex
