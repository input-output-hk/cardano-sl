{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Node.API where

import           Universum

import           Control.Lens (At, Index, IxValue, at, ix, makePrisms, (?~))
import           Data.Aeson
import qualified Data.Aeson.Options as Aeson
import           Data.Aeson.TH as A
import           Data.Aeson.Types (toJSONKeyText)
import qualified Data.ByteArray as ByteArray
import qualified Data.Char as C
import qualified Data.Map.Strict as Map
import           Data.Swagger hiding (Example, Tag, example)
import qualified Data.Swagger as S
import           Data.Swagger.Declare (Declare, look)
import           Data.Swagger.Internal.Schema (GToSchema)
import           Data.Swagger.Internal.TypeShape (GenericHasSimpleShape,
                     GenericShape)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Version (Version, parseVersion, showVersion)
import           Formatting (bprint, build, shown, (%))
import qualified Formatting.Buildable
import           GHC.Generics (Generic, Rep)
import qualified Network.Transport as NT
import           Node (NodeId (..))
import qualified Prelude
import           Servant
import           Test.QuickCheck
import           Text.ParserCombinators.ReadP (readP_to_S)

import qualified Pos.Chain.Update as Core
import qualified Pos.Core as Core
import           Pos.Infra.Diffusion.Subscription.Status
                     (SubscriptionStatus (..))
import           Pos.Infra.Util.LogSafe (BuildableSafeGen (..), SecureLog (..),
                     deriveSafeBuildable)
import           Pos.Util.Example
import           Pos.Util.Servant (APIResponse, CustomQueryFlag, Flaggable (..),
                     HasCustomQueryFlagDescription (..), Tag,
                     TagDescription (..), ValidJSON)
import           Pos.Util.UnitsOfMeasure
import           Serokell.Util.Text

-- ToJSON/FromJSON instances for NodeId
import           Pos.Infra.Communication.Types.Protocol ()
import           Test.Pos.Core.Arbitrary ()



-- TODO: Stuff that was pulled upstream. Likely to be factored out into other modules.

type IsPropertiesMap m =
  (IxValue m ~ Referenced Schema, Index m ~ Text, At m, HasProperties Schema m)

genericSchemaDroppingPrefix
    :: forall a m.
    ( Generic a, ToJSON a, Example a, GToSchema (Rep a), IsPropertiesMap m
    , GenericHasSimpleShape
        a
        "genericDeclareNamedSchemaUnrestricted"
        (GenericShape (Rep a))
    )
    => String -- ^ Prefix to drop on each constructor tag
    -> ((Index m -> Text -> m -> m) -> m -> m) -- ^ Callback update to attach descriptions to underlying properties
    -> Proxy a -- ^ Underlying data-type proxy
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


--
-- Helpers for writing instances for types with units
--

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

forceNtpCheckDescription :: T.Text
forceNtpCheckDescription =
    "In some cases, API Clients need to force a new NTP check as a previous result gets cached. A typical use-case is after asking a user to fix its system clock. If this flag is set, request will block until NTP server responds or it will timout if NTP server is not available within **30** seconds."


-- | The different between the local time and the remote NTP server.
newtype LocalTimeDifference =
    LocalTimeDifference (MeasuredIn 'Microseconds Integer)
    deriving stock (Show, Eq)
    deriving newtype (ToJSON, FromJSON, BuildableSafeGen)
deriveSafeBuildable ''LocalTimeDifference

mkLocalTimeDifference :: Integer -> LocalTimeDifference
mkLocalTimeDifference = LocalTimeDifference . MeasuredIn

instance Arbitrary LocalTimeDifference where
    arbitrary = mkLocalTimeDifference <$> arbitrary

instance ToSchema LocalTimeDifference where
    declareNamedSchema _ = do
        NamedSchema _ s <- declareNamedSchema $ Proxy @(MeasuredIn 'Microseconds Integer)
        pure $ NamedSchema (Just "LocalTimeDifference") s

data TimeInfo
    = TimeInfoUnavailable
    | TimeInfoPending
    | TimeInfoAvailable { localTimeDifference :: LocalTimeDifference }
    deriving (Eq, Show, Generic)

instance ToSchema TimeInfo where
    declareNamedSchema _ = do
        NamedSchema _ localTimeSchema <- declareNamedSchema (Proxy @LocalTimeDifference)
        pure $ NamedSchema (Just "TimeInfo") $ mempty
            & type_ ?~ SwaggerObject
            & description .~ Just
                "When 'available', returns an extra 'localTimeDifference' about \
                \the difference in microseconds between the node time and the \
                \NTP server."
            & required .~ ["status"]
            & properties .~ (mempty
                & at "status" ?~ Inline (mempty
                    & type_ ?~ SwaggerString
                    & enum_ ?~ [String "unavailable", String "pending", String "available"]
                )
                & at "localTimeDifference" ?~ Inline localTimeSchema
            )

instance Arbitrary TimeInfo where
    arbitrary = oneof
        [ pure TimeInfoUnavailable
        , pure TimeInfoPending
        , TimeInfoAvailable <$> arbitrary
        ]

instance Example TimeInfo

deriveSafeBuildable ''TimeInfo
instance BuildableSafeGen TimeInfo where
    buildSafeGen _ = \case
        TimeInfoUnavailable -> "unavailable"
        TimeInfoPending -> "pending"
        TimeInfoAvailable localTimeDifference -> bprint ("{"
            %" differenceFromNtpServer="%build
            %" }")
            localTimeDifference

deriveJSON (A.defaultOptions
    { A.constructorTagModifier = (\(h:q) -> C.toLower h : q) . drop 8
    , A.sumEncoding            = A.TaggedObject "status" "_"
    }) ''TimeInfo

data Percent -- No concrete type needed, this is just for the Swagger schema

instance ToSchema Percent where
    declareNamedSchema _ = do
        NamedSchema _ s <- declareNamedSchema $ Proxy @Word
        pure $ NamedSchema (Just "Percent") $ s
            & minimum_ ?~ 0
            & maximum_ ?~ 100

newtype SyncPercentage
    = SyncPercentage (MeasuredIn 'Percentage100 Word8)
    deriving stock (Show, Eq, Ord)
    deriving newtype (ToJSON, FromJSON, BuildableSafeGen)
deriveSafeBuildable ''SyncPercentage

instance ToSchema SyncPercentage where
    declareNamedSchema _ = do
        NamedSchema _ s <- declareNamedSchema $ Proxy @(MeasuredIn 'Percentage100 Percent)
        pure $ NamedSchema (Just "SyncPercentage") s

instance Arbitrary SyncPercentage where
    arbitrary = mkSyncPercentage <$> choose (0, 100)

instance Example SyncPercentage where
    example = pure (SyncPercentage (MeasuredIn 14))

mkSyncPercentage :: Word8 -> SyncPercentage
mkSyncPercentage = SyncPercentage . MeasuredIn


-- | The absolute or relative height of the blockchain, measured in number
-- of blocks.
newtype BlockchainHeight =
    BlockchainHeight (MeasuredIn 'Blocks Word64)
    deriving stock (Show, Eq)
    deriving newtype (ToJSON, FromJSON, BuildableSafeGen)
deriveSafeBuildable ''BlockchainHeight

mkBlockchainHeight :: Core.BlockCount -> BlockchainHeight
mkBlockchainHeight (Core.BlockCount n) = BlockchainHeight $ MeasuredIn n

instance Arbitrary BlockchainHeight where
    arbitrary =
        mkBlockchainHeight . Core.BlockCount <$> choose (minBound, maxBound)

instance Example BlockchainHeight

instance ToSchema BlockchainHeight where
    declareNamedSchema _ = do
        NamedSchema _ s <- declareNamedSchema $ Proxy @(MeasuredIn 'Blocks Word64)
        pure $ NamedSchema (Just "BlockchainHeight") s



-- | The @dynamic@ information for this node.
data NodeInfo = NodeInfo {
     nfoSyncProgress          :: !SyncPercentage
   , nfoBlockchainHeight      :: !(Maybe BlockchainHeight)
   , nfoLocalBlockchainHeight :: !BlockchainHeight
   , nfoLocalTimeInformation  :: !TimeInfo
   , nfoSubscriptionStatus    :: Map NodeId SubscriptionStatus
   } deriving (Show, Eq, Generic)

deriveJSON Aeson.defaultOptions ''NodeInfo

instance ToSchema NodeInfo where
    declareNamedSchema =
        genericSchemaDroppingPrefix "nfo" (\(--^) props -> props
            & ("syncProgress"
                --^ "Syncing progression, in percentage.")
            & ("blockchainHeight"
                --^ "If known, the current blockchain height, in number of blocks.")
            & ("localBlockchainHeight"
                --^ "Local blockchain height, in number of blocks.")
            & ("localTimeInformation"
                --^ "Information about the clock on this node. The field \
                \'localTimeDifference' is only defined when the status is 'available'")
            & ("subscriptionStatus"
                --^ "Is the node connected to the network?")
        )

instance Arbitrary NodeInfo where
    arbitrary =
        NodeInfo
            <$> arbitrary
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

instance Example NodeInfo where
    example = NodeInfo <$> example
                       <*> example  -- NOTE: will produce `Just a`
                       <*> example
                       <*> example
                       <*> example

-- | The sync progress with the blockchain.

availableSubscriptionStatus :: [SubscriptionStatus]
availableSubscriptionStatus = [Subscribed, Subscribing]

deriveSafeBuildable ''SubscriptionStatus
instance BuildableSafeGen SubscriptionStatus where
    buildSafeGen _ = \case
        Subscribed  -> "Subscribed"
        Subscribing -> "Subscribing"

deriveJSON Aeson.defaultOptions ''SubscriptionStatus

instance Arbitrary SubscriptionStatus where
    arbitrary =
        elements availableSubscriptionStatus

instance Example SubscriptionStatus

instance ToSchema SubscriptionStatus where
    declareNamedSchema _ = do
        let enum = toJSON <$> availableSubscriptionStatus
        pure $ NamedSchema (Just "SubscriptionStatus") $ mempty
            & type_ ?~ SwaggerString
            & enum_ ?~ enum

instance Arbitrary NodeId where
    arbitrary = do
        ipv4  <- genIPv4
        port_ <- genPort
        idx   <- genIdx
        return . toNodeId $ ipv4 <> ":" <> port_ <> ":" <> idx
      where
        toNodeId = NodeId . NT.EndPointAddress . T.encodeUtf8
        showT    = T.pack . show :: Int -> Text
        genIdx   = showT <$> choose (0, 9)
        genPort  = showT <$> choose (1000, 8000)
        genIPv4  = T.intercalate "." <$> replicateM 4 (showT <$> choose (0, 255))

instance Example NodeId

instance FromJSONKey NodeId where
    fromJSONKey =
        FromJSONKeyText (NodeId . NT.EndPointAddress . encodeUtf8)

instance ToJSONKey NodeId where
    toJSONKey =
        toJSONKeyText (decodeUtf8 . getAddress)
      where
        getAddress (NodeId (NT.EndPointAddress x)) = x

instance ToSchema NodeId where
    declareNamedSchema _ = pure $ NamedSchema (Just "NodeId") $ mempty
        & type_ ?~ SwaggerString

instance (Buildable a, Buildable b) => Buildable (a, b) where
    build (a, b) = bprint ("("%build%", "%build%")") a b

-- Types for the NodeSettings API. TODO: Extract to own module.

-- | How many milliseconds a slot lasts for.
newtype SlotDuration = SlotDuration (MeasuredIn 'Milliseconds Word)
    deriving stock (Show, Eq)
    deriving newtype (ToJSON, FromJSON, BuildableSafeGen)
deriveSafeBuildable ''SlotDuration

instance ToSchema SlotDuration where
    declareNamedSchema _ = do
        NamedSchema _ s <- declareNamedSchema $ Proxy @(MeasuredIn 'Milliseconds Word)
        pure $ NamedSchema (Just "SlotDuration") s

instance Arbitrary SlotDuration where
    arbitrary = mkSlotDuration <$> choose (0, 100)

mkSlotDuration :: Word -> SlotDuration
mkSlotDuration = SlotDuration . MeasuredIn


newtype MaxTxSize = MaxTxSize (MeasuredIn 'Bytes Word)
    deriving stock (Show, Eq)
    deriving newtype (ToJSON, FromJSON, BuildableSafeGen)
deriveSafeBuildable ''MaxTxSize

instance ToSchema MaxTxSize where
    declareNamedSchema _ = do
        NamedSchema _ s <- declareNamedSchema $ Proxy @(MeasuredIn 'Bytes Word)
        pure $ NamedSchema (Just "MaxTxSize") s

instance Arbitrary MaxTxSize where
    arbitrary = mkMaxTxSize <$> arbitrary

mkMaxTxSize :: Word -> MaxTxSize
mkMaxTxSize = MaxTxSize . MeasuredIn


-- | This deceptively-simple newtype is a wrapper to virtually @all@ the types exposed as
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

instance Buildable (SecureLog a) => Buildable (SecureLog (V1 a)) where
    build (SecureLog (V1 x)) = bprint build (SecureLog x)

--
-- Benign instances
--

instance ByteArray.ByteArrayAccess a => ByteArray.ByteArrayAccess (V1 a) where
   length (V1 a) = ByteArray.length a
   withByteArray (V1 a) = ByteArray.withByteArray a

instance Enum a => Enum (V1 a) where
    toEnum x = V1 (toEnum x)
    fromEnum (V1 a) = fromEnum a

instance Bounded a => Bounded (V1 a) where
    minBound = V1 $ minBound @a
    maxBound = V1 $ maxBound @a

instance Show a => Show (V1 a) where
    show (V1 a) = Prelude.show a


instance ToJSON (V1 Core.ApplicationName) where
    toJSON (V1 svAppName) = toJSON (Core.getApplicationName svAppName)

instance FromJSON (V1 Core.ApplicationName) where
    parseJSON = withText "ApplicationName" $ \svAppName ->
        pure (V1 (Core.ApplicationName svAppName))

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
            & type_ ?~ SwaggerObject
            & properties .~ (mempty
                & at "applicationName" ?~ Inline (toSchema (Proxy @Text))
                & at "version" ?~ Inline (toSchema (Proxy @Word32))
            )
            & required .~ ["applicationName", "version"]

instance Arbitrary (V1 Core.SoftwareVersion) where
    arbitrary = fmap V1 arbitrary

instance Buildable Version where
    build v = bprint shown v

instance ToJSON (V1 Version) where
    toJSON (V1 v) = toJSON (showVersion v)

instance FromJSON (V1 Version) where
    parseJSON = withText "V1Version" $ \v ->
        case readP_to_S parseVersion (T.unpack v) of
            (reverse -> ((ver,_):_)) -> pure (V1 ver)
            _                        -> mempty

instance Arbitrary (V1 Version) where
    arbitrary = fmap V1 arbitrary

instance {-# OVERLAPPABLE #-} Buildable a => Buildable (V1 a) where
    build (V1 x) = bprint build x

#if !(MIN_VERSION_swagger2(2,2,2))
-- See note [Version Orphan]
instance ToSchema Version where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "Version") $ mempty
            & type_ .~ SwaggerString

-- Note [Version Orphan]
-- I have opened a PR to add an instance of 'Version' to the swagger2
-- library. When the PR is merged, we can delete the instance here and remove the warning from the file.
-- PR: https://github.com/GetShopTV/swagger2/pull/152
#endif

instance ToSchema (V1 Version) where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "V1Version") $ mempty
            & type_ ?~ SwaggerString


newtype SecurityParameter = SecurityParameter Int
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Arbitrary SecurityParameter where
    arbitrary = SecurityParameter . abs <$> arbitrary

instance Buildable SecurityParameter where
    build (SecurityParameter i) = bprint shown i

instance ToSchema SecurityParameter where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "SecurityParameter") $ mempty
            & type_ ?~ SwaggerNumber
            & minimum_ .~ (Just 0)


instance ToSchema (V1 Core.SlotId) where
    declareNamedSchema _ = do
        word64Schema <- declareSchemaRef (Proxy @Word64)
        word16Schema <- declareSchemaRef (Proxy @Word16)
        return $ NamedSchema (Just "SlotId") $ mempty
            & type_ ?~ SwaggerObject
            & properties .~ (mempty
                & at "slot" ?~ word16Schema
                & at "epoch" ?~ word64Schema)

instance ToJSON (V1 Core.SlotId) where
    toJSON (V1 s) =
        object
            [ "epoch" .= toJSON (Core.getEpochIndex $ Core.siEpoch s)
            , "slot"  .= toJSON (Core.getSlotIndex  $ Core.siSlot s)
            ]

instance FromJSON (V1 Core.SlotId) where
    parseJSON = withObject "SlotId" $ \sl ->
        Core.SlotId
            <$> (fromInteger <$> sl .: "epoch")
            <*> (Core.UnsafeLocalSlotIndex <$> sl .: "slot")
            <&> V1

instance Arbitrary (V1 Core.SlotId) where
    arbitrary = fmap V1 arbitrary

data TxFeePolicy = TxFeePolicy
    { tfpA :: MeasuredIn 'LovelacePerByte Core.Coeff
    , tfpB :: MeasuredIn 'Lovelace Core.Coeff
    } deriving (Eq, Ord, Show, Generic)

fromCorePolicy :: Core.TxSizeLinear -> TxFeePolicy
fromCorePolicy (Core.TxSizeLinear a b) =
    TxFeePolicy (MeasuredIn a) (MeasuredIn b)

deriveJSON Aeson.defaultOptions ''TxFeePolicy

instance Arbitrary TxFeePolicy where
    arbitrary = fmap fromCorePolicy' (arbitrary `suchThat` predicate)
      where
        fromCorePolicy' = \case
            Core.TxFeePolicyTxSizeLinear a -> fromCorePolicy a
            _ -> error "Arbitrary TxFeePolicy: Just generated a non-linear fee policy?"
        predicate (Core.TxFeePolicyTxSizeLinear  _) = True
        predicate (Core.TxFeePolicyUnknown _ _)     = False

instance Example TxFeePolicy

instance ToSchema TxFeePolicy where
    declareNamedSchema _ = do
        -- NOTE
        -- Not using 'genericSchemaDroppingPrefix' here because there
        -- seems to be a bug where the base shema of 'b' would override the base
        -- schema of 'a'; So, having the right descriptions for the wrong unit
        -- for 'a'. It's kinda weird and I don't have time to look further to
        -- fix this ATM ¯\_(ツ)_/¯
        NamedSchema _ a <- declareNamedSchema $ Proxy @(MeasuredIn 'LovelacePerByte Double)
        NamedSchema _ b <- declareNamedSchema $ Proxy @(MeasuredIn 'Lovelace Double)
        pure $ NamedSchema (Just "TxFeePolicy") $ mempty
            & type_ ?~ SwaggerObject
            & required .~ ["a", "b"]
            & properties .~ (mempty
                & at "a" ?~ Inline (a & description ?~ "Slope of the linear curve")
                & at "b" ?~ Inline (b & description ?~ "Intercept of the linear curve")
                )

deriveSafeBuildable ''TxFeePolicy
instance BuildableSafeGen TxFeePolicy where
    buildSafeGen _ (TxFeePolicy a b) = bprint
        ( "( a = " % build % ", b = " % build % " )")
        a
        b

instance Arbitrary (V1 Core.SlotCount) where
    arbitrary = fmap V1 arbitrary

instance ToSchema (V1 Core.SlotCount) where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "V1Core.SlotCount") $ mempty
            & type_ ?~ SwaggerNumber
            & minimum_ .~ Just 0

instance ToJSON (V1 Core.SlotCount) where
    toJSON (V1 (Core.SlotCount c)) = toJSON c

instance FromJSON (V1 Core.SlotCount) where
    parseJSON v = V1 . Core.SlotCount <$> parseJSON v



-- | The @static@ settings for this wallet node. In particular, we could group
-- here protocol-related settings like the slot duration, the transaction max size,
-- the current software version running on the node, etc.
data NodeSettings = NodeSettings
    { setSlotId            :: !(V1 Core.SlotId)
    , setSlotDuration      :: !SlotDuration
    , setSlotCount         :: !(V1 Core.SlotCount)
    , setSoftwareInfo      :: !(V1 Core.SoftwareVersion)
    , setProjectVersion    :: !(V1 Version)
    , setGitRevision       :: !Text
    , setMaxTxSize         :: !MaxTxSize
    , setFeePolicy         :: !TxFeePolicy
    , setSecurityParameter :: !SecurityParameter
    } deriving (Show, Eq, Generic)

deriveJSON Aeson.defaultOptions ''NodeSettings

instance ToSchema NodeSettings where
  declareNamedSchema =
    genericSchemaDroppingPrefix "set" (\(--^) props -> props
      & ("slotId"             --^ "The current slot and epoch.")
      & ("slotDuration"       --^ "Duration of a slot.")
      & ("slotCount"          --^ "The number of slots per epoch.")
      & ("softwareInfo"       --^ "Various pieces of information about the current software.")
      & ("projectVersion"     --^ "Current project's version.")
      & ("gitRevision"        --^ "Git revision of this deployment.")
      & ("maxTxSize"          --^ "The largest allowed transaction size")
      & ("feePolicy"          --^ "The fee policy.")
      & ("securityParameter"  --^ "The security parameter.")
    )

instance Arbitrary NodeSettings where
    arbitrary = NodeSettings <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> pure "0e1c9322a"
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary

instance Example NodeSettings

deriveSafeBuildable ''NodeSettings
instance BuildableSafeGen NodeSettings where
    buildSafeGen _ NodeSettings{..} = bprint ("{"
        %" slotId="%build
        %" slotDuration="%build
        %" slotCount="%build
        %" softwareInfo="%build
        %" projectRevision="%build
        %" gitRevision="%build
        %" maxTxSize="%build
        %" feePolicy="%build
        %" securityParameter="%build
        %" }")
        setSlotId
        setSlotDuration
        setSlotCount
        setSoftwareInfo
        setProjectVersion
        setGitRevision
        setMaxTxSize
        setFeePolicy
        setSecurityParameter



type SettingsAPI =
    Tag "Settings" 'NoTagDescription
        :> "node-settings"
        :> Summary "Retrieves the static settings for this node."
        :> Get '[ValidJSON] (APIResponse NodeSettings)

type InfoAPI =
        Tag "Info" 'NoTagDescription
            :> "node-info"
            :> Summary "Retrieves the dynamic information for this node."
            :> CustomQueryFlag "force_ntp_check" ForceNtpCheck
            :> Get '[ValidJSON] (APIResponse NodeInfo)

instance HasCustomQueryFlagDescription "force_ntp_check" where
    customDescription _ = Just forceNtpCheckDescription

-- The API definition is down here for now due to TH staging restrictions. Will
-- relocate other stuff into it's own module when the extraction is complete.
type API =
        SettingsAPI
    :<|>
        InfoAPI
    :<|>
        Summary "Version of the next update (404 if none)"
        :> "next-update"
        :> Get '[ValidJSON] (APIResponse (V1 Core.SoftwareVersion))
    :<|>
        Summary "Restart the underlying node software."
        :> "restart-node"
        :> Post '[ValidJSON] NoContent



