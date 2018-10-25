{-# LANGUAGE TypeOperators, DataKinds, RecordWildCards #-}

module Pos.Node.API where

import Universum

import qualified Data.Map.Strict as Map
import qualified Data.Char as C
import Formatting (bprint, build, (%))
import qualified Pos.Core as Core
import           Control.Lens (At, Index, IxValue, at, ix, makePrisms, to, (?~))
import           Pos.Infra.Util.LogSafe (BuildableSafeGen (..), SecureLog (..),
                     buildSafe, buildSafeList, buildSafeMaybe,
                     deriveSafeBuildable, plainOrSecureF)
import           Pos.Infra.Diffusion.Subscription.Status
                     (SubscriptionStatus (..))
import           Pos.Util.Servant (CustomQueryFlag, ValidJSON, Flaggable(..))
import           Node (NodeId (..))
import           Test.QuickCheck
import           Data.Aeson
import qualified Data.Aeson.Options as Serokell
import           Data.Aeson.TH as A
import           Data.Aeson.Types (Parser, Value (..), toJSONKeyText,
                     typeMismatch)
import           Data.Swagger hiding (Example, example)
import qualified Data.Swagger as S
import           Data.Swagger.Declare (Declare, look)
import           Data.Swagger.Internal.Schema (GToSchema)
import           Data.Swagger.Internal.TypeShape (GenericHasSimpleShape,
                     GenericShape)
import           GHC.Generics (Generic, Rep)
import Pos.Util.UnitsOfMeasure

import           Servant

type IsPropertiesMap m =
  (IxValue m ~ Referenced Schema, Index m ~ Text, At m, HasProperties Schema m)

genericSchemaDroppingPrefix
    :: forall a m proxy.
    ( Generic a, ToJSON a, GToSchema (Rep a), IsPropertiesMap m
    , GenericHasSimpleShape
        a
        "genericDeclareNamedSchemaUnrestricted"
        (GenericShape (Rep a))
    )
    => a -- ^ An example of the type.
    -> String -- ^ Prefix to drop on each constructor tag
    -> ((Index m -> Text -> m -> m) -> m -> m) -- ^ Callback update to attach descriptions to underlying properties
    -> proxy a -- ^ Underlying data-type proxy
    -> Declare (Definitions Schema) NamedSchema
genericSchemaDroppingPrefix example prfx extraDoc proxy = do
    let opts = defaultSchemaOptions
          { S.fieldLabelModifier = over (ix 0) C.toLower . drop (length prfx) }
    s <- genericDeclareNamedSchema opts proxy
    defs <- look
    pure $ s
      & over schema (over properties (extraDoc (addFieldDescription defs)))
      & schema . S.example ?~ toJSON example
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
            & ("syncProgress"
                --^ "Syncing progression, in percentage.")
            & ("blockchainHeight"
                --^ "If known, the current blockchain height, in number of blocks.")
            & ("localBlockchainHeight"
                --^ "Local blockchain height, in number of blocks.")
            & ("localTimeInformation"
                --^ "Information about the clock on this node.")
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

-- | The sync progress with the blockchain.

type API = Tags '["Info"] :>
         (    "node-info" :> Summary "Retrieves the dynamic information for this node."
                          :> CustomQueryFlag "force_ntp_check" ForceNtpCheck
                          :> Get '[ValidJSON] (WalletResponse NodeInfo)
         )
