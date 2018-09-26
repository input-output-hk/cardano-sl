{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Pos.Chain.Genesis.Config
       ( StaticConfig (..)
       , Config (..)
       , configK
       , configVssMinTTL
       , configVssMaxTTL
       , configBlkSecurityParam
       , configSlotSecurityParam
       , configChainQualityThreshold
       , configEpochSlots
       , configGeneratedSecretsThrow
       , configBootStakeholders
       , configHeavyDelegation
       , configStartTime
       , configVssCerts
       , configNonAvvmBalances
       , configBlockVersionData
       , configGenesisProtocolConstants
       , configAvvmDistr
       , configFtsSeed

       , ConfigurationError (..)
       , mkConfigFromStaticConfig
       , mkConfig

       , canonicalGenesisJson
       , prettyGenesisJson
       ) where

import           Universum

import           Control.Exception (throwIO)
import           Data.Aeson (FromJSON, ToJSON, Value (..), genericToEncoding,
                     pairs, parseJSON, toEncoding, (.:))
import           Data.Aeson.Encoding (pairStr)
import           Data.Aeson.Options (defaultOptions)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import           System.FilePath ((</>))
import           System.IO.Error (userError)
import qualified Text.JSON.Canonical as Canonical

import           Pos.Binary.Class (Raw)
import           Pos.Chain.Genesis.AvvmBalances (GenesisAvvmBalances (..))
import           Pos.Chain.Genesis.Data (GenesisData (..))
import           Pos.Chain.Genesis.Delegation (GenesisDelegation,
                     mkGenesisDelegation)
import           Pos.Chain.Genesis.Generate (GeneratedGenesisData (..),
                     GeneratedSecrets, generateGenesisData)
import           Pos.Chain.Genesis.Hash (GenesisHash (..))
import           Pos.Chain.Genesis.Initializer (GenesisInitializer (..))
import           Pos.Chain.Genesis.NonAvvmBalances (GenesisNonAvvmBalances)
import           Pos.Chain.Genesis.ProtocolConstants
                     (GenesisProtocolConstants (..),
                     genesisProtocolConstantsToProtocolConstants)
import           Pos.Chain.Genesis.Spec (GenesisSpec (..))
import           Pos.Chain.Genesis.WStakeholders (GenesisWStakeholders)
import           Pos.Chain.Ssc.VssCertificatesMap (VssCertificatesMap)
import           Pos.Chain.Update.BlockVersionData (BlockVersionData)
import           Pos.Core.Common (BlockCount, SharedSeed)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..),
                     pcBlkSecurityParam, pcChainQualityThreshold, pcEpochSlots,
                     pcSlotSecurityParam, vssMaxTTL, vssMinTTL)
import           Pos.Core.Slotting (SlotCount, Timestamp)
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic)
import           Pos.Crypto.Hashing (Hash, hashRaw, unsafeHash)
import           Pos.Util.Json.Canonical (SchemaError)
import           Pos.Util.Util (leftToPanic)

--------------------------------------------------------------------------------
-- StaticConfig
--------------------------------------------------------------------------------

data StaticConfig
      -- | Genesis from a 'GenesisSpec'.
    = GCSpec !GenesisSpec
      -- | 'GenesisData' is stored in a file.
    | GCSrc !FilePath !(Hash Raw)
      -- !FilePath = Path to file where 'GenesisData' is stored. Must be
      -- in JSON, not necessary canonical.
      -- !(Hash Raw) = Hash of canonically encoded 'GenesisData'.
    deriving (Eq, Show, Generic)

instance ToJSON StaticConfig where
    toEncoding (GCSrc gcsFile gcsHash) =
        pairs . pairStr "src"
            . pairs  $ pairStr "file"
                (toEncoding gcsFile) <> pairStr "hash" (toEncoding gcsHash)

    toEncoding (GCSpec value)          =
        genericToEncoding defaultOptions (GCSpec value)

instance FromJSON StaticConfig where
    parseJSON (Object o)
        | HM.member "src" o  = GCSrc <$> ((o .: "src") >>= (.: "file"))
                                      <*> ((o .: "src") >>= (.: "hash"))
        | HM.member "spec" o = do
              -- GCSpec Object
              specO <- o .: "spec"

              -- GenesisAvvmBalances
              avvmDistrO <- specO .: "avvmDistr"
              avvmDistr <- parseJSON (avvmDistrO)

              -- SharedSeed
              ftsSeed <- specO .: "ftsSeed"

              -- GenesisDelegation
              heavyDelegationO <- specO .: "heavyDelegation"
              heavyDelegation <- parseJSON (heavyDelegationO)

              -- BlockVersionData
              blockVersionDataO <- specO .: "blockVersionData"
              blockVersionData <- parseJSON blockVersionDataO

              -- GenesisProtocolConstants
              protocolConstantsO <- specO .: "protocolConstants"
              protocolConstants <- parseJSON protocolConstantsO

              -- GenesisInitializer
              initializerO <- specO .: "initializer"
              testBalanceO <- initializerO .: "testBalance"
              testBalance <- parseJSON testBalanceO
              fakeAvvmBalanceO <- (initializerO .: "fakeAvvmBalance")
              fakeAvvmBalance <- parseJSON fakeAvvmBalanceO
              avvmBalanceFactor <- initializerO .: "avvmBalanceFactor"
              useHeavyDlg <- initializerO .: "useHeavyDlg"
              seed <- initializerO .: "seed"

              return . GCSpec $
                  UnsafeGenesisSpec
                      (GenesisAvvmBalances avvmDistr)
                      ftsSeed
                      heavyDelegation
                      blockVersionData
                      protocolConstants
                      (GenesisInitializer
                          testBalance
                          fakeAvvmBalance
                          avvmBalanceFactor
                          useHeavyDlg
                          seed)
        | otherwise = fail "Incorrect JSON encoding for GenesisConfiguration"

    parseJSON invalid = typeMismatch "GenesisConfiguration" invalid

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

data Config = Config
    { configProtocolMagic     :: ProtocolMagic
    , configProtocolConstants :: ProtocolConstants
    , configGeneratedSecrets  :: Maybe GeneratedSecrets
    , configGenesisData       :: GenesisData
    , configGenesisHash       :: GenesisHash
    }

configK :: Config -> Int
configK = pcK . configProtocolConstants

configVssMinTTL :: Integral i => Config -> i
configVssMinTTL = vssMinTTL . configProtocolConstants

configVssMaxTTL :: Integral i => Config -> i
configVssMaxTTL = vssMaxTTL . configProtocolConstants

configBlkSecurityParam :: Config -> BlockCount
configBlkSecurityParam = pcBlkSecurityParam . configProtocolConstants

configSlotSecurityParam :: Config -> SlotCount
configSlotSecurityParam = pcSlotSecurityParam . configProtocolConstants

configChainQualityThreshold :: Fractional f => Config -> f
configChainQualityThreshold = pcChainQualityThreshold . configProtocolConstants

configEpochSlots :: Config -> SlotCount
configEpochSlots = pcEpochSlots . configProtocolConstants

configGeneratedSecretsThrow
    :: (HasCallStack, MonadIO m) => Config -> m GeneratedSecrets
configGeneratedSecretsThrow =
    maybe
            (liftIO $ throwIO $ userError
                "GeneratedSecrets missing from Genesis.Config"
            )
            pure
        . configGeneratedSecrets

configBootStakeholders :: Config -> GenesisWStakeholders
configBootStakeholders = gdBootStakeholders . configGenesisData

configHeavyDelegation :: Config -> GenesisDelegation
configHeavyDelegation = gdHeavyDelegation . configGenesisData

configStartTime :: Config -> Timestamp
configStartTime = gdStartTime . configGenesisData

configVssCerts :: Config -> VssCertificatesMap
configVssCerts = gdVssCerts . configGenesisData

configNonAvvmBalances :: Config -> GenesisNonAvvmBalances
configNonAvvmBalances = gdNonAvvmBalances . configGenesisData

configBlockVersionData :: Config -> BlockVersionData
configBlockVersionData = gdBlockVersionData . configGenesisData

configGenesisProtocolConstants :: Config -> GenesisProtocolConstants
configGenesisProtocolConstants = gdProtocolConsts . configGenesisData

configAvvmDistr :: Config -> GenesisAvvmBalances
configAvvmDistr = gdAvvmDistr . configGenesisData

configFtsSeed :: Config -> SharedSeed
configFtsSeed = gdFtsSeed . configGenesisData

canonicalGenesisJson :: GenesisData -> (BSL.ByteString, Hash Raw)
canonicalGenesisJson theGenesisData = (canonicalJsonBytes, jsonHash)
  where
    jsonHash = hashRaw canonicalJsonBytes
    canonicalJsonBytes = Canonical.renderCanonicalJSON $ runIdentity $ Canonical.toJSON theGenesisData

-- | Encode 'GenesisData' in JSON format in a pretty way. JSON object
-- is the same as in canonical JSON, but formatting doesn't adhere to
-- canonical JSON rules.
prettyGenesisJson :: GenesisData -> String
prettyGenesisJson theGenesisData =
    Canonical.prettyCanonicalJSON $
    runIdentity $ Canonical.toJSON theGenesisData

-- | Come up with a HasConfiguration constraint using a Configuration.
-- The Configuration record can be parsed from JSON or Yaml, and used to
-- get a GenesisSpec, either from the file itself or from another file:
-- the canonical JSON encoding of a mainnet genesis.
--
-- If the canonical JSON source is given, then it will be hashed and checked
-- against expected hash (which is also part of configuration in this case).
--
-- If the configuration gives a testnet genesis spec, then a start time must
-- be provided, probably sourced from command line arguments.
mkConfigFromStaticConfig
    :: (MonadThrow m, MonadIO m)
    => FilePath
    -- ^ Directory where 'configuration.yaml' is stored.
    -> Maybe Timestamp
    -- ^ Optional system start time.
    --   It must be given when the genesis spec uses a testnet initializer.
    -> Maybe Integer
    -- ^ Optional seed which overrides one from testnet initializer if
    -- provided.
    -> RequiresNetworkMagic
    -> StaticConfig
    -> m Config
mkConfigFromStaticConfig confDir mSystemStart mSeed rnm = \case
    -- If a 'GenesisData' source file is given, we check its hash against the
    -- given expected hash, parse it, and use the GenesisData to fill in all of
    -- the obligations.
    GCSrc fp expectedHash -> do
        !bytes <- liftIO $ BS.readFile (confDir </> fp)

        whenJust mSeed $ const $
            throwM $ MeaninglessSeed
                "Seed doesn't make sense when genesis data itself is provided"

        gdataJSON <- case Canonical.parseCanonicalJSON (BSL.fromStrict bytes) of
            Left str -> throwM $ GenesisDataParseFailure (fromString str)
            Right it -> return it

        theGenesisData <- case Canonical.fromJSON gdataJSON of
            Left err -> throwM $ GenesisDataSchemaError err
            Right it -> return it
        -- Override the RequiresNetworkMagic in GenesisData with the value
        -- specified in Configuration.
        let overriddenGenesisData = updateGD theGenesisData

        let (_, theGenesisHash) = canonicalGenesisJson overriddenGenesisData
            pc = genesisProtocolConstantsToProtocolConstants (gdProtocolConsts overriddenGenesisData)
            -- We must override the `getRequiresNetworkMagic` field of the `ProtocolMagic`,
            -- in order to preserve backwards compatibility of configuration.
            pm = (gpcProtocolMagic (gdProtocolConsts overriddenGenesisData))
                     { getRequiresNetworkMagic = rnm }
        when (theGenesisHash /= expectedHash) $
            throwM $ GenesisHashMismatch
                     (show theGenesisHash) (show expectedHash)

        pure $ Config
            { configProtocolMagic     = pm
            , configProtocolConstants = pc
            , configGeneratedSecrets  = Nothing
            , configGenesisData       = overriddenGenesisData
            , configGenesisHash       = GenesisHash $ coerce theGenesisHash
            }

    -- If a 'GenesisSpec' is given, we ensure we have a start time (needed if
    -- it's a testnet initializer) and then make a 'GenesisData' from it.
    GCSpec spec -> do

        theSystemStart <- case mSystemStart of
            Just it -> do
                return it
            Nothing -> throwM MissingSystemStartTime

        -- Override seed if necessary
        let overrideSeed :: Integer -> GenesisInitializer -> GenesisInitializer
            overrideSeed newSeed gi = gi {giSeed = newSeed}

        let theSpec = case mSeed of
                Nothing -> spec
                Just newSeed -> spec
                    { gsInitializer = overrideSeed newSeed (gsInitializer spec)
                    }
            -- Override the RequiresNetworkMagic in GenesisSpec with the value
            -- specified in Configuration.
            overriddenSpec = updateGS theSpec

        pure $ mkConfig theSystemStart overriddenSpec
  where
    updateGD :: GenesisData -> GenesisData
    updateGD gd = gd { gdProtocolConsts = updateGPC (gdProtocolConsts gd) }
    --
    updateGS :: GenesisSpec -> GenesisSpec
    updateGS gs = gs { gsProtocolConstants = updateGPC (gsProtocolConstants gs) }
    --
    updateGPC :: GenesisProtocolConstants -> GenesisProtocolConstants
    updateGPC gpc = gpc { gpcProtocolMagic = updatePM (gpcProtocolMagic gpc) }
    --
    updatePM :: ProtocolMagic -> ProtocolMagic
    updatePM pm = pm { getRequiresNetworkMagic = rnm }

mkConfig :: Timestamp -> GenesisSpec -> Config
mkConfig theSystemStart spec = Config
    { configProtocolMagic     = pm
    , configProtocolConstants = pc
    , configGeneratedSecrets  = Just ggdSecrets
    , configGenesisData       = genesisData
    , configGenesisHash       = genesisHash
    }
  where
    pm = gpcProtocolMagic (gsProtocolConstants spec)
    pc = genesisProtocolConstantsToProtocolConstants (gsProtocolConstants spec)

    -- Generate
    GeneratedGenesisData {..} =
        generateGenesisData pm pc (gsInitializer spec) (gsAvvmDistr spec)

    -- Unite with generated
    finalHeavyDelegation :: GenesisDelegation
    finalHeavyDelegation =
        leftToPanic "mkConfig"
            $  mkGenesisDelegation
            $  (toList $ gsHeavyDelegation spec)
            <> toList ggdDelegation

    -- Construct the final value
    genesisData = GenesisData
        { gdBootStakeholders = ggdBootStakeholders
        , gdHeavyDelegation  = finalHeavyDelegation
        , gdStartTime        = theSystemStart
        , gdVssCerts         = ggdVssCerts
        , gdNonAvvmBalances  = ggdNonAvvm
        , gdBlockVersionData = gsBlockVersionData spec
        , gdProtocolConsts   = gsProtocolConstants spec
        , gdAvvmDistr        = ggdAvvm
        , gdFtsSeed          = gsFtsSeed spec
        }

    -- Anything will do for the genesis hash. A hash of "patak" was used
    -- before, and so it remains.
    genesisHash = GenesisHash $ coerce $ unsafeHash @Text "patak"

data ConfigurationError =
      -- | A system start time must be given when a testnet genesis is used.
      -- A mainnet genesis has this built-in, so it's not needed.
      MissingSystemStartTime

      -- | Must not give a custom system start time when using a mainnet
      -- genesis.
    | UnnecessarySystemStartTime

    | GenesisDataParseFailure !Text
    | GenesisDataSchemaError !SchemaError

      -- | The GenesisData canonical JSON hash is different than expected.
    | GenesisHashMismatch !Text !Text

    | ConfigurationInternalError !Text

      -- | Custom seed was provided, but it doesn't make sense.
    | MeaninglessSeed !Text
    deriving (Show)

instance Exception ConfigurationError
