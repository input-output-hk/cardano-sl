{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Pos.Core.Configuration
       ( Config (..)
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
       , withCoreConfigurations
       , mkConfig

       , canonicalGenesisJson
       , prettyGenesisJson

       , module E
       ) where

import           Universum

import           Control.Exception (throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Coerce (coerce)
import           System.FilePath ((</>))
import           System.IO.Error (userError)
import qualified Text.JSON.Canonical as Canonical

import           Pos.Binary.Class (Raw)
import           Pos.Core.Common (BlockCount, SharedSeed)
import           Pos.Core.Configuration.Core as E
import           Pos.Core.Configuration.GenesisHash as E
import           Pos.Core.Genesis (GeneratedSecrets, GenesisAvvmBalances,
                     GenesisData (..), GenesisDelegation,
                     GenesisInitializer (..), GenesisNonAvvmBalances,
                     GenesisProtocolConstants (..), GenesisSpec (..),
                     GenesisWStakeholders,
                     genesisProtocolConstantsToProtocolConstants,
                     mkGenesisDelegation)
import           Pos.Core.Genesis.Generate (GeneratedGenesisData (..),
                     generateGenesisData)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..),
                     pcBlkSecurityParam, pcChainQualityThreshold, pcEpochSlots,
                     pcSlotSecurityParam, vssMaxTTL, vssMinTTL)
import           Pos.Core.Slotting (SlotCount, Timestamp)
import           Pos.Core.Ssc (VssCertificatesMap)
import           Pos.Core.Update (BlockVersionData)
import           Pos.Crypto.Configuration as E
import           Pos.Crypto.Hashing (Hash, hashRaw, unsafeHash)
import           Pos.Util.Json.Canonical (SchemaError)
import           Pos.Util.Util (leftToPanic)

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
                "GeneratedSecrets missing from Core.Config"
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
withCoreConfigurations
    :: (MonadThrow m, MonadIO m)
    => CoreConfiguration
    -- ^ Update @'GenesisData'@ before passing its parts to @'given'@.
    -> FilePath
    -- ^ Directory where 'configuration.yaml' is stored.
    -> Maybe Timestamp
    -- ^ Optional system start time.
    --   It must be given when the genesis spec uses a testnet initializer.
    -> Maybe Integer
    -- ^ Optional seed which overrides one from testnet initializer if
    -- provided.
    -> m Config
withCoreConfigurations conf confDir mSystemStart mSeed = case ccGenesis conf of
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

        let (_, theGenesisHash) = canonicalGenesisJson theGenesisData
            pc = genesisProtocolConstantsToProtocolConstants (gdProtocolConsts theGenesisData)
            pm = gpcProtocolMagic (gdProtocolConsts theGenesisData)
        when (theGenesisHash /= expectedHash) $
            throwM $ GenesisHashMismatch
                     (show theGenesisHash) (show expectedHash)

        pure $ Config
            { configProtocolMagic     = pm
            , configProtocolConstants = pc
            , configGeneratedSecrets  = Nothing
            , configGenesisData       = theGenesisData
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

        pure $ mkConfig theSystemStart theSpec

mkConfig
    :: Timestamp
    -> GenesisSpec
    -> Config
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
