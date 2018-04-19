{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration
       ( ConfigurationError (..)
       , HasConfiguration
       , withCoreConfigurations
       , withGenesisSpec

       , canonicalGenesisJson
       , prettyGenesisJson

       , module E
       ) where

import           Universum

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath ((</>))
import qualified Text.JSON.Canonical as Canonical

import           Pos.Binary.Class (Raw)
import           Pos.Core.Configuration.BlockVersionData as E
import           Pos.Core.Configuration.Core as E
import           Pos.Core.Configuration.GeneratedSecrets as E
import           Pos.Core.Configuration.GenesisData as E
import           Pos.Core.Configuration.GenesisHash as E
import           Pos.Core.Configuration.Protocol as E
import           Pos.Core.Genesis.Canonical (SchemaError)
import           Pos.Core.Genesis.Generate (GeneratedGenesisData (..), generateGenesisData)
import           Pos.Core.Genesis.Helpers (mkGenesisDelegation)
import           Pos.Core.Genesis.Types (GenesisData (..), GenesisDelegation,
                                         GenesisInitializer (..), GenesisSpec (..),
                                         GenesisProtocolConstants (..),
                                         genesisProtocolConstantsToProtocolConstants)
import           Pos.Core.Slotting.Types (Timestamp)
import           Pos.Crypto.Configuration as E
import           Pos.Crypto.Hashing (Hash, hashRaw, unsafeHash)
import           Pos.Util.Util (leftToPanic)

-- | Coarse catch-all configuration constraint for use by depending modules.
type HasConfiguration =
    ( HasProtocolMagic
    , HasCoreConfiguration
    , HasGenesisData
    , HasGenesisHash
    , HasGeneratedSecrets
    , HasGenesisBlockVersionData
    , HasProtocolConstants
    )

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
    :: forall m r.
       ( MonadThrow m
       , MonadIO m
       , Canonical.FromJSON (Either SchemaError) GenesisData
       )
    => CoreConfiguration
    -> FilePath
    -- ^ Directory where 'configuration.yaml' is stored.
    -> Maybe Timestamp
    -- ^ Optional system start time.
    --   It must be given when the genesis spec uses a testnet initializer.
    -> Maybe Integer
    -- ^ Optional seed which overrides one from testnet initializer if
    -- provided.
    -> (HasConfiguration => m r)
    -> m r
withCoreConfigurations conf@CoreConfiguration{..} confDir mSystemStart mSeed act = case ccGenesis of
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

        withCoreConfiguration conf $
            withProtocolMagic pm $
            withProtocolConstants pc $
            withGenesisBlockVersionData (gdBlockVersionData theGenesisData) $
            withGenesisData theGenesisData $
            withGenesisHash theGenesisHash $
            withGeneratedSecrets Nothing $
            act

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

        let theConf = conf {ccGenesis = GCSpec theSpec}

        withGenesisSpec theSystemStart theConf act

withGenesisSpec
    :: Timestamp
    -> CoreConfiguration
    -> (HasConfiguration => r)
    -> r
withGenesisSpec theSystemStart conf@CoreConfiguration{..} val = case ccGenesis of
    GCSrc {} -> error "withGenesisSpec called with GCSrc"
    GCSpec spec ->
        withProtocolMagic pm $
        withProtocolConstants pc $
        withGenesisBlockVersionData (gsBlockVersionData spec) $
            let
                -- Generate
                GeneratedGenesisData {..} =
                    generateGenesisData protocolMagic (gsInitializer spec) (gsAvvmDistr spec)

                -- Unite with generated
                finalHeavyDelegation :: GenesisDelegation
                finalHeavyDelegation =
                    leftToPanic "withGenesisSpec" $ mkGenesisDelegation $
                    (toList $ gsHeavyDelegation spec) <> toList ggdDelegation

                -- Construct the final value
                theGenesisData =
                   GenesisData
                      { gdBootStakeholders = ggdBootStakeholders
                      , gdHeavyDelegation  = finalHeavyDelegation
                      , gdStartTime        = theSystemStart
                      , gdVssCerts         = ggdVssCerts
                      , gdNonAvvmBalances  = ggdNonAvvm
                      , gdBlockVersionData = genesisBlockVersionData
                      , gdProtocolConsts   = gsProtocolConstants spec
                      , gdAvvmDistr        = ggdAvvm
                      , gdFtsSeed          = gsFtsSeed spec
                      }
                -- Anything will do for the genesis hash. A hash of "patak" was used
                -- before, and so it remains.
                theGenesisHash = unsafeHash @Text "patak"
             in withCoreConfiguration conf $
                  withGenesisHash theGenesisHash $
                  withGeneratedSecrets (Just ggdSecrets) $
                  withGenesisData theGenesisData val
      where
        pm = gpcProtocolMagic (gsProtocolConstants spec)
        pc = genesisProtocolConstantsToProtocolConstants (gsProtocolConstants spec)

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
