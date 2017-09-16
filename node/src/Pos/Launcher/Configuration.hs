{-# LANGUAGE Rank2Types #-}

-- | Configuration for a node: values which are constant for the lifetime of
-- the running program, not for the lifetime of the executable binary itself.

module Pos.Launcher.Configuration
    ( Configuration (..)
    , MultiConfiguration
    , HasConfigurations

    , ConfigurationOptions (..)
    , defaultConfigurationOptions

    , withConfigurations
    ) where

import           Universum

import qualified Crypto.Hash                as Hash
import           Data.Aeson                 (FromJSON (..), genericParseJSON)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import qualified Data.Yaml                  as Yaml
import           Formatting                 (sformat, shown, (%))
import           Serokell.Aeson.Options     (defaultOptions)
import           System.Wlog                (WithLogger, logInfo)
import qualified Text.JSON.Canonical        as Canonical

-- FIXME consistency on the locus of the JSON instances for configuration.
-- Core keeps them separate, infra update and gt define them on-site.
import           Pos.Aeson.Core.Configuration ()

import           Pos.Core.Types                   (Timestamp)
import           Pos.Core.Configuration
import           Pos.Core.Constants               (genesisDataDigest)
import           Pos.Core.Genesis                 (GenesisData (..), GeneratedGenesisData (..),
                                                   GenesisInitializer (..), GenesisSpec (..),
                                                   generateGenesisData)
import           Pos.Crypto.Hashing               (AbstractHash (..), unsafeHash)
import           Pos.Infra.Configuration
import           Pos.Update.Configuration
import           Pos.Ssc.GodTossing.Configuration
import           Pos.Configuration

-- | Product of all configurations required to run a node.
data Configuration = Configuration
    { ccCore   :: !CoreConfiguration
    , ccInfra  :: !InfraConfiguration
    , ccUpdate :: !UpdateConfiguration
    , ccGt     :: !GtConfiguration
    , ccNode   :: !NodeConfiguration
    } deriving (Show, Generic)

instance FromJSON Configuration where
    parseJSON = genericParseJSON defaultOptions

-- | Complete configurations keyed on texts. You may want to parse this from a
-- file and then use some command-line argument text value to select an
-- appropriate configuration.
type MultiConfiguration = Map Text Configuration

type HasConfigurations =
    ( HasConfiguration
    , HasInfraConfiguration
    , HasUpdateConfiguration
    , HasGtConfiguration
    , HasNodeConfiguration
    )

-- | Configuration yaml file location and the key to use. The file should
-- parse to a MultiConfiguration and the 'cfoKey' should be one of the keys
-- in the map.
data ConfigurationOptions = ConfigurationOptions
    { cfoFilePath    :: !FilePath
    , cfoKey         :: !Text
      -- | An optional system start time override. Required when using a
      -- testnet genesis configuration.
    , cfoSystemStart :: !(Maybe Timestamp)
    }
    deriving (Show)

defaultConfigurationOptions :: ConfigurationOptions
defaultConfigurationOptions = ConfigurationOptions
    { cfoFilePath    = "configuration.yaml"
    , cfoKey         = "default"
    , cfoSystemStart = Nothing
    }

instance Default ConfigurationOptions where
    def = defaultConfigurationOptions

-- | Parse some big yaml file to 'MultiConfiguration' and then use the
-- configuration at a given key.
withConfigurations
    :: (WithLogger m, MonadThrow m, MonadIO m)
    => ConfigurationOptions
    -> (HasConfigurations => m r)
    -> m r
withConfigurations cfo@ConfigurationOptions{..} act = do
    decoded <- liftIO $ Yaml.decodeFileEither cfoFilePath 
    multiConfig <- either (throwM . ConfigurationParseFailure cfo) return decoded
    Configuration{..} <- maybe (throwM (ConfigurationKeyNotFound cfo)) return (Map.lookup cfoKey multiConfig)
    withCoreConfigurations ccCore cfoSystemStart $
      withInfraConfiguration ccInfra $
      withUpdateConfiguration ccUpdate $
      withGtConfiguration ccGt $
      withNodeConfiguration ccNode $ act

data ConfigurationException =

      -- | Couldn't parse the configuration file.
      ConfigurationParseFailure !ConfigurationOptions !(Yaml.ParseException)

      -- | Configuration at the given key not found.
    | ConfigurationKeyNotFound !ConfigurationOptions

    deriving (Show)

instance Exception ConfigurationException

-- | Come up with a HasConfiguration constraint using a Configuration.
-- The Configuration record can be parsed from JSON or Yaml, and used to
-- get a GenesisSpec, either from the file itself or from another file:
-- the canonical JSON encoding of a mainnet genesis.
--
-- If the canonical JSON source is given, then it will be hashed and checked
-- against the constant expected genesisDataDigest.
--
-- If the configuration gives a testnet genesis spec, then a start time must
-- be provided, probably sourced from command line arguments.
withCoreConfigurations
    :: ( MonadThrow m
       , WithLogger m
       , MonadIO m
       , Canonical.FromJSON (Either String) GenesisData
       )
    => CoreConfiguration
    -> Maybe Timestamp
    -- ^ Optional system start time.
    --   It must be given when the genesis spec uses a testnet initializer.
    -> (HasConfiguration => m r)
    -> m r
withCoreConfigurations conf@CoreConfiguration{..} mSystemStart act = case ccGenesis of
    -- If a 'GenesisData' source file is given, we check its hash against the
    -- built-in constant, parse it, and use the GenesisData to fill in all of]
    -- the obligations.
    GCSrc fp -> do
        !bytes <- liftIO $ BS.readFile fp

        let digest :: Hash.Digest Hash.Blake2b_256
            digest = Hash.hash bytes
        when (digest /= genesisDataDigest) $
            throwM $ GenesisHashMismatch (sformat shown digest) (sformat shown genesisDataDigest)
        -- See Pos.Core.Constants. The genesisHash is conceptually distinct
        -- from the genesisDataDigest, they just happen to coincide for
        -- mainnet.
        let theGenesisHash = AbstractHash digest

        gdataJSON <- case Canonical.parseCanonicalJSON (BSL.fromStrict bytes) of
            Left str -> throwM $ GenesisDataParseFailure (fromString str)
            Right it -> return it

        gdata <- case Canonical.fromJSON gdataJSON of
            Left str -> throwM $ GenesisDataParseFailure (fromString str)
            Right it -> return it

        let theProtocolConstants = gdProtocolConsts gdata
            theBlockVersionData = gdBlockVersionData gdata
            theSharedSeed = gdFtsSeed gdata
            theGenesisAvvmBalances = gdAvvmDistr gdata
            theGenesisDelegation = gdHeavyDelegation gdata
            theSystemStart = gdStartTime gdata
            theGeneratedGData = GeneratedGenesisData [] (gdBootStakeholders gdata) (gdVssCerts gdata) Nothing Nothing

        withCoreConfiguration conf $
          withProtocolConstants theProtocolConstants $
          withGenesisBlockVersionData theBlockVersionData $
          withGenesisAvvmBalances theGenesisAvvmBalances $
          withGenesisDelegation theGenesisDelegation $
          withGenesisHash theGenesisHash $
          withSharedSeed theSharedSeed $
          withSystemStart theSystemStart $
          withGeneratedGenesisData theGeneratedGData $
          act

    -- If a 'GenesisSpec' is given, we ensure we have a start time (needed if
    -- it's a testnet initializer) and then make a 'GenesisData' from it.
    GCSpec spec -> do

        theSystemStart <- case mSystemStart of
            Just it -> case gsInitializer spec of
                TestnetInitializer{..} -> do
                    logInfo $ sformat ("withConfiguration using custom system start time "%shown) it
                    return it
                MainnetInitializer{..} -> throwM UnnecessarySystemStartTime
            Nothing -> case gsInitializer spec of
                TestnetInitializer{..} -> throwM MissingSystemStartTime
                MainnetInitializer{..} -> do
                    logInfo $ sformat ("withConfiguration using genesis configured system start time "%shown) miStartTime
                    return miStartTime

        -- We can immediately get everything we need except for the generated
        -- genesis data. To get that we need the protocol constants and block
        -- version data
        let theProtocolConstants = gsProtocolConstants spec
            theBlockVersionData = gsBlockVersionData spec
            theSharedSeed = gsFtsSeed spec
            theGenesisAvvmBalances = gsAvvmDistr spec
            theGenesisDelegation = gsHeavyDelegation spec
            -- Anything will do for the genesis hash. A hash of "patak" was used
            -- before, and so it remains.
            theGenesisHash = unsafeHash @Text "patak"

        withCoreConfiguration conf $
          withProtocolConstants theProtocolConstants $
          withGenesisBlockVersionData theBlockVersionData $
          withGenesisAvvmBalances theGenesisAvvmBalances $
          withGenesisDelegation theGenesisDelegation $
          withGenesisHash theGenesisHash $
          withSharedSeed theSharedSeed $
          withSystemStart theSystemStart $
              let theGeneratedGData = generateGenesisData (gsInitializer spec)
              in  withGeneratedGenesisData theGeneratedGData act

data ConfigurationError =
      -- | A system start time must be given when a testnet genesis is used.
      -- A mainnet genesis has this built-in, so it's not needed.
      MissingSystemStartTime

      -- | Must not give a custom system start time when using a mainnet
      -- genesis.
    | UnnecessarySystemStartTime

    | GenesisDataParseFailure !Text

      -- | The GenesisData canonical JSON hash is different than expected.
    | GenesisHashMismatch !Text !Text

    | ConfigurationInternalError !Text
    deriving (Show)

instance Exception ConfigurationError
