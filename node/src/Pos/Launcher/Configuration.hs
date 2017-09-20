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

import           Data.Aeson                       (FromJSON (..), genericParseJSON)
import           Data.Default                     (Default (..))
import           Serokell.Aeson.Options           (defaultOptions)
import           System.Wlog                      (WithLogger)

-- FIXME consistency on the locus of the JSON instances for configuration.
-- Core keeps them separate, infra update and gt define them on-site.
import           Pos.Aeson.Core.Configuration     ()

import           Pos.Configuration
import           Pos.Core.Configuration
import           Pos.Core.Types                   (Timestamp)
import           Pos.Infra.Configuration
import           Pos.Ssc.GodTossing.Configuration
import           Pos.Update.Configuration
import           Pos.Util.Config                  (parseYamlConfig)

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
    } deriving (Show)

defaultConfigurationOptions :: ConfigurationOptions
defaultConfigurationOptions = ConfigurationOptions
    { cfoFilePath    = "node/configuration.yaml"
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
withConfigurations co@ConfigurationOptions{..} act = do
    putText $ show co
    Configuration{..} <- parseYamlConfig cfoFilePath cfoKey
    withCoreConfigurations ccCore cfoSystemStart $
        withInfraConfiguration ccInfra $
        withUpdateConfiguration ccUpdate $
        withGtConfiguration ccGt $
        withNodeConfiguration ccNode $ act
