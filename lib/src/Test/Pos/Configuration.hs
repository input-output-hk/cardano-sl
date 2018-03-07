{-# LANGUAGE RankNTypes #-}
-- | Default configuration for running tests.

module Test.Pos.Configuration
       ( defaultTestConf
       , defaultTestGenesisSpec
       , defaultTestBlockVersionData

       , HasStaticConfigurations
       , withDefConfiguration
       , withDefInfraConfiguration
       , withDefNodeConfiguration
       , withDefSscConfiguration
       , withDefUpdateConfiguration
       , withDefBlockConfiguration
       , withDefDlgConfiguration
       , withDefConfigurations
       , withStaticConfigurations
       ) where

import           Universum

import qualified Data.Aeson as J

import           Pos.Block.Configuration (HasBlockConfiguration, withBlockConfiguration)
import           Pos.Configuration (HasNodeConfiguration, withNodeConfiguration)
import           Pos.Core (BlockVersionData, HasConfiguration, withGenesisSpec)
import           Pos.Core.Configuration (CoreConfiguration (..), GenesisConfiguration (..))
import           Pos.Core.Genesis (GenesisSpec (..))
import           Pos.Delegation (HasDlgConfiguration, withDlgConfiguration)
import           Pos.Infra.Configuration (HasNtpConfiguration, withNtpConfiguration)
import           Pos.Launcher.Configuration (Configuration (..), HasConfigurations)
import           Pos.Ssc.Configuration (HasSscConfiguration, withSscConfiguration)
import           Pos.Txp (HasTxpConfiguration, withTxpConfiguration)
import           Pos.Update.Configuration (HasUpdateConfiguration, withUpdateConfiguration)
import           Pos.Util.Config (embedYamlConfigCT)


-- | This configuration is embedded into binary and is used by default
-- in tests.
defaultTestConf :: Configuration
defaultTestConf = case J.fromJSON $ J.Object jobj of
              J.Error str    -> error $ toText str
              J.Success conf -> conf
  where
    jobj = $(embedYamlConfigCT (Proxy @J.Object) "configuration.yaml" "configuration.yaml" "test")

defaultTestGenesisSpec :: GenesisSpec
defaultTestGenesisSpec =
    case ccGenesis (ccCore defaultTestConf) of
        GCSpec spec -> spec
        _           -> error "unexpected genesis type in test"

defaultTestBlockVersionData :: BlockVersionData
defaultTestBlockVersionData = gsBlockVersionData defaultTestGenesisSpec

-- | This constraint requires all configurations which are not
-- always hardcoded in tests (currently).
type HasStaticConfigurations =
    ( HasNtpConfiguration
    , HasUpdateConfiguration
    , HasSscConfiguration
    , HasBlockConfiguration
    , HasNodeConfiguration
    , HasDlgConfiguration
    , HasTxpConfiguration
    )

withDefNodeConfiguration :: (HasNodeConfiguration => r) -> r
withDefNodeConfiguration = withNodeConfiguration (ccNode defaultTestConf)

withDefSscConfiguration :: (HasSscConfiguration => r) -> r
withDefSscConfiguration = withSscConfiguration (ccSsc defaultTestConf)

withDefUpdateConfiguration :: (HasUpdateConfiguration => r) -> r
withDefUpdateConfiguration = withUpdateConfiguration (ccUpdate defaultTestConf)

withDefInfraConfiguration :: (HasNtpConfiguration => r) -> r
withDefInfraConfiguration = withNtpConfiguration (ccNtp defaultTestConf)

withDefBlockConfiguration :: (HasBlockConfiguration => r) -> r
withDefBlockConfiguration = withBlockConfiguration (ccBlock defaultTestConf)

withDefDlgConfiguration :: (HasDlgConfiguration => r) -> r
withDefDlgConfiguration = withDlgConfiguration (ccDlg defaultTestConf)

withDefTxpConfiguration :: (HasTxpConfiguration => r) -> r
withDefTxpConfiguration = withTxpConfiguration (ccTxp defaultTestConf)

withDefConfiguration :: (HasConfiguration => r) -> r
withDefConfiguration = withGenesisSpec 0 (ccCore defaultTestConf)

withStaticConfigurations :: (HasStaticConfigurations => r) -> r
withStaticConfigurations patak =
    withDefNodeConfiguration $
    withDefSscConfiguration $
    withDefUpdateConfiguration $
    withDefBlockConfiguration $
    withDefDlgConfiguration $
    withDefTxpConfiguration $
    withDefInfraConfiguration patak

withDefConfigurations :: (HasConfigurations => r) -> r
withDefConfigurations bardaq =
    withDefConfiguration $ withStaticConfigurations bardaq
