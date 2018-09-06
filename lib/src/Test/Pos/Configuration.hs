{-# LANGUAGE RankNTypes #-}
-- | Default configuration for running tests.

module Test.Pos.Configuration
       ( defaultTestConf
       , defaultTestGenesisSpec
       , defaultTestBlockVersionData

       , HasStaticConfigurations
       , withDefConfiguration
       , withProvidedMagicConfig
       , withDefNtpConfiguration
       , withDefNodeConfiguration
       , withDefSscConfiguration
       , withDefUpdateConfiguration
       , withDefBlockConfiguration
       , withDefDlgConfiguration
       , withDefConfigurations
       , withStaticConfigurations

       , HasConfigurations
       ) where

import           Universum

import qualified Data.Aeson as J

import           Pos.Block.Configuration (HasBlockConfiguration, withBlockConfiguration)
import           Pos.Configuration (HasNodeConfiguration, withNodeConfiguration)
import           Pos.Core (BlockVersionData, HasConfiguration, withGenesisSpec)
import           Pos.Core.Configuration (CoreConfiguration (..), GenesisConfiguration (..))
import           Pos.Core.Genesis (GenesisProtocolConstants (..), GenesisSpec (..))
import           Pos.Crypto (ProtocolMagic)
import           Pos.Delegation (HasDlgConfiguration, withDlgConfiguration)
import           Pos.Infra.Ntp.Configuration (NtpConfiguration)
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
    ( HasUpdateConfiguration
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

withDefNtpConfiguration :: (NtpConfiguration -> r) -> r
withDefNtpConfiguration fn = fn (ccNtp defaultTestConf)

withDefBlockConfiguration :: (HasBlockConfiguration => r) -> r
withDefBlockConfiguration = withBlockConfiguration (ccBlock defaultTestConf)

withDefDlgConfiguration :: (HasDlgConfiguration => r) -> r
withDefDlgConfiguration = withDlgConfiguration (ccDlg defaultTestConf)

withDefTxpConfiguration :: (HasTxpConfiguration => r) -> r
withDefTxpConfiguration = withTxpConfiguration (ccTxp defaultTestConf)

withDefConfiguration :: (HasConfiguration => ProtocolMagic -> r) -> r
withDefConfiguration = withGenesisSpec 0 (ccCore defaultTestConf)

withProvidedMagicConfig :: ProtocolMagic -> (HasConfigurations => r) -> r
withProvidedMagicConfig pm f = withGenesisSpec 0 (updateCC (ccCore defaultTestConf)) (\_pm -> withStaticConfigurations (const f))
  where
    updateCC :: CoreConfiguration -> CoreConfiguration
    updateCC cc = cc { ccGenesis = updateGC (ccGenesis cc) }
    --
    updateGC :: GenesisConfiguration -> GenesisConfiguration
    updateGC (GCSrc _ _) = error "got GCSrc"
    updateGC (GCSpec spec) = GCSpec $ spec
        { gsProtocolConstants = updateGPC (gsProtocolConstants spec) }
    --
    updateGPC :: GenesisProtocolConstants -> GenesisProtocolConstants
    updateGPC gpc = gpc { gpcProtocolMagic = pm }

withStaticConfigurations :: (HasStaticConfigurations => NtpConfiguration -> r) -> r
withStaticConfigurations patak =
    withDefNodeConfiguration $
    withDefSscConfiguration $
    withDefUpdateConfiguration $
    withDefBlockConfiguration $
    withDefDlgConfiguration $
    withDefTxpConfiguration $
    withDefNtpConfiguration patak

withDefConfigurations
    :: (HasConfigurations => NtpConfiguration -> ProtocolMagic -> r) -> r
withDefConfigurations bardaq =
    withDefConfiguration $ withStaticConfigurations bardaq
