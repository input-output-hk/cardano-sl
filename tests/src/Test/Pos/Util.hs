{-# LANGUAGE RankNTypes #-}
module Test.Pos.Util
       ( HasStaticConfigurations
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

import           Pos.Block.Configuration (HasBlockConfiguration, withBlockConfiguration)
import           Pos.Configuration (HasNodeConfiguration, withNodeConfiguration)
import           Pos.Core (HasConfiguration, withGenesisSpec)
import           Pos.Delegation (HasDlgConfiguration, withDlgConfiguration)
import           Pos.Infra.Configuration (HasInfraConfiguration, withInfraConfiguration)
import           Pos.Launcher.Configuration (Configuration (..), HasConfigurations)
import           Pos.Ssc.Configuration (HasSscConfiguration, withSscConfiguration)
import           Pos.Update.Configuration (HasUpdateConfiguration, withUpdateConfiguration)

import           Test.Pos.Configuration (defaultTestConf)

-- | This constraint requires all configurations which are not
-- always hardcoded in tests (currently).
type HasStaticConfigurations =
    ( HasInfraConfiguration
    , HasUpdateConfiguration
    , HasSscConfiguration
    , HasBlockConfiguration
    , HasNodeConfiguration
    , HasDlgConfiguration
    )

withDefNodeConfiguration :: (HasNodeConfiguration => r) -> r
withDefNodeConfiguration = withNodeConfiguration (ccNode defaultTestConf)

withDefSscConfiguration :: (HasSscConfiguration => r) -> r
withDefSscConfiguration = withSscConfiguration (ccSsc defaultTestConf)

withDefUpdateConfiguration :: (HasUpdateConfiguration => r) -> r
withDefUpdateConfiguration = withUpdateConfiguration (ccUpdate defaultTestConf)

withDefInfraConfiguration :: (HasInfraConfiguration => r) -> r
withDefInfraConfiguration = withInfraConfiguration (ccInfra defaultTestConf)

withDefBlockConfiguration :: (HasBlockConfiguration => r) -> r
withDefBlockConfiguration = withBlockConfiguration (ccBlock defaultTestConf)

withDefDlgConfiguration :: (HasDlgConfiguration => r) -> r
withDefDlgConfiguration = withDlgConfiguration (ccDlg defaultTestConf)

withDefConfiguration :: (HasConfiguration => r) -> r
withDefConfiguration = withGenesisSpec 0 (ccCore defaultTestConf)

withStaticConfigurations :: (HasStaticConfigurations => r) -> r
withStaticConfigurations patak =
    withDefNodeConfiguration $
    withDefSscConfiguration $
    withDefUpdateConfiguration $
    withDefBlockConfiguration $
    withDefDlgConfiguration $
    withDefInfraConfiguration patak

withDefConfigurations :: (HasConfigurations => r) -> r
withDefConfigurations bardaq =
    withDefConfiguration $ withStaticConfigurations bardaq
