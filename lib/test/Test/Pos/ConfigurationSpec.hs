{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module tests configuration.

module Test.Pos.ConfigurationSpec
       ( spec
       ) where

import           Universum

import           System.Wlog (HasLoggerName (..), LoggerName (..))

import           Mockable.Production (Production (..))
import           Pos.Core.Slotting (getCurrentTimestamp)
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import           Pos.Infra.Ntp.Configuration (NtpConfiguration)
import           Pos.Launcher.Configuration (ConfigurationOptions (..), withConfigurations)

import           Test.Hspec (Spec, describe, it, runIO, shouldBe)

instance HasLoggerName IO where
    askLoggerName = pure $ LoggerName "ConfigurationSpec"
    modifyLoggerName _ x = x

configFilePath :: FilePath
configFilePath = "configuration.yaml"

checkYamlSection :: Text -> Spec
checkYamlSection key = describe ("key: " ++ show key) $ do
    startTime <- runIO (runProduction getCurrentTimestamp)
    let opts = ConfigurationOptions configFilePath key (Just startTime) Nothing
    rnm <- runIO (withConfigurations Nothing opts getRNM)
    it "should be NMMustBeNothing" $
        rnm `shouldBe` NMMustBeNothing

getRNM :: NtpConfiguration -> ProtocolMagic -> IO RequiresNetworkMagic
getRNM _ pm = pure (getRequiresNetworkMagic pm)

spec :: Spec
spec = mapM_ checkYamlSection [
    "mainnet_full" -- mainnet core/relay nodes and exchange wallets
  , "mainnet_dryrun_full" -- staging core/relay nodes and exchange wallets
  , "mainnet_wallet_win64" -- mainnet wallets (daedalus)
  , "mainnet_wallet_macos64"
  , "mainnet_wallet_linux64"
  , "mainnet_dryrun_wallet_win64" -- staging wallets (daedalus)
  , "mainnet_dryrun_wallet_macos64"
  , "mainnet_dryrun_wallet_linux64"
  ]
