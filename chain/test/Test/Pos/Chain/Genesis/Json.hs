{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Chain.Genesis.Json
       ( tests
       ) where

import           Universum

import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LB
import           Hedgehog (Property, assert, withTests)
import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)
import           Pos.Chain.Genesis (GenesisProtocolConstants, StaticConfig)

import           Test.Pos.Chain.Genesis.Example (exampleGenesisData0,
                     exampleGenesisData1, exampleGenesisData2,
                     exampleGenesisProtocolConstants0,
                     exampleGenesisProtocolConstants1,
                     exampleGenesisProtocolConstants2,
                     exampleStaticConfig_GCSpec0, exampleStaticConfig_GCSpec1,
                     exampleStaticConfig_GCSpec2, exampleStaticConfig_GCSrc)
import           Test.Pos.Chain.Genesis.Gen (genGenesisAvvmBalances,
                     genGenesisData, genGenesisDelegation,
                     genGenesisInitializer, genGenesisProtocolConstants,
                     genStaticConfig)
import           Test.Pos.Core.ExampleHelpers (feedPM, feedPMWithRequiresMagic)
import           Test.Pos.Util.Golden (discoverGolden, eachOf,
                     goldenFileCanonicalEquiv, goldenTestCanonicalJSONDec,
                     goldenTestJSONDec, goldenTestJSONPretty, goldenValueEquiv)
import           Test.Pos.Util.Tripping (discoverRoundTrip,
                     roundTripsAesonYamlShow, roundTripsCanonicalJSONShow)

--------------------------------------------------------------------------------
-- StaticConfig
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `StaticConfig` JSON format, the `RequiresNetworkMagic` field defaults to
-- `RequiresMagic`.

golden_StaticConfig_GCSpec0Dec :: Property
golden_StaticConfig_GCSpec0Dec =
    goldenTestJSONDec
        exampleStaticConfig_GCSpec0
            "test/golden/json/StaticConfig_GCSpec0_Legacy_HasNetworkMagic"

golden_StaticConfig_GCSpec1Dec :: Property
golden_StaticConfig_GCSpec1Dec =
    goldenTestJSONDec
        exampleStaticConfig_GCSpec1
            "test/golden/json/StaticConfig_GCSpec1_Legacy_HasNetworkMagic"

golden_StaticConfig_GCSpec2Dec :: Property
golden_StaticConfig_GCSpec2Dec =
    goldenTestJSONDec
        exampleStaticConfig_GCSpec2
            "test/golden/json/StaticConfig_GCSpec2_Legacy_HasNetworkMagic"

golden_StaticConfig_GCSrc :: Property
golden_StaticConfig_GCSrc =
    goldenTestJSONPretty
        exampleStaticConfig_GCSrc
            "test/golden/json/StaticConfig_GCSrc"

roundTripStaticConfig :: Property
roundTripStaticConfig =
    roundTripsAesonYamlShow 100 (feedPM genStaticConfig)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- Pretty print format equivalence tests. The test reads and decodes the
-- non-prettified JSON (from oldJson dir) and the prettified JSON
-- (from json dir). If the decoding is successful the two values are compared.

golden_prettyEquivalence_StaticConfig_GCSrc :: Property
golden_prettyEquivalence_StaticConfig_GCSrc = withFrozenCallStack $ do
    withTests 1 . H.property $ do
        prettyJ <- liftIO $ LB.readFile "test/golden/json/StaticConfig_GCSrc"
        oldJ <- liftIO $ LB.readFile "test/golden/oldJson/StaticConfig_GCSrc"
        let equivTest = goldenValueEquiv
                            (eitherDecode prettyJ :: Either String StaticConfig)
                            (eitherDecode oldJ :: Either String StaticConfig)
        case equivTest of
            Left err    -> failWith Nothing $ "could not decode: " <> show err
            Right bool' -> assert bool'

golden_prettyEquivalence_StaticConfig_GCSrc0 :: Property
golden_prettyEquivalence_StaticConfig_GCSrc0 = withFrozenCallStack $ do
    withTests 1 . H.property $ do
        prettyJ <- liftIO $ LB.readFile pFile
        oldJ <- liftIO $ LB.readFile oFile
        let equivTest = goldenValueEquiv
                            (eitherDecode prettyJ :: Either String StaticConfig)
                            (eitherDecode oldJ :: Either String StaticConfig)
        case equivTest of
            Left err    -> failWith Nothing $ "could not decode: " <> show err
            Right bool' -> assert bool'
   where
      pFile = "test/golden/json/StaticConfig_GCSpec0_Legacy_HasNetworkMagic"
      oFile = "test/golden/oldJson/StaticConfig_GCSpec0_Legacy_HasNetworkMagic"

golden_prettyEquivalence_StaticConfig_GCSrc1 :: Property
golden_prettyEquivalence_StaticConfig_GCSrc1 = withFrozenCallStack $ do
    withTests 1 . H.property $ do
        prettyJ <- liftIO $ LB.readFile pFile
        oldJ <- liftIO $ LB.readFile oFile
        let equivTest = goldenValueEquiv
                            (eitherDecode prettyJ :: Either String StaticConfig)
                            (eitherDecode oldJ :: Either String StaticConfig)
        case equivTest of
            Left err    -> failWith Nothing $ "could not decode: " <> show err
            Right bool' -> assert bool'
   where
      pFile = "test/golden/json/StaticConfig_GCSpec1_Legacy_HasNetworkMagic"
      oFile = "test/golden/oldJson/StaticConfig_GCSpec1_Legacy_HasNetworkMagic"

golden_prettyEquivalence_StaticConfig_GCSrc2 :: Property
golden_prettyEquivalence_StaticConfig_GCSrc2 = withFrozenCallStack $ do
    withTests 1 . H.property $ do
        prettyJ <- liftIO $ LB.readFile pFile
        oldJ <- liftIO $ LB.readFile oFile
        let equivTest = goldenValueEquiv
                            (eitherDecode prettyJ :: Either String StaticConfig)
                            (eitherDecode oldJ :: Either String StaticConfig)
        case equivTest of
            Left err    -> failWith Nothing $ "could not decode: " <> show err
            Right bool' -> assert bool'
   where
      pFile = "test/golden/json/StaticConfig_GCSpec2_Legacy_HasNetworkMagic"
      oFile = "test/golden/oldJson/StaticConfig_GCSpec2_Legacy_HasNetworkMagic"

--------------------------------------------------------------------------------
-- GenesisData (Canonical JSON)
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `GenesisData` canonical JSON format, the `RequiresNetworkMagic` field
-- defaults to `RequiresMagic`.

golden_GenesisData0Dec :: Property
golden_GenesisData0Dec =
    goldenTestCanonicalJSONDec
        exampleGenesisData0
            "test/golden/canonical-json/GenesisData0_Legacy_HasNetworkMagic"

golden_GenesisDataDec1 :: Property
golden_GenesisDataDec1 =
    goldenTestCanonicalJSONDec
        exampleGenesisData1
            "test/golden/canonical-json/GenesisData1_Legacy_HasNetworkMagic"

golden_GenesisDataDec2 :: Property
golden_GenesisDataDec2 =
    goldenTestCanonicalJSONDec
        exampleGenesisData2
            "test/golden/canonical-json/GenesisData2_Legacy_HasNetworkMagic"

roundTripGenesisData :: Property
roundTripGenesisData =
    eachOf 100 (feedPMWithRequiresMagic genGenesisData) roundTripsCanonicalJSONShow

golden_prettyEquivalence_canonical_GenesisData_0 :: Property
golden_prettyEquivalence_canonical_GenesisData_0 =
    goldenFileCanonicalEquiv
        "test/golden/canonical-json/GenesisData0_Legacy_HasNetworkMagic"
            "test/golden/oldCanonical-json/GenesisData0_Legacy_HasNetworkMagic"

golden_prettyEquivalence_canonical_GenesisData_1 :: Property
golden_prettyEquivalence_canonical_GenesisData_1 =
    goldenFileCanonicalEquiv
        "test/golden/canonical-json/GenesisData1_Legacy_HasNetworkMagic"
            "test/golden/oldCanonical-json/GenesisData1_Legacy_HasNetworkMagic"

golden_prettyEquivalence_canonical_GenesisData_2 :: Property
golden_prettyEquivalence_canonical_GenesisData_2 =
    goldenFileCanonicalEquiv
        "test/golden/canonical-json/GenesisData2_Legacy_HasNetworkMagic"
            "test/golden/oldCanonical-json/GenesisData2_Legacy_HasNetworkMagic"
--------------------------------------------------------------------------------
-- GenesisAvvmBalances
--------------------------------------------------------------------------------

roundTripGenesisAvvmBalances :: Property
roundTripGenesisAvvmBalances =
     roundTripsAesonYamlShow 100 genGenesisAvvmBalances

--------------------------------------------------------------------------------
-- GenesisDelegation
--------------------------------------------------------------------------------

roundTripGenesisDelegation :: Property
roundTripGenesisDelegation =
    roundTripsAesonYamlShow 100 (feedPM genGenesisDelegation)

--------------------------------------------------------------------------------
-- GenesisProtocolConstants
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `GenesisProtocolConstants` JSON format, the `RequiresNetworkMagic` field
-- defaults to `RequiresMagic`.

golden_GenesisProtocolConstants0Dec :: Property
golden_GenesisProtocolConstants0Dec =
    goldenTestJSONDec exampleGenesisProtocolConstants0
        "test/golden/json/GenesisProtocolConstants0_Legacy_HasNetworkMagic"

golden_GenesisProtocolConstants1Dec :: Property
golden_GenesisProtocolConstants1Dec =
    goldenTestJSONDec exampleGenesisProtocolConstants1
        "test/golden/json/GenesisProtocolConstants1_Legacy_HasNetworkMagic"

golden_GenesisProtocolConstants2Dec :: Property
golden_GenesisProtocolConstants2Dec =
    goldenTestJSONDec exampleGenesisProtocolConstants2
        "test/golden/json/GenesisProtocolConstants2_Legacy_HasNetworkMagic"

roundTripGenesisProtocolConstants :: Property
roundTripGenesisProtocolConstants =
    roundTripsAesonYamlShow 1000 (feedPM genGenesisProtocolConstants)

golden_prettyEquivalence_GenesisProtocolConstants0 :: Property
golden_prettyEquivalence_GenesisProtocolConstants0 = withFrozenCallStack $ do
    withTests 1 . H.property $ do
        prettyJ <- liftIO $ LB.readFile pFile
        oldJ <- liftIO $ LB.readFile oFile
        let equivTest = goldenValueEquiv
                (eitherDecode prettyJ :: Either String GenesisProtocolConstants)
                (eitherDecode oldJ :: Either String GenesisProtocolConstants)
        case equivTest of
            Left err    -> failWith Nothing $ "could not decode: " <> show err
            Right bool' -> assert bool'
  where
    pFile = "test/golden/json/GenesisProtocolConstants0_Legacy_HasNetworkMagic"
    oFile = "test/golden/oldJson/GenesisProtocolConstants0_Legacy_HasNetworkMagic"

golden_prettyEquivalence_GenesisProtocolConstants1 :: Property
golden_prettyEquivalence_GenesisProtocolConstants1 = withFrozenCallStack $ do
    withTests 1 . H.property $ do
        prettyJ <- liftIO $ LB.readFile pFile
        oldJ <- liftIO $ LB.readFile oFile
        let equivTest = goldenValueEquiv
                (eitherDecode prettyJ :: Either String GenesisProtocolConstants)
                (eitherDecode oldJ :: Either String GenesisProtocolConstants)
        case equivTest of
            Left err    -> failWith Nothing $ "could not decode: " <> show err
            Right bool' -> assert bool'
  where
    pFile = "test/golden/json/GenesisProtocolConstants1_Legacy_HasNetworkMagic"
    oFile = "test/golden/oldJson/GenesisProtocolConstants1_Legacy_HasNetworkMagic"

golden_prettyEquivalence_GenesisProtocolConstants2 :: Property
golden_prettyEquivalence_GenesisProtocolConstants2 = withFrozenCallStack $ do
    withTests 1 . H.property $ do
        prettyJ <- liftIO $ LB.readFile pFile
        oldJ <- liftIO $ LB.readFile oFile
        let equivTest = goldenValueEquiv
                (eitherDecode prettyJ :: Either String GenesisProtocolConstants)
                (eitherDecode oldJ :: Either String GenesisProtocolConstants)
        case equivTest of
            Left err    -> failWith Nothing $ "could not decode: " <> show err
            Right bool' -> assert bool'
  where
    pFile = "test/golden/json/GenesisProtocolConstants2_Legacy_HasNetworkMagic"
    oFile = "test/golden/oldJson/GenesisProtocolConstants2_Legacy_HasNetworkMagic"
--------------------------------------------------------------------------------
-- GenesisInitializer
--------------------------------------------------------------------------------

roundTripGenesisInitializer :: Property
roundTripGenesisInitializer =
    roundTripsAesonYamlShow 1000 genGenesisInitializer

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip
