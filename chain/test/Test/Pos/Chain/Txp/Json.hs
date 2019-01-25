{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Pos.Chain.Txp.Json
       ( tests
       ) where
import           Universum

import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Set as S
import           Hedgehog (Property, assert, withTests)
import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)

import           Pos.Chain.Txp (TxpConfiguration (..))

import           Test.Pos.Chain.Txp.Gen (genTxValidationRulesConfig,
                     genTxpConfiguration)
import           Test.Pos.Core.ExampleHelpers (exampleAddress, exampleAddress1,
                     exampleAddress2, exampleAddress3, exampleAddress4)
import           Test.Pos.Util.Golden (discoverGolden, eachOf,
                     goldenTestJSONPretty, goldenValueEquiv)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)

-------------------------------------------------------------------------------
-- TxpConfiguration
-------------------------------------------------------------------------------

golden_TxpConfiguration0 :: Property
golden_TxpConfiguration0 =
    goldenTestJSONPretty exampleTxpConfiguration0
        "test/golden/json/TxpConfiguration0"

golden_TxpConfiguration1 :: Property
golden_TxpConfiguration1 =
    goldenTestJSONPretty exampleTxpConfiguration1
        "test/golden/json/TxpConfiguration1"

golden_TxpConfiguration2 :: Property
golden_TxpConfiguration2 =
    goldenTestJSONPretty exampleTxpConfiguration2
        "test/golden/json/TxpConfiguration2"

roundTripTxpConfiguration :: Property
roundTripTxpConfiguration =
    eachOf 200 genTxpConfiguration roundTripsAesonShow

golden_prettyEquivalence_TxpConfiguration0 :: Property
golden_prettyEquivalence_TxpConfiguration0 = withFrozenCallStack $ do
    withTests 1 . H.property $ do
        prettyJ <- liftIO $ LB.readFile "test/golden/json/TxpConfiguration0"
        oldJ <- liftIO $ LB.readFile "test/golden/oldJson/TxpConfiguration0"
        let equivTest = goldenValueEquiv
                            (eitherDecode prettyJ :: Either String TxpConfiguration)
                            (eitherDecode oldJ :: Either String TxpConfiguration)
        case equivTest of
            Left err    -> failWith Nothing $ "could not decode: " <> show err
            Right bool' -> assert bool'

golden_prettyEquivalence_TxpConfiguration1 :: Property
golden_prettyEquivalence_TxpConfiguration1 = withFrozenCallStack $ do
    withTests 1 . H.property $ do
        prettyJ <- liftIO $ LB.readFile "test/golden/json/TxpConfiguration1"
        oldJ <- liftIO $ LB.readFile "test/golden/oldJson/TxpConfiguration1"
        let equivTest = goldenValueEquiv
                            (eitherDecode prettyJ :: Either String TxpConfiguration)
                            (eitherDecode oldJ :: Either String TxpConfiguration)
        case equivTest of
            Left err    -> failWith Nothing $ "could not decode: " <> show err
            Right bool' -> assert bool'

golden_prettyEquivalence_TxpConfiguration2 :: Property
golden_prettyEquivalence_TxpConfiguration2 = withFrozenCallStack $ do
    withTests 1 . H.property $ do
        prettyJ <- liftIO $ LB.readFile "test/golden/json/TxpConfiguration2"
        oldJ <- liftIO $ LB.readFile "test/golden/oldJson/TxpConfiguration2"
        let equivTest = goldenValueEquiv
                            (eitherDecode prettyJ :: Either String TxpConfiguration)
                            (eitherDecode oldJ :: Either String TxpConfiguration)
        case equivTest of
            Left err    -> failWith Nothing $ "could not decode: " <> show err
            Right bool' -> assert bool'

-------------------------------------------------------------------------------
-- TxValidationRules
-------------------------------------------------------------------------------

roundTripTxValidationRulesConfig :: Property
roundTripTxValidationRulesConfig =
    eachOf 200 genTxValidationRulesConfig roundTripsAesonShow

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

exampleTxpConfiguration0 :: TxpConfiguration
exampleTxpConfiguration0 = TxpConfiguration 99 talsa
  where
    talsa = S.fromList [exampleAddress]

exampleTxpConfiguration1 :: TxpConfiguration
exampleTxpConfiguration1 = TxpConfiguration 9 talsa
  where
    talsa = S.fromList [exampleAddress1, exampleAddress2, exampleAddress3]

exampleTxpConfiguration2 :: TxpConfiguration
exampleTxpConfiguration2 = TxpConfiguration 700 talsa
  where
    talsa = S.fromList [exampleAddress4, exampleAddress]

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip
