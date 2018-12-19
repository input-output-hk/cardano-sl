{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Chain.Delegation.Bi
       ( tests
       ) where

import           Universum

import           Data.List ((!!))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.Chain.Delegation (DlgPayload (..), ProxySKBlockInfo)

import           Test.Pos.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     roundTripsBiBuildable, roundTripsBiShow)
import           Test.Pos.Chain.Delegation.Example (exampleLightDlgIndices,
                     exampleProxySKBlockInfo, staticHeavyDlgIndexes,
                     staticProxySKHeavys)
import           Test.Pos.Chain.Delegation.Gen (genDlgPayload, genHeavyDlgIndex,
                     genLightDlgIndices, genProxySKBlockInfo, genProxySKHeavy)
import           Test.Pos.Core.ExampleHelpers (feedPM)
import           Test.Pos.Util.Golden (discoverGolden, eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip)

--------------------------------------------------------------------------------
-- DlgPayload
--------------------------------------------------------------------------------
golden_DlgPayload :: Property
golden_DlgPayload = goldenTestBi dp "test/golden/bi/delegation/DlgPayload"
  where dp = UnsafeDlgPayload (take 4 staticProxySKHeavys)

roundTripDlgPayloadBi :: Property
roundTripDlgPayloadBi = eachOf 100 (feedPM genDlgPayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- HeavyDlgIndex
--------------------------------------------------------------------------------
golden_HeavyDlgIndex :: Property
golden_HeavyDlgIndex = goldenTestBi hdi "test/golden/bi/delegation/HeavyDlgIndex"
  where hdi = staticHeavyDlgIndexes !! 0

roundTripHeavyDlgIndexBi :: Property
roundTripHeavyDlgIndexBi = eachOf 1000 genHeavyDlgIndex roundTripsBiBuildable

--------------------------------------------------------------------------------
-- LightDlgIndices
--------------------------------------------------------------------------------
golden_LightDlgIndices :: Property
golden_LightDlgIndices = goldenTestBi exampleLightDlgIndices
                                      "test/golden/bi/delegation/LightDlgIndices"

roundTripLightDlgIndicesBi :: Property
roundTripLightDlgIndicesBi = eachOf 1000 genLightDlgIndices roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ProxySKBlockInfo
--------------------------------------------------------------------------------
golden_ProxySKBlockInfo_Nothing :: Property
golden_ProxySKBlockInfo_Nothing = goldenTestBi pskbi "test/golden/bi/delegation/ProxySKBlockInfo_Nothing"
  where pskbi = Nothing :: ProxySKBlockInfo

golden_ProxySKBlockInfo_Just :: Property
golden_ProxySKBlockInfo_Just = goldenTestBi exampleProxySKBlockInfo
                                            "test/golden/bi/delegation/ProxySKBlockInfo_Just"

roundTripProxySKBlockInfoBi :: Property
roundTripProxySKBlockInfoBi = eachOf 200 (feedPM genProxySKBlockInfo) roundTripsBiShow

--------------------------------------------------------------------------------
-- ProxySKHeavy
--------------------------------------------------------------------------------
golden_ProxySKHeavy :: Property
golden_ProxySKHeavy = goldenTestBi skh "test/golden/bi/delegation/ProxySKHeavy"
  where skh = staticProxySKHeavys !! 0

roundTripProxySKHeavyBi :: Property
roundTripProxySKHeavyBi = eachOf 200 (feedPM genProxySKHeavy) roundTripsBiBuildable

tests :: IO Bool
tests = and <$> sequence
    [ H.checkSequential $$discoverGolden
    , H.checkParallel $$discoverRoundTrip
    ]
