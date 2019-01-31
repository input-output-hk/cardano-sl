module Test.Pos.Crypto.Json where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.Pos.Crypto.Example (exampleProtocolMagic0,
                     exampleProtocolMagic1, exampleProtocolMagic2,
                     exampleProtocolMagic3, exampleProtocolMagic4)
import           Test.Pos.Crypto.Gen
import           Test.Pos.Util.Golden (discoverGolden, eachOf,
                     goldenTestJSONDec)
import           Test.Pos.Util.Tripping (discoverRoundTrip,
                     roundTripsAesonYamlBuildable, roundTripsAesonYamlShow)

--------------------------------------------------------------------------------
-- AbstractHash
--------------------------------------------------------------------------------

roundTripAbstractHashAeson :: Property
roundTripAbstractHashAeson =
    eachOf 1000 genUnitAbstractHash roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- ProtocolMagic
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `ProtocolMagic` JSON format, the `RequiresNetworkMagic` field defaults to
-- `RequiresMagic`.

golden_ProtocolMagic0AesonDec :: Property
golden_ProtocolMagic0AesonDec =
    goldenTestJSONDec
        exampleProtocolMagic0
            "test/golden/json/ProtocolMagic0_Legacy_HasNetworkMagic"

golden_ProtocolMagic1AesonDec :: Property
golden_ProtocolMagic1AesonDec =
    goldenTestJSONDec
        exampleProtocolMagic1
            "test/golden/json/ProtocolMagic1_Legacy_HasNetworkMagic"

golden_ProtocolMagic2AesonDec :: Property
golden_ProtocolMagic2AesonDec =
    goldenTestJSONDec
        exampleProtocolMagic2
            "test/golden/json/ProtocolMagic2_Legacy_HasNetworkMagic"

-- Legacy JSON encoding where requiresNetworkMagic was
-- encoded as "NMMustBeNothing" or "NMMustBeJust"

golden_ProtocolMagic3AesonDec_NMMustBeJust :: Property
golden_ProtocolMagic3AesonDec_NMMustBeJust =
    goldenTestJSONDec
        exampleProtocolMagic3
            "test/golden/json/ProtocolMagic_Legacy_NMMustBeJust"

golden_ProtocolMagic4AesonDec_NMMustBeNothing :: Property
golden_ProtocolMagic4AesonDec_NMMustBeNothing =
    goldenTestJSONDec
        exampleProtocolMagic4
            "test/golden/json/ProtocolMagic_Legacy_NMMustBeNothing"

roundTripProtocolMagicAeson :: Property
roundTripProtocolMagicAeson = roundTripsAesonYamlShow 1000 genProtocolMagic

--------------------------------------------------------------------------------
-- HDAddressPayload
--------------------------------------------------------------------------------

roundTripHDAddressPayloadAeson :: Property
roundTripHDAddressPayloadAeson =
    roundTripsAesonYamlShow 1000 genHDAddressPayload

--------------------------------------------------------------------------------
-- ProxyCert
--------------------------------------------------------------------------------

roundTripProxyCertAeson :: Property
roundTripProxyCertAeson = eachOf 100 genUnitProxyCert roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- ProxySecretKey
--------------------------------------------------------------------------------

roundTripProxySecretKeyAeson :: Property
roundTripProxySecretKeyAeson =
    eachOf 100 genUnitProxySecretKey roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- PublicKey
--------------------------------------------------------------------------------

roundTripPublicKeyAeson :: Property
roundTripPublicKeyAeson = eachOf 1000 genPublicKey roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- RedeemPublicKey
--------------------------------------------------------------------------------

roundTripRedeemPublicKeyAeson :: Property
roundTripRedeemPublicKeyAeson =
    eachOf 1000 genRedeemPublicKey roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- RedeemSignature
--------------------------------------------------------------------------------

roundTripRedeemSignatureAeson :: Property
roundTripRedeemSignatureAeson =
    eachOf 1000 genUnitRedeemSignature roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- Signature
--------------------------------------------------------------------------------

roundTripSignatureAeson :: Property
roundTripSignatureAeson = eachOf 1000 genUnitSignature roundTripsAesonYamlBuildable

tests :: IO Bool
tests = and <$> sequence [ H.checkSequential $$discoverGolden
                         , H.checkSequential $$discoverRoundTrip
                         ]
