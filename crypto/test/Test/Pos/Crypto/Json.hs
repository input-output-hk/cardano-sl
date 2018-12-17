module Test.Pos.Crypto.Json where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.Pos.Crypto.Example (exampleProtocolMagic0,
                     exampleProtocolMagic1, exampleProtocolMagic2,
                     exampleProtocolMagic3, exampleProtocolMagic4)
import           Test.Pos.Util.Golden (discoverGolden, goldenTestJSONDec)

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

tests :: IO Bool
tests = H.checkSequential $$discoverGolden
