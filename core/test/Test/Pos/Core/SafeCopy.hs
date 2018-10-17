module Test.Pos.Core.SafeCopy where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.Pos.Core.ExampleHelpers (exampleAddress, exampleAddress',
                     exampleAddress'1, exampleAddress'2, exampleAddress'3,
                     exampleAddress'4, exampleAddress'5, exampleAddress'6,
                     exampleAddress'7, exampleAddress1, exampleAddress2,
                     exampleAddress3, exampleAddress4, exampleAddress5,
                     exampleAddress6, exampleAddress7)
import           Test.Pos.Util.Golden (discoverGolden, goldenTestSafeCopy,
                     goldenTestSafeCopyDec)

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `Address` `SafeCopy` format, the `RequiresNetworkMagic` field defaults to
-- `RequiresNoMagic`.

golden_Address0 :: Property
golden_Address0 =
    goldenTestSafeCopyDec
        exampleAddress
        "test/golden/safecopy/Address0_Legacy_NoNetworkMagic"

golden_Address1 :: Property
golden_Address1 =
    goldenTestSafeCopyDec
        exampleAddress1
        "test/golden/safecopy/Address1_Legacy_NoNetworkMagic"

golden_Address2 :: Property
golden_Address2 =
    goldenTestSafeCopyDec
        exampleAddress2
        "test/golden/safecopy/Address2_Legacy_NoNetworkMagic"

golden_Address3 :: Property
golden_Address3 =
    goldenTestSafeCopyDec
        exampleAddress3
        "test/golden/safecopy/Address3_Legacy_NoNetworkMagic"

golden_Address4 :: Property
golden_Address4 =
    goldenTestSafeCopyDec
        exampleAddress4
        "test/golden/safecopy/Address4_Legacy_NoNetworkMagic"

golden_Address5 :: Property
golden_Address5 =
    goldenTestSafeCopy
        exampleAddress5
        "test/golden/safecopy/Address5_HasNetworkMagic"

golden_Address6 :: Property
golden_Address6 =
    goldenTestSafeCopy
        exampleAddress6
        "test/golden/safecopy/Address6_HasNetworkMagic"

golden_Address7 :: Property
golden_Address7 =
    goldenTestSafeCopy
        exampleAddress7
        "test/golden/safecopy/Address7_HasNetworkMagic"

--------------------------------------------------------------------------------
-- Address'
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `Address'` `SafeCopy` format, the `RequiresNetworkMagic` field defaults to
-- `RequiresNoMagic`.

golden_Address'0 :: Property
golden_Address'0 =
    goldenTestSafeCopyDec
        exampleAddress'
        "test/golden/safecopy/Address'0_Legacy_NoNetworkMagic"

golden_Address'1 :: Property
golden_Address'1 =
    goldenTestSafeCopyDec
        exampleAddress'1
        "test/golden/safecopy/Address'1_Legacy_NoNetworkMagic"

golden_Address'2 :: Property
golden_Address'2 =
    goldenTestSafeCopyDec
        exampleAddress'2
        "test/golden/safecopy/Address'2_Legacy_NoNetworkMagic"

golden_Address'3 :: Property
golden_Address'3 =
    goldenTestSafeCopyDec
        exampleAddress'3
        "test/golden/safecopy/Address'3_Legacy_NoNetworkMagic"

golden_Address'4 :: Property
golden_Address'4 =
    goldenTestSafeCopyDec
        exampleAddress'4
        "test/golden/safecopy/Address'4_Legacy_NoNetworkMagic"

golden_Address'5 :: Property
golden_Address'5 =
    goldenTestSafeCopy
        exampleAddress'5
        "test/golden/safecopy/Address'5_HasNetworkMagic"

golden_Address'6 :: Property
golden_Address'6 =
    goldenTestSafeCopy
        exampleAddress'6
        "test/golden/safecopy/Address'6_HasNetworkMagic"

golden_Address'7 :: Property
golden_Address'7 =
    goldenTestSafeCopy
        exampleAddress'7
        "test/golden/safecopy/Address'7_HasNetworkMagic"

tests :: IO Bool
tests = H.checkSequential $$discoverGolden
