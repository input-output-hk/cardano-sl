module Test.Pos.Binary.Class.PrimitiveSpec
    ( spec
    ) where

import           Universum

import qualified Data.ByteString.Lazy as BSL
import           Codec.CBOR.Write (toStrictByteString, toLazyByteString)
import           Test.Hspec (Spec, describe, it)
import           Test.QuickCheck (arbitrary, forAll, (===))
import           Serokell.Data.Memory.Units (Byte, fromBytes)

import           Pos.Binary.Class (Bi (..), encodeKnownCborDataItem, encodeUnknownCborDataItem,
                                   encodedKnownCborDataItemSize, encodedUnknownCborDataItemSize,
                                   encodeCrcProtected, encodedCrcProtectedSize,
                                   withSize)
import           Test.Pos.Binary.Class.Core
import           Data.ByteString.Arbitrary

byteStringSize :: BSL.ByteString -> Byte
byteStringSize = fromBytes . fromIntegral . length

spec :: Spec
spec = describe "Pos.Binary.Class.Primitive" $ do
    it "encodedKnownCborDataItemSize" $ forAll word64Gen $ \a ->
        encodedKnownCborDataItemSize a === fromIntegral (length $ toLazyByteString $ encodeKnownCborDataItem a)
    it "encodedUnknownCborDataItemSize" $ forAll arbitrary $ \(ABS a) ->
        let len = fromIntegral $ length a
        in
                encodedSize (toStrictByteString $ encodeUnknownCborDataItem (BSL.fromStrict a))
            === withSize len 1 2 3 5 9 + encodedUnknownCborDataItemSize (fromBytes len)
    it "encodedCrcProtectedSize" $ forAll word64Gen $ \a ->
        greaterEqual
            (encodedCrcProtectedSize a)
            (byteStringSize (toLazyByteString (encodeCrcProtected a)))
