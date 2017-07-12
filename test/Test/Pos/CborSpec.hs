
-- | Pos.Crypto specification

module Test.Pos.CborSpec
       ( spec
       ) where

import qualified Codec.CBOR.FlatTerm as CBOR
import           Pos.Binary.Cbor
import           Pos.Binary.Class.Numbers
import           Pos.Binary.Core.Fee ()
import           Pos.Binary.Core.Script ()
import           Pos.Core.Arbitrary ()
import           Pos.Core.Fee
import           Pos.Core.Genesis.Types
import           Pos.Core.Types
import           Test.QuickCheck
import           Universum

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- Machinery to test we perform "flat" encoding.
hasValidFlatTerm :: Bi a => a -> Bool
hasValidFlatTerm = CBOR.validFlatTerm . CBOR.toFlatTerm . encode

-- | Given a data type which can be generated randomly and for which the CBOR
-- encoding is defined, generates the roundtrip tests.
roundtripProperty :: (Arbitrary a, Eq a, Show a, Bi a) => a -> Property
roundtripProperty (input :: a) = ((deserialize . serialize $ input) :: a) === input

-- | Given a data type which can be extended, verify we can indeed do so
-- without breaking anything. This should work with every time which adopted
-- the schema of having at least one constructor of the form:
-- .... | Unknown Word8 ByteString
extensionProperty :: (Arbitrary a, Eq a, Show a, Bi a) => Proxy a -> Property
extensionProperty (Proxy :: Proxy a) = forAll (arbitrary :: Gen a) $ \input ->
  let serialized      = serialize input -- We now have a BS blob
      (u :: U)        = deserialize serialized
      (encoded :: a)  = deserialize (serialize u)
  in encoded === input

soundInstanceProperty :: (Arbitrary a, Eq a, Show a, Bi a) => Proxy a -> Property
soundInstanceProperty (Proxy :: Proxy a) = forAll (arbitrary :: Gen a) $ \input ->
  let itRoundtrips = roundtripProperty input
      isFlat       = hasValidFlatTerm input === True
  in itRoundtrips .&&. isFlat

spec = describe "Cbor Specs" $ do
         prop "Cbor.Bi" (soundInstanceProperty @(UnsignedVarInt Int) Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @(SignedVarInt Int) Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @(FixedSizeInt Int) Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @(UnsignedVarInt Int64) Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @(SignedVarInt Int64) Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @(FixedSizeInt Int64) Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @(UnsignedVarInt Word) Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @(FixedSizeInt Word) Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @(UnsignedVarInt Word16) Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @(UnsignedVarInt Word32) Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @(UnsignedVarInt Word64) Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @TinyVarInt Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @Int64 Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @MyScript Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @Coeff Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @TxSizeLinear Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @TxFeePolicy Proxy .&&. extensionProperty @TxFeePolicy Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @Script Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @Timestamp Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @EpochIndex Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @Coin Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @CoinPortion Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @LocalSlotIndex Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @SlotId Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @EpochOrSlot Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @SharedSeed Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @ChainDifficulty Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @StakeDistribution Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @ApplicationName Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @SoftwareVersion Proxy)
         prop "Cbor.Bi" (soundInstanceProperty @BlockVersion Proxy)
