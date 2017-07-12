{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pos.Binary.Cbor.Test where

import qualified Codec.CBOR.FlatTerm as CBOR
import           Pos.Binary.Cbor

import           Universum
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
import           Pos.Core.Fee
import           Pos.Binary.Core.Fee()
import           Pos.Core.Arbitrary()
import           Pos.Binary.Core.Script()
import           Pos.Core.Types
import           Pos.Core.Genesis.Types
import           Pos.Data.Attributes

import qualified Data.ByteString as BS
import qualified Data.Map as M

data MyScript = MyScript
    { version :: ScriptVersion -- ^ Version
    , script  :: LByteString   -- ^ Serialized script
    } deriving (Eq, Show, Generic, Typeable)

instance Arbitrary MyScript where
  arbitrary = MyScript <$> arbitrary <*> arbitrary

deriveSimpleBi ''MyScript [
    Cons 'MyScript [
        Field [| version :: ScriptVersion |],
        Field [| script  :: LByteString   |]
    ]]

-- Type to be used to simulate a breaking change in the serialisation
-- schema, so we can test instances which uses the `UnknownXX` pattern
-- for extensibility.
data U = U Word8 BS.ByteString

instance Bi U where
  encode (U word8 bs) = encodeListLen 2 <> encode (word8 :: Word8) <> encode bs
  decode = do
    decodeListLenOf 2
    U <$> decode <*> decode

----------------------------------------

data X1 = X1 { x1A :: Int }
    deriving (Eq, Ord, Show, Generic)

data X2 = X2 { x2A :: Int, x2B :: String }
    deriving (Eq, Ord, Show, Generic)

instance Arbitrary X1 where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary X2 where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Bi (Attributes X1) where
    encode = encodeAttributes [(0, serialize' . x1A)]
    decode = decodeAttributes (X1 0) $ \n v acc -> case n of
        0 -> Just $ acc { x1A = deserialize' v }
        _ -> Nothing

instance Bi (Attributes X2) where
    encode = encodeAttributes [(0, serialize' . x2A), (1, serialize' . x2B)]
    decode = decodeAttributes (X2 0 []) $ \n v acc -> case n of
        0 -> Just $ acc { x2A = deserialize' v }
        1 -> Just $ acc { x2B = deserialize' v }
        _ -> Nothing

----------------------------------------

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

soundSerializationAttributesOfAsProperty
    :: forall a b aa ab. (aa ~ Attributes a, ab ~ Attributes b,
                          Bi aa, Bi ab, Eq aa, Arbitrary a, Show aa)
    => Proxy a
    -> Proxy b
    -> Property
soundSerializationAttributesOfAsProperty _ _ = forAll arbitraryAttrs $ \input ->
    let serialized      = serialize input
        (middle  :: ab) = deserialize serialized
        (encoded :: aa) = deserialize $ serialize middle
    in encoded === input
    where
      arbitraryAttrs :: Gen aa
      arbitraryAttrs = Attributes <$> arbitrary <*> arbitraryUnparsedFields

-- TODO: Use as a main Arbitrary instance for UnparsedFields after transition to
-- CBOR.
arbitraryUnparsedFields :: Gen UnparsedFields
arbitraryUnparsedFields = sized $ go M.empty
  where
    go !acc 0 = pure $ UnparsedFields acc
    go !acc n = do
        -- Assume that data type doesn't have more than 100 constructors.
        k <- choose (100, maxBound)
        v <- arbitrary
        go (M.insert k v acc) (n - 1)

soundInstanceProperty :: (Arbitrary a, Eq a, Show a, Bi a) => Proxy a -> Property
soundInstanceProperty (Proxy :: Proxy a) = forAll (arbitrary :: Gen a) $ \input ->
  let itRoundtrips = roundtripProperty input
      isFlat       = hasValidFlatTerm input === True
  in itRoundtrips .&&. isFlat

-- Override the `Args` to be a bit more exhaustive.
qc :: Property -> IO ()
qc = quickCheckWith (stdArgs { maxSuccess = 1000 })

-- | A set of basic yet-useful roundtrips properties to be included as part
-- of a bigger testsuite.
soundInstancesTest :: IO ()
soundInstancesTest = do
  qc (soundInstanceProperty @MyScript Proxy)
  qc (soundInstanceProperty @Coeff Proxy)
  qc (soundInstanceProperty @TxSizeLinear Proxy)
  qc (soundInstanceProperty @TxFeePolicy Proxy .&&. extensionProperty @TxFeePolicy Proxy)
  qc (soundInstanceProperty @Script Proxy)
  qc (soundInstanceProperty @Timestamp Proxy)
  qc (soundInstanceProperty @EpochIndex Proxy)
  qc (soundInstanceProperty @(Attributes ()) Proxy)
  qc (soundInstanceProperty @Coin Proxy)
  qc (soundInstanceProperty @CoinPortion Proxy)
  qc (soundInstanceProperty @LocalSlotIndex Proxy)
  qc (soundInstanceProperty @SlotId Proxy)
  qc (soundInstanceProperty @EpochOrSlot Proxy)
  qc (soundInstanceProperty @SharedSeed Proxy)
  qc (soundInstanceProperty @ChainDifficulty Proxy)
  qc (soundInstanceProperty @StakeDistribution Proxy)
  qc (soundInstanceProperty @ApplicationName Proxy)
  qc (soundInstanceProperty @SoftwareVersion Proxy)
  qc (soundInstanceProperty @BlockVersion Proxy)
  qc (soundInstanceProperty @(Attributes X1) Proxy)
  qc (soundInstanceProperty @(Attributes X2) Proxy)
  qc (soundSerializationAttributesOfAsProperty @X2 @X1 Proxy Proxy)

----------------------------------------

data User
    = Login {
      login :: String
    , age   :: Int
    }
    | FullName {
      firstName  :: String
    , lastName   :: String
    , sex        :: Bool
    } deriving Show

deriveSimpleBi ''User [
    Cons 'Login [
        Field [| login :: String |],
        Field [| age   :: Int    |]
    ],
    Cons 'FullName [
        Field [| firstName :: String |],
        Field [| lastName  :: String |],
        Field [| sex       :: Bool   |]
    ]]

u1 :: User
u1 = deserialize $ serialize $ Login "asd" 34

----------------------------------------

data T = T1 Int | T2 Int Int | Unknown Word8 BS.ByteString
    deriving Show

instance Bi T where
    encode = \case
        T1 a         -> encode (0::Word8)
                     <> encode (serialize' a)
        T2 a b       -> encode (1::Word8)
                     <> encode (serialize' (a, b))
        Unknown n bs -> encode n
                     <> encode bs

    decode = decode @Word8 >>= \case
        0 ->         T1 . deserialize' <$> decode
        1 -> uncurry T2 . deserialize' <$> decode
        t -> Unknown t                 <$> decode
