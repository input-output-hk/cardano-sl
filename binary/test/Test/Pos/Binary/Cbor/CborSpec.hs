{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TemplateHaskell           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Binary.Cbor.CborSpec
       ( spec
       , U
       , extensionProperty
       ) where

import           Universum

import           Data.Bits (shiftL)
import qualified Data.ByteString as BS
import           Data.Fixed (Nano)
import           Data.Time.Units (Microsecond, Millisecond)
import           Serokell.Data.Memory.Units (Byte)
import           Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), choose, oneof, sized, (===))
import           Test.QuickCheck.Arbitrary.Generic (genericShrink)
import           Test.QuickCheck.Instances ()

import qualified Codec.CBOR.FlatTerm as CBOR

import           Pos.Binary.Class
import           Test.Pos.Binary.Helpers (U, binaryTest, extensionProperty)
import qualified Test.Pos.Cbor.RefImpl as R
import           Test.Pos.Util.Orphans ()
import           Test.Pos.Util.QuickCheck.Property (expectationError)

-- | Wrapper for Integer with Arbitrary instance that can generate "proper" big
-- integers, i.e. ones that don't fit in Int64. This really needs to be fixed
-- within QuickCheck though (https://github.com/nick8325/quickcheck/issues/213).
newtype LargeInteger = LargeInteger Integer
    deriving (Eq, Show)

instance Arbitrary LargeInteger where
    arbitrary = sized $ \sz -> do
        n <- choose (1, sz)
        sign <- arbitrary
        LargeInteger . (if sign then negate else identity) . foldr f 0
            <$> replicateM n arbitrary
      where
        f :: Word8 -> Integer -> Integer
        f w acc = (acc `shiftL` 8) + fromIntegral w

instance Bi LargeInteger where
    encode (LargeInteger n) = encode n
    decode = LargeInteger <$> decode

----------------------------------------

data User
    = Login { login :: String
            , age   :: Int }
    | FullName { firstName :: String
               , lastName  :: String
               , sex       :: Bool }
    deriving (Show, Eq)

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

----------------------------------------
data ARecord = ARecord String Int ARecord
             | ANull
             deriving (Generic, Eq, Show)

instance Bi ARecord where
    encode = genericEncode
    decode = genericDecode

instance Arbitrary ARecord where
    arbitrary = oneof [
          ARecord <$> arbitrary <*> arbitrary <*> arbitrary
        , pure ANull
        ]
    shrink = genericShrink

data AUnit = AUnit
           deriving (Generic, Eq, Show)

instance Bi AUnit where
    encode = genericEncode
    decode = genericDecode

instance Arbitrary AUnit where
    arbitrary = pure AUnit
    shrink = genericShrink

newtype ANewtype = ANewtype Int
                 deriving (Generic, Eq, Show)

instance Bi ANewtype where
    encode = genericEncode
    decode = genericDecode

instance Arbitrary ANewtype where
    arbitrary = ANewtype <$> arbitrary
    shrink = genericShrink

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
        0 ->         T1 <$> (deserialize' =<< decode)
        1 -> uncurry T2 <$> (deserialize' =<< decode)
        t -> Unknown t  <$> decode

----------------------------------------

testANewtype :: SpecWith ()
testANewtype = testAgainstFile "a newtype" x rep
  where
    x :: ANewtype
    x = ANewtype 42

    rep :: [CBOR.TermToken]
    rep = [CBOR.TkListLen 1, CBOR.TkInt 42]

testAUnit :: SpecWith ()
testAUnit = testAgainstFile "a unit" x rep
  where
    x :: AUnit
    x = AUnit

    rep :: [CBOR.TermToken]
    rep = [CBOR.TkListLen 0]

testARecord :: SpecWith ()
testARecord = testAgainstFile "a record" x rep
  where
    x :: ARecord
    x = ARecord "hello" 42 (ARecord "world" 52 ANull)

    rep :: [CBOR.TermToken]
    rep = [CBOR.TkListLen 4, CBOR.TkInt 0, CBOR.TkString "hello", CBOR.TkInt 42,
           CBOR.TkListLen 4, CBOR.TkInt 0, CBOR.TkString "world", CBOR.TkInt 52,
           CBOR.TkListLen 1, CBOR.TkInt 1
          ]

testAgainstFile
    :: (Eq a, Show a, Bi a)
    => String
    -> a
    -> CBOR.FlatTerm
    -> SpecWith (Arg Expectation)
testAgainstFile name x expected =
    describe name $ do
      it "serialise" $ do
            let actual = CBOR.toFlatTerm $ encode x
            expected `shouldBe` actual
      it "deserialise" $ do
            case CBOR.fromFlatTerm decode expected of
              Left err     -> expectationError (fromString err)
              Right actual -> x `shouldBe` actual

spec :: Spec
spec = do
    describe "Cbor.Bi instances" $
        modifyMaxSuccess (const 1000) $
            prop "User" (let u1 = Login "asd" 34 in (unsafeDeserialize $ serialize u1) === u1)

    describe "Reference implementation" $ do
        describe "properties" $ do
            prop "encoding/decoding initial byte"    R.prop_InitialByte
            prop "encoding/decoding additional info" R.prop_AdditionalInfo
            prop "encoding/decoding token header"    R.prop_TokenHeader
            prop "encoding/decoding token header 2"  R.prop_TokenHeader2
            prop "encoding/decoding tokens"          R.prop_Token
            modifyMaxSuccess (const 1000) . modifyMaxSize (const 150) $ do
                prop "encoding/decoding terms"       R.prop_Term
        describe "internal properties" $ do
            prop "Integer to/from bytes"             R.prop_integerToFromBytes
            prop "Word16 to/from network byte order" R.prop_word16ToFromNet
            prop "Word32 to/from network byte order" R.prop_word32ToFromNet
            prop "Word64 to/from network byte order" R.prop_word64ToFromNet
            modifyMaxSuccess (const 1) $ do
                -- Using once inside the property would be lovely (as it tests
                -- all the Halfs) but it doesn't work for some reason.
                prop "Numeric.Half to/from Float"    R.prop_halfToFromFloat

    describe "Cbor.Bi instances" $ do
        modifyMaxSuccess (const 1000) $ do
            describe "Generic deriving" $ do
                testARecord
                testAUnit
                testANewtype
                binaryTest @ARecord
                binaryTest @AUnit
                binaryTest @ANewtype

            describe "Primitive instances" $ do
                binaryTest @()
                binaryTest @Bool
                binaryTest @Char
                binaryTest @Integer
                binaryTest @LargeInteger
                binaryTest @Word
                binaryTest @Word8
                binaryTest @Word16
                binaryTest @Word32
                binaryTest @Word64
                binaryTest @Int
                binaryTest @Float
                binaryTest @Int32
                binaryTest @Int64
                binaryTest @Nano
                binaryTest @Millisecond
                binaryTest @Microsecond
                binaryTest @Byte
                binaryTest @(Map Int Int)
                binaryTest @(HashMap Int Int)
                binaryTest @(Set Int)
                binaryTest @(HashSet Int)
                binaryTest @ByteString
                binaryTest @Text
