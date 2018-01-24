{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TemplateHaskell           #-}

-- | Test.Pos.Cbor.CborSpec specification

module Test.Pos.Cbor.CborSpec
       ( spec
       , U
       , extensionProperty
       ) where

import           Universum

import qualified Data.ByteString as BS
import           Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, pendingWith, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess, prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import qualified Codec.CBOR.FlatTerm as CBOR

import           Pos.Arbitrary.Block ()
import           Pos.Arbitrary.Core ()
import           Pos.Arbitrary.Delegation ()
import           Pos.Arbitrary.Infra ()
import           Pos.Arbitrary.Slotting ()
import           Pos.Arbitrary.Ssc ()
import           Pos.Arbitrary.Update ()
import           Pos.Binary.Class
import           Pos.Binary.Communication ()
import           Pos.Binary.Core ()
import           Pos.Binary.Crypto ()
import           Pos.Binary.Infra ()
import           Pos.Binary.Ssc ()
import           Pos.Core.Common (ScriptVersion)
import           Pos.Data.Attributes (Attributes (..), decodeAttributes, encodeAttributes)
import qualified Test.Pos.Cbor.RefImpl as R
import           Test.Pos.Helpers (binaryTest)
import           Test.Pos.Util (withDefConfiguration, expectationError)

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

data MyScript = MyScript
    { version :: ScriptVersion -- ^ Version
    , script  :: ByteString    -- ^ Serialized script
    } deriving (Eq, Show, Generic, Typeable)

instance Arbitrary MyScript where
    arbitrary = MyScript <$> arbitrary <*> arbitrary

deriveSimpleBi ''MyScript [
    Cons 'MyScript [
        Field [| version :: ScriptVersion |],
        Field [| script  :: ByteString   |]
    ]]

-- Type to be used to simulate a breaking change in the serialisation
-- schema, so we can test instances which uses the `UnknownXX` pattern
-- for extensibility.
-- Check the `extensionProperty` for more details.
data U = U Word8 BS.ByteString deriving (Show, Eq)

instance Bi U where
    encode (U word8 bs) = encodeListLen 2 <> encode (word8 :: Word8) <> encodeUnknownCborDataItem bs
    decode = do
        decodeListLenCanonicalOf 2
        U <$> decode <*> decodeUnknownCborDataItem

instance Arbitrary U where
    arbitrary = U <$> choose (0, 255) <*> arbitrary

-- | Like `U`, but we expect to read back the Cbor Data Item when decoding.
data U24 = U24 Word8 BS.ByteString deriving (Show, Eq)

instance Bi U24 where
    encode (U24 word8 bs) = encodeListLen 2 <> encode (word8 :: Word8) <> encodeUnknownCborDataItem bs
    decode = do
        decodeListLenCanonicalOf 2
        U24 <$> decode <*> decodeUnknownCborDataItem

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
        0 -> pure $ Just $ acc { x1A = unsafeDeserialize' v }
        _ -> pure $ Nothing

instance Bi (Attributes X2) where
    encode = encodeAttributes [(0, serialize' . x2A), (1, serialize' . x2B)]
    decode = decodeAttributes (X2 0 []) $ \n v acc -> case n of
        0 -> return $ Just $ acc { x2A = unsafeDeserialize' v }
        1 -> return $ Just $ acc { x2B = unsafeDeserialize' v }
        _ -> return $ Nothing

----------------------------------------

-- | Given a data type which can be extended, verify we can indeed do so
-- without breaking anything. This should work with every time which adopted
-- the schema of having at least one constructor of the form:
-- .... | Unknown Word8 ByteString
extensionProperty :: forall a. (Arbitrary a, Eq a, Show a, Bi a) => Property
extensionProperty = forAll @a (arbitrary :: Gen a) $ \input ->
{- This function works as follows:

   1. When we call `serialized`, we are implicitly assuming (as contract of this
      function) that the input type would be of a shape such as:

      data MyType = Constructor1 Int Bool
                  | Constructor2 String
                  | UnknownConstructor Word8 ByteString

      Such type will be encoded, roughly, like this:

      encode (Constructor1 a b) = encodeWord 0 <> encodeKnownCborDataItem (a,b)
      encode (Constructor2 a b) = encodeWord 1 <> encodeKnownCborDataItem a
      encode (UnknownConstructor tag bs) = encodeWord tag <> encodeUnknownCborDataItem bs

      In CBOR terms, we would produce something like this:

      <tag :: Word32><Tag24><CborDataItem :: ByteString>

   2. Now, when we call `unsafeDeserialize serialized`, we are effectively asking to produce as
      output a value of type `U`. `U` is defined by only 1 constructor, it
      being `U Word8 ByteString`, but this is still compatible with our `tag + cborDataItem`
      format. So now we will have something like:

      U <tag :: Word32> <CborDataItem :: ByteString>

      (The <Tag24> has been removed as part of the decoding process).

   3. We now call `unsafeDeserialize (serialize u)`, which means: Can you produce a CBOR binary
      from `U`, and finally try to decode it into a value of type `a`? This will work because
      our intermediate encoding into `U` didn't touch the inital `<tag :: Word32>`, so we will
      be able to reconstruct the original object back.
      More specifically, `serialize u` would produce once again:

      <tag :: Word32><Tag24><CborDataItem :: ByteString>

      (The <Tag24> has been added as part of the encoding process).

      `unsafeDeserialize` would then consume the tag (to understand which type constructor this corresponds to),
      remove the <Tag24> token and finally proceed to deserialise the rest.

-}
    let serialized      = serialize input             -- Step 1
        (u :: U)        = unsafeDeserialize serialized      -- Step 2
        (encoded :: a)  = unsafeDeserialize (serialize u)   -- Step 3
    in encoded === input

soundSerializationAttributesOfAsProperty
    :: forall a b aa ab. (aa ~ Attributes a, ab ~ Attributes b,
                          Bi aa, Bi ab, Eq aa, Arbitrary a, Show aa)
    => Property
soundSerializationAttributesOfAsProperty = forAll arbitraryAttrs $ \input ->
    let serialized      = serialize input
        (middle  :: ab) = unsafeDeserialize serialized
        (encoded :: aa) = unsafeDeserialize $ serialize middle
    in encoded === input
  where
    arbitraryAttrs :: Gen aa
    arbitraryAttrs = Attributes <$> arbitrary <*> arbitrary


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
spec = withDefConfiguration $ do
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

    describe "Cbor.Bi instances" $ modifyMaxSuccess (const 1000) $ do
        describe "Test instances" $ do
            prop "User" (let u1 = Login "asd" 34 in (unsafeDeserialize $ serialize u1) === u1)
            binaryTest @MyScript
            prop "X2" (soundSerializationAttributesOfAsProperty @X2 @X1)
        describe "Generic deriving" $ do
            testARecord
            testAUnit
            testANewtype
            binaryTest @ARecord
            binaryTest @AUnit
            binaryTest @ANewtype
        describe "Lib/core instances" $ do
            binaryTest @(Attributes X1)
            binaryTest @(Attributes X2)

            -- Pending specs which doesn't have an `Arbitrary` or `Eq` instance defined.
            it "UserSecret" $ pendingWith "No Eq instance defined"
            it "WalletUserSecret" $ pendingWith "No Eq instance defined"
            pendingNoArbitrary "Undo"
            pendingNoArbitrary "DataMsg (UpdateProposal, [UpdateVote])"
            pendingNoArbitrary "DataMsg UpdateVote"
            pendingNoArbitrary "MsgGetHeaders"
            pendingNoArbitrary "MsgGetBlocks"
            pendingNoArbitrary "WithHash"
            pendingNoArbitrary "Pvss.PublicKey"
            pendingNoArbitrary "Pvss.KeyPair"
            pendingNoArbitrary "Pvss.Secret"
            pendingNoArbitrary "Pvss.DecryptedShare"
            pendingNoArbitrary "Pvss.EncryptedShare"
            pendingNoArbitrary "Pvss.Proof"
            pendingNoArbitrary "Ed25519.PointCompressed"
            pendingNoArbitrary "Ed25519.Scalar"
            pendingNoArbitrary "Ed25519.Signature"
            pendingNoArbitrary "CC.ChainCode"
            pendingNoArbitrary "CC.XPub"
            pendingNoArbitrary "CC.XPrv"
            pendingNoArbitrary "CC.XSignature"
            pendingNoArbitrary "EdStandard.PublicKey"
            pendingNoArbitrary "EdStandard.SecretKey"
            pendingNoArbitrary "EdStandard.Signature"
            pendingNoArbitrary "EncryptedSecretKey"

pendingNoArbitrary :: String -> Spec
pendingNoArbitrary ty = it ty $ pendingWith "Arbitrary instance required"
