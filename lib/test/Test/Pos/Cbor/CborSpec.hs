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

import qualified Cardano.Crypto.Wallet as CC
import           Crypto.Hash (Blake2b_224, Blake2b_256)
import           Data.Bits (shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Fixed (Nano)
import           Data.Tagged (Tagged)
import           Data.Time.Units (Microsecond, Millisecond)
import           Serokell.Data.Memory.Units (Byte)
import           System.FileLock (FileLock)
import           Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess, prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import qualified Codec.CBOR.FlatTerm as CBOR

import           Pos.Arbitrary.Block ()
import           Pos.Arbitrary.Block.Message ()
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
import qualified Pos.Block.Network as BT
import qualified Pos.Block.Types as BT
import qualified Pos.Communication as C
import qualified Pos.Communication.Relay as R
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Communication.Limits (mlOpening, mlVssCertificate, mlUpdateVote)
import qualified Pos.Core as T
import qualified Pos.Core.Block as BT
import           Pos.Core.Common (ScriptVersion)
import qualified Pos.Core.Ssc as Ssc
import qualified Pos.Crypto as Crypto
import           Pos.Crypto.Hashing (WithHash)
import           Pos.Crypto.Signing (EncryptedSecretKey)
import           Pos.Data.Attributes (Attributes (..), decodeAttributes, encodeAttributes)
import           Pos.Delegation (DlgPayload, DlgUndo)
import qualified Pos.DHT.Model as DHT
import           Pos.Merkle (MerkleTree)
import           Pos.Slotting.Types (SlottingData)
import qualified Pos.Ssc as Ssc
import qualified Pos.Txp as T
import qualified Pos.Update as U
import           Pos.Util (SmallGenerator)
import           Pos.Util.Chrono (NE, NewestFirst, OldestFirst)
import           Pos.Util.QuickCheck.Property (expectationError)
import           Pos.Util.UserSecret (UserSecret, WalletUserSecret)
import           Test.Pos.Crypto.Arbitrary ()
import qualified Test.Pos.Cbor.RefImpl as R
import           Test.Pos.Configuration (withDefConfiguration)
import           Test.Pos.Helpers (binaryTest, msgLenLimitedTest)

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
type VoteId' = Tagged U.UpdateVote U.VoteId
type UpId' = Tagged (U.UpdateProposal, [U.UpdateVote])U.UpId

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
    encode (U word8 bs) = encodeListLen 2 <> encode (word8 :: Word8) <> encodeUnknownCborDataItem (LBS.fromStrict bs)
    decode = do
        decodeListLenCanonicalOf 2
        U <$> decode <*> decodeUnknownCborDataItem

instance Arbitrary U where
    arbitrary = U <$> choose (0, 255) <*> arbitrary

-- | Like `U`, but we expect to read back the Cbor Data Item when decoding.
data U24 = U24 Word8 BS.ByteString deriving (Show, Eq)

instance Bi U24 where
    encode (U24 word8 bs) = encodeListLen 2 <> encode (word8 :: Word8) <> encodeUnknownCborDataItem (LBS.fromStrict bs)
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
    encode = encodeAttributes [(0, serialize . x1A)]
    decode = decodeAttributes (X1 0) $ \n v acc -> case n of
        0 -> pure $ Just $ acc { x1A = unsafeDeserialize v }
        _ -> pure $ Nothing

instance Bi (Attributes X2) where
    encode = encodeAttributes [(0, serialize . x2A), (1, serialize . x2B)]
    decode = decodeAttributes (X2 0 []) $ \n v acc -> case n of
        0 -> return $ Just $ acc { x2A = unsafeDeserialize v }
        1 -> return $ Just $ acc { x2B = unsafeDeserialize v }
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

    describe "Cbor.Bi instances" $ do
        modifyMaxSuccess (const 1000) $ do
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
                brokenDisabled $ binaryTest @UserSecret
                modifyMaxSuccess (min 50) $ do
                    binaryTest @WalletUserSecret
                    binaryTest @EncryptedSecretKey
                binaryTest @(WithHash ARecord)

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

        describe "Types" $ do
          -- 100 is not enough to catch some bugs (e.g. there was a bug with
          -- addresses that only manifested when address's CRC started with 0x00)
          describe "Bi instances" $ do
              describe "Core.Address" $ do
                  binaryTest @T.Address
                  binaryTest @T.Address'
                  binaryTest @T.AddrType
                  binaryTest @T.AddrStakeDistribution
                  binaryTest @T.AddrSpendingData
              describe "Core.Types" $ do
                  binaryTest @T.Timestamp
                  binaryTest @T.TimeDiff
                  binaryTest @T.EpochIndex
                  binaryTest @T.Coin
                  binaryTest @T.CoinPortion
                  binaryTest @T.LocalSlotIndex
                  binaryTest @T.SlotId
                  binaryTest @T.EpochOrSlot
                  binaryTest @T.SharedSeed
                  binaryTest @T.ChainDifficulty
                  binaryTest @T.SoftforkRule
                  binaryTest @T.BlockVersionData
                  binaryTest @(Attributes ())
                  binaryTest @(Attributes T.AddrAttributes)
              describe "Core.Fee" $ do
                  binaryTest @T.Coeff
                  binaryTest @T.TxSizeLinear
                  binaryTest @T.TxFeePolicy
              describe "Core.Script" $ do
                  binaryTest @T.Script
              describe "Core.Vss" $ do
                  binaryTest @T.VssCertificate
              describe "Core.Version" $ do
                  binaryTest @T.ApplicationName
                  binaryTest @T.SoftwareVersion
                  binaryTest @T.BlockVersion
              describe "Util" $ do
                  binaryTest @(NewestFirst NE U)
                  binaryTest @(OldestFirst NE U)
          describe "Message length limit" $ do
              msgLenLimitedTest @T.VssCertificate mlVssCertificate
        describe "Block types" $ do
            describe "Bi instances" $ do
                describe "Undo" $ do
                    binaryTest @BT.SlogUndo
                    modifyMaxSuccess (min 50) $ do
                        binaryTest @BT.Undo
                describe "Block network types" $ modifyMaxSuccess (min 10) $ do
                    binaryTest @BT.MsgGetHeaders
                    binaryTest @BT.MsgGetBlocks
                    binaryTest @BT.MsgHeaders
                    binaryTest @BT.MsgBlock
                describe "Blockchains and blockheaders" $ do
                    modifyMaxSuccess (min 10) $ describe "GenericBlockHeader" $ do
                        describe "GenesisBlockHeader" $ do
                            binaryTest @BT.GenesisBlockHeader
                        describe "MainBlockHeader" $ do
                            binaryTest @BT.MainBlockHeader
                    describe "GenesisBlockchain" $ do
                        describe "BodyProof" $ do
                            binaryTest @BT.GenesisExtraHeaderData
                            binaryTest @BT.GenesisExtraBodyData
                            binaryTest @(BT.BodyProof BT.GenesisBlockchain)
                        describe "ConsensusData" $ do
                            binaryTest @(BT.ConsensusData BT.GenesisBlockchain)
                        describe "Body" $ do
                            binaryTest @(BT.Body BT.GenesisBlockchain)
                    describe "MainBlockchain" $ do
                        describe "BodyProof" $ do
                            binaryTest @(BT.BodyProof BT.MainBlockchain)
                        describe "BlockSignature" $ do
                            binaryTest @BT.BlockSignature
                        describe "ConsensusData" $ do
                            binaryTest @(BT.ConsensusData BT.MainBlockchain)
                        modifyMaxSuccess (min 10) $ describe "Body" $ do
                            binaryTest @(BT.Body BT.MainBlockchain)
                        describe "MainToSign" $ do
                            binaryTest @BT.MainToSign
                        describe "Extra data" $ do
                            binaryTest @BT.MainExtraHeaderData
                            binaryTest @BT.MainExtraBodyData
        describe "Communication" $ do
            describe "Bi instances" $ do
                binaryTest @C.HandlerSpec
                binaryTest @C.VerInfo
                binaryTest @C.MessageCode
            describe "Bi extension" $ do
                prop "HandlerSpec" (extensionProperty @C.HandlerSpec)
        describe "Merkle" $ do
            binaryTest @(MerkleTree Int32)
        describe "Crypto" $ do
            describe "Hashing" $ do
                binaryTest @(Crypto.Hash Word64)
            describe "Signing" $ do
                describe "Bi instances" $ do
                    binaryTest @Crypto.SecretKey
                    binaryTest @Crypto.PublicKey
                    binaryTest @(Crypto.Signature ())
                    binaryTest @(Crypto.Signature U)
                    binaryTest @(Crypto.ProxyCert Int32)
                    binaryTest @(Crypto.ProxySecretKey Int32)
                    binaryTest @(Crypto.ProxySecretKey U)
                    binaryTest @(Crypto.ProxySignature Int32 Int32)
                    binaryTest @(Crypto.ProxySignature U U)
                    binaryTest @(Crypto.Signed Bool)
                    binaryTest @(Crypto.Signed U)
                    binaryTest @Crypto.RedeemSecretKey
                    binaryTest @Crypto.RedeemPublicKey
                    binaryTest @(Crypto.RedeemSignature Bool)
                    binaryTest @(Crypto.RedeemSignature U)
                    binaryTest @Crypto.Threshold
                    binaryTest @Crypto.VssPublicKey
                    binaryTest @Crypto.PassPhrase
                    binaryTest @Crypto.VssKeyPair
                    binaryTest @Crypto.Secret
                    binaryTest @Crypto.DecShare
                    binaryTest @Crypto.EncShare
                    binaryTest @Crypto.SecretProof
                    binaryTest @Crypto.HDAddressPayload
                    binaryTest @(Crypto.AbstractHash Blake2b_224 U)
                    binaryTest @(Crypto.AbstractHash Blake2b_256 U)
                    binaryTest @(AsBinary Crypto.VssPublicKey)
                    binaryTest @(AsBinary Crypto.Secret)
                    binaryTest @(AsBinary Crypto.DecShare)
                    binaryTest @(AsBinary Crypto.EncShare)
        describe "DHT.Model" $ do
            describe "Bi instances" $ do
                binaryTest @DHT.DHTKey
                binaryTest @DHT.DHTData
        describe "Delegation types" $ do
            describe "Bi instances" $ do
                binaryTest @DlgPayload
                binaryTest @DlgUndo
            describe "Network" $ do
                binaryTest @(DataMsg T.ProxySKHeavy)
        describe "Slotting types" $ do
            binaryTest @SlottingData
        describe "Ssc" $ do
            describe "Bi instances" $ do
                binaryTest @Ssc.Commitment
                binaryTest @Ssc.CommitmentsMap
                binaryTest @Ssc.Opening
                modifyMaxSuccess (min 10) $ do
                    binaryTest @Ssc.SscPayload
                    binaryTest @Ssc.TossModifier
                    binaryTest @Ssc.VssCertData
                    binaryTest @Ssc.SscGlobalState
                binaryTest @Ssc.SscProof
                binaryTest @(R.InvMsg (Tagged Ssc.MCCommitment T.StakeholderId))
                binaryTest @(R.ReqMsg (Tagged Ssc.MCCommitment T.StakeholderId))
                binaryTest @(R.MempoolMsg Ssc.MCCommitment)
                binaryTest @(R.DataMsg Ssc.MCCommitment)
                binaryTest @(R.DataMsg Ssc.MCOpening)
                binaryTest @(R.DataMsg Ssc.MCShares)
                binaryTest @(R.DataMsg Ssc.MCVssCertificate)
                binaryTest @Ssc.SscTag
                binaryTest @Ssc.SscSecretStorage
            describe "Message length limit" $ do
                msgLenLimitedTest @Ssc.Opening mlOpening
                msgLenLimitedTest @(R.InvMsg (Tagged Ssc.MCCommitment T.StakeholderId)) C.mlInvMsg
                msgLenLimitedTest @(R.ReqMsg (Tagged Ssc.MCCommitment T.StakeholderId)) C.mlReqMsg
                msgLenLimitedTest @(R.MempoolMsg Ssc.MCCommitment) C.mlMempoolMsg
                -- msgLenLimitedTest' @(C.MaxSize (R.DataMsg Ssc.MCCommitment))
                --     (C.MaxSize . R.DataMsg <$> C.mcCommitmentMsgLenLimit)
                --     "MCCommitment"
                --     (has Ssc._MCCommitment . R.dmContents . C.getOfMaxSize)
                -- msgLenLimitedTest' @(R.DataMsg Ssc.MCOpening)
                --     (R.DataMsg <$> C.mcOpeningLenLimit)
                --     "MCOpening"
                --     (has Ssc._MCOpening . R.dmContents)
                -- msgLenLimitedTest' @(C.MaxSize (R.DataMsg Ssc.MCShares))
                --     (C.MaxSize . R.DataMsg <$> C.mcSharesMsgLenLimit)
                --     "MCShares"
                --     (has Ssc._MCShares . R.dmContents . C.getOfMaxSize)
                -- msgLenLimitedTest' @(R.DataMsg Ssc.MCVssCertificate)
                --     (R.DataMsg <$> C.mcVssCertificateLenLimit)
                --     "MCVssCertificate"
                --     (has Ssc._MCVssCertificate . R.dmContents)
        describe "Txp (transaction processing) system" $ do
            describe "Bi instances" $ do
                describe "Core" $ do
                    binaryTest @T.TxIn
                    binaryTest @T.TxOut
                    binaryTest @T.TxOutAux
                    binaryTest @T.Tx
                    binaryTest @T.TxInWitness
                    binaryTest @T.TxSigData
                    binaryTest @T.TxAux
                    binaryTest @T.TxProof
                    binaryTest @(SmallGenerator T.TxPayload)
                    binaryTest @T.TxpUndo
                describe "Network" $ do
                    binaryTest @(R.InvMsg (Tagged T.TxMsgContents T.TxId))
                    binaryTest @(R.ReqMsg (Tagged T.TxMsgContents T.TxId))
                    binaryTest @(R.MempoolMsg T.TxMsgContents)
                    binaryTest @(R.DataMsg T.TxMsgContents)
            describe "Bi extension" $ do
                prop "TxInWitness" (extensionProperty @T.TxInWitness)
            describe "Message length limit" $ do
                msgLenLimitedTest @(R.InvMsg (Tagged T.TxMsgContents T.TxId)) C.mlInvMsg
                msgLenLimitedTest @(R.ReqMsg (Tagged T.TxMsgContents T.TxId)) C.mlReqMsg
                msgLenLimitedTest @(R.MempoolMsg T.TxMsgContents) C.mlMempoolMsg
                -- No check for (DataMsg T.TxMsgContents) since overal message size
                -- is forcely limited
        describe "Update system" $ do
            describe "Bi instances" $ do
                describe "Core" $ do
                    binaryTest @U.BlockVersionModifier
                    binaryTest @U.SystemTag
                    binaryTest @U.UpdateVote
                    binaryTest @U.UpdateData
                    binaryTest @U.UpdateProposal
                    binaryTest @U.UpdateProposalToSign
                    binaryTest @U.UpdatePayload
                    binaryTest @U.VoteState
                    binaryTest @U.UpId
                describe "Poll" $ do
                    binaryTest @(U.PrevValue ())
                    binaryTest @(U.PrevValue U)
                    binaryTest @U.USUndo
                    binaryTest @U.UpsExtra
                    binaryTest @U.DpsExtra
                    binaryTest @U.UndecidedProposalState
                    binaryTest @U.DecidedProposalState
                    binaryTest @U.ProposalState
                    binaryTest @U.ConfirmedProposalState
                    binaryTest @U.BlockVersionState
                describe "Network" $ do
                    binaryTest @(R.InvMsg VoteId')
                    binaryTest @(R.ReqMsg VoteId')
                    binaryTest @(R.MempoolMsg U.UpdateVote)
                    binaryTest @(R.DataMsg U.UpdateVote)
                    binaryTest @(R.InvMsg UpId')
                    binaryTest @(R.ReqMsg UpId')
                    binaryTest @(R.MempoolMsg (U.UpdateProposal, [U.UpdateVote]))
                    binaryTest @(R.DataMsg (U.UpdateProposal, [U.UpdateVote]))
                describe "Message length limit" $ do
                    msgLenLimitedTest @(R.InvMsg VoteId') C.mlInvMsg
                    msgLenLimitedTest @(R.ReqMsg VoteId') C.mlReqMsg
                    msgLenLimitedTest @(R.MempoolMsg U.UpdateVote) C.mlMempoolMsg
                    msgLenLimitedTest @(R.InvMsg UpId') C.mlInvMsg
                    msgLenLimitedTest @(R.ReqMsg UpId') C.mlReqMsg
                    msgLenLimitedTest @(R.MempoolMsg (U.UpdateProposal, [U.UpdateVote])) C.mlMempoolMsg
                    -- TODO [CSL-859]
                    -- msgLenLimitedTest @(C.MaxSize (R.DataMsg (U.UpdateProposal, [U.UpdateVote])))
                    msgLenLimitedTest @(R.DataMsg U.UpdateVote) (C.mlDataMsg mlUpdateVote)
                    -- msgLenLimitedTest @U.UpdateProposal

instance {-# OVERLAPPING #-} Arbitrary (Maybe FileLock) where
    arbitrary = pure Nothing

-- | This instance is unsafe, as it allows a timing attack. But it's OK for
-- tests.
instance Eq CC.XPrv where
    (==) = (==) `on` CC.unXPrv

-- | Mark a test case as broken. The intended use is for tests that are
-- themselves valid, but the code they're testing turned out to be buggy.
brokenDisabled :: Monad m => m a -> m ()
brokenDisabled _ = return ()
