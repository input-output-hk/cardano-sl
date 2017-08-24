
-- | Test.Pos.CborSpec specification

{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TemplateHaskell           #-}

module Test.Pos.CborSpec
       ( spec
       ) where

import           Universum

import qualified Data.ByteString                   as BS
import           Test.Hspec                        (Arg, Expectation, Spec, SpecWith,
                                                    describe, it, pendingWith, shouldBe)
import           Test.Hspec.QuickCheck             (modifyMaxSuccess, prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import qualified Codec.CBOR.FlatTerm               as CBOR
import           Crypto.Hash.Algorithms            (SHA256)
import           Node.Message.Class

import           Pos.Arbitrary.Block               ()
import           Pos.Arbitrary.Core                ()
import           Pos.Arbitrary.Delegation          ()
import           Pos.Arbitrary.Explorer            ()
import           Pos.Arbitrary.Infra               ()
import           Pos.Arbitrary.Slotting            ()
import           Pos.Arbitrary.Ssc.GodTossing      ()
import           Pos.Arbitrary.Update              ()
import           Pos.Binary.Class
import           Pos.Binary.Communication          ()
import           Pos.Binary.Core.Fee               ()
import           Pos.Binary.Core.Script            ()
import           Pos.Binary.Crypto                 ()
import           Pos.Binary.GodTossing             ()
import           Pos.Binary.Infra                  ()
import           Pos.Binary.Relay                  ()
import           Pos.Block.Core
import           Pos.Communication.Protocol
import           Pos.Communication.Types.Relay     (DataMsg)
import           Pos.Core.Context                  (giveStaticConsts)
import           Pos.Core.Fee
import           Pos.Core.Genesis.Types
import           Pos.Core.Types
import           Pos.Crypto                        (AbstractHash)
import           Pos.Crypto.HD                     (HDAddressPayload)
import           Pos.Crypto.RedeemSigning          (RedeemPublicKey, RedeemSecretKey,
                                                    RedeemSignature)
import           Pos.Crypto.SafeSigning            (PassPhrase)
import           Pos.Crypto.SecretSharing          (EncShare, Secret, SecretProof,
                                                    SecretSharingExtra, Share, VssKeyPair,
                                                    VssPublicKey)
import           Pos.Crypto.Signing                (ProxySecretKey, ProxySignature,
                                                    PublicKey, SecretKey, Signature,
                                                    Signed)
import           Pos.Data.Attributes
import           Pos.Delegation.Types
import           Pos.DHT.Model.Types
import           Pos.Explorer
import           Pos.Slotting.Types
import           Pos.Ssc.GodTossing
import           Pos.Txp                           hiding (Unknown)
import           Pos.Update.Core
import           Pos.Update.Poll
import           Pos.Util.BackupPhrase
import           Pos.Util.Chrono

----------------------------------------

data User
    = Login {
      login :: String
    , age   :: Int
    }
    | FullName {
      firstName :: String
    , lastName  :: String
    , sex       :: Bool
    } deriving (Show, Eq)

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
        0 ->         T1 . deserialize' <$> decode
        1 -> uncurry T2 . deserialize' <$> decode
        t -> Unknown t                 <$> decode

data MyScript = MyScript
    { version :: ScriptVersion -- ^ Version
    , script  :: ByteString   -- ^ Serialized script
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
data U = U Word8 BS.ByteString deriving (Show, Eq)

instance Bi U where
    encode (U word8 bs) = encodeListLen 2 <> encode (word8 :: Word8) <> encode bs
    decode = do
        decodeListLenOf 2
        U <$> decode <*> decode

instance Arbitrary U where
    arbitrary = U <$> choose (0, 255) <*> arbitrary

-- | Like `U`, but we expect to read back the Cbor Data Item when decoding.
data U24 = U24 Word8 BS.ByteString deriving (Show, Eq)

instance Bi U24 where
    encode (U24 word8 bs) = encodeListLen 2 <> encode (word8 :: Word8) <> encodeUnknownCborDataItem bs
    decode = do
        decodeListLenOf 2
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
extensionPropertyOn :: forall a b. (Bi a, Arbitrary b, Eq b, Show b, Bi b)
                    => Property
extensionPropertyOn = forAll @b (arbitrary :: Gen b) $ \input ->
    let serialized      = serialize input -- We now have a BS blob
        (u :: a)        = deserialize serialized
        (encoded :: b)  = deserialize (serialize u)
    in encoded === input

soundSerializationAttributesOfAsProperty
    :: forall a b aa ab. (aa ~ Attributes a, ab ~ Attributes b,
                          Bi aa, Bi ab, Eq aa, Arbitrary a, Show aa)
    => Property
soundSerializationAttributesOfAsProperty = forAll arbitraryAttrs $ \input ->
    let serialized      = serialize input
        (middle  :: ab) = deserialize serialized
        (encoded :: aa) = deserialize $ serialize middle
    in encoded === input
  where
    arbitraryAttrs :: Gen aa
    arbitraryAttrs = Attributes <$> arbitrary <*> arbitrary

soundInstanceProperty :: forall a. (Arbitrary a, Eq a, Show a, Bi a) => Property
soundInstanceProperty = forAll (arbitrary :: Gen a) $ \input ->
    let itRoundtrips = roundtripProperty input
        isFlat       = hasValidFlatTerm input === True
    in itRoundtrips .&&. isFlat

asBinaryIdempotencyProperty ::
       forall a. (Arbitrary a, AsBinaryClass a, Eq a, Show a)
    => Property
asBinaryIdempotencyProperty = forAll (arbitrary :: Gen a) $ \input ->
    (fromBinary . asBinary $ input) === Right input

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
              Left err     -> fail err
              Right actual -> x `shouldBe` actual

spec :: Spec
spec = giveStaticConsts $ describe "Cbor.Bi instances" $ do
    modifyMaxSuccess (const 1000) $ do
        describe "(Hash)Map and (Hash)Set instances are sound" $ do
            prop "HashMap Int Int" (soundInstanceProperty @(HashMap Int Int))
            prop "HashSet Int" (soundInstanceProperty @(HashSet Int))
            prop "Map Int Int" (soundInstanceProperty @(Map Int Int))
            prop "Set Int" (soundInstanceProperty @(Set Int))
        describe "Test instances are sound" $ do
            prop "User" (let u1 = Login "asd" 34 in (deserialize $ serialize u1) === u1)
            prop "MyScript" (soundInstanceProperty @MyScript)
            prop "X2" (soundSerializationAttributesOfAsProperty @X2 @X1)
            describe "Generic deriving is sound" $ do
                testARecord
                testAUnit
                testANewtype
                prop "ARecord"  (soundInstanceProperty @ARecord)
                prop "AUnit"    (soundInstanceProperty @AUnit)
                prop "ANewtype" (soundInstanceProperty @ANewtype)
            modifyMaxSuccess (const 20000) $ do
                describe "Primitive instances are sound" $ do
                    prop "Int64" (soundInstanceProperty @Int64)
                    prop "IntMap" (soundInstanceProperty @(Map Int Int))
                    prop "IntHashMap" (soundInstanceProperty @(HashMap Int Int))
                    prop "IntSet" (soundInstanceProperty @(Set Int))
                    prop "IntHashSet" (soundInstanceProperty @(HashSet Int))
            describe "Plutus Types' instances are sound" $ do
                prop "Script" (soundInstanceProperty @Script)
            describe "Core instances are sound" $ do
                prop "UnsignedVarInt" (soundInstanceProperty @(UnsignedVarInt Int))
                prop "SignedVarInt" (soundInstanceProperty @(SignedVarInt Int))
                prop "FixedSizeInt" (soundInstanceProperty @(FixedSizeInt Int))
                prop "UnsignedVarInt" (soundInstanceProperty @(UnsignedVarInt Int64))
                prop "SignedVarInt" (soundInstanceProperty @(SignedVarInt Int64))
                prop "FixedSizeInt" (soundInstanceProperty @(FixedSizeInt Int64))
                prop "UnsignedVarInt" (soundInstanceProperty @(UnsignedVarInt Word))
                prop "FixedSizeInt" (soundInstanceProperty @(FixedSizeInt Word))
                prop "UnsignedVarInt" (soundInstanceProperty @(UnsignedVarInt Word16))
                prop "UnsignedVarInt" (soundInstanceProperty @(UnsignedVarInt Word32))
                prop "UnsignedVarInt" (soundInstanceProperty @(UnsignedVarInt Word64))
                prop "TinyVarInt" (soundInstanceProperty @TinyVarInt)
                prop "Coeff" (soundInstanceProperty @Coeff)
                prop "TxSizeLinear" (soundInstanceProperty @TxSizeLinear)
                prop "TxFeePolicy" (soundInstanceProperty @TxFeePolicy .&&. extensionPropertyOn @U @TxFeePolicy)
                prop "Timestamp" (soundInstanceProperty @Timestamp)
                prop "TimeDiff"  (soundInstanceProperty @TimeDiff)
                prop "EpochIndex" (soundInstanceProperty @EpochIndex)
                prop "Attributes" (soundInstanceProperty @(Attributes ()))
                prop "AddrType" (soundInstanceProperty @AddrType)
                prop "AddrStakeDistribution" (soundInstanceProperty @AddrStakeDistribution)
                prop "AddrSpendingData" (soundInstanceProperty @AddrSpendingData)
                prop "Attributes AddrAttributes" (soundInstanceProperty @(Attributes AddrAttributes))
                prop "Address'" (soundInstanceProperty @Address')
                prop "Address" (soundInstanceProperty @Address)
                prop "Coin" (soundInstanceProperty @Coin)
                prop "CoinPortion" (soundInstanceProperty @CoinPortion)
                prop "LocalSlotIndex" (soundInstanceProperty @LocalSlotIndex)
                prop "SlotId" (soundInstanceProperty @SlotId)
                prop "EpochOrSlot" (soundInstanceProperty @EpochOrSlot)
                prop "SharedSeed" (soundInstanceProperty @SharedSeed)
                prop "ChainDifficulty" (soundInstanceProperty @ChainDifficulty)
                prop "StakeDistribution" (soundInstanceProperty @StakeDistribution)
                prop "GenesisCoreData" (soundInstanceProperty @GenesisCoreData)
                prop "ApplicationName" (soundInstanceProperty @ApplicationName)
                prop "SoftwareVersion" (soundInstanceProperty @SoftwareVersion)
                prop "BlockVersion" (soundInstanceProperty @BlockVersion)
                prop "Attributes X1" (soundInstanceProperty @(Attributes X1))
                prop "Attributes X2" (soundInstanceProperty @(Attributes X2))
                prop "AbstractHash " (soundInstanceProperty @(Attributes X2))
                prop "VssPublicKey" (soundInstanceProperty @VssPublicKey)
                prop "VssKeyPair" (soundInstanceProperty @VssKeyPair)
                prop "Secret" (soundInstanceProperty @Secret)
                prop "Share" (soundInstanceProperty @Share)
                prop "EncShare" (soundInstanceProperty @EncShare)
                prop "SecretSharingExtra" (soundInstanceProperty @SecretSharingExtra)
                prop "SecretProof" (soundInstanceProperty @SecretProof)
                prop "AsBinary VssPublicKey" (    soundInstanceProperty @(AsBinary VssPublicKey)
                                                 .&&. asBinaryIdempotencyProperty @VssPublicKey
                                             )
                prop "AsBinary Secret" (    soundInstanceProperty @Secret
                                           .&&. asBinaryIdempotencyProperty @Secret
                                       )
                prop "AsBinary Share" (soundInstanceProperty @(AsBinary Share))
                prop "AsBinary EncShare" (soundInstanceProperty @(AsBinary EncShare))
                prop "AsBinary SecretProof" (soundInstanceProperty @(AsBinary SecretProof))
                prop "SecretSharingExtra"   (soundInstanceProperty @SecretSharingExtra)
                prop "CC.ChainCode" (soundInstanceProperty @(AsBinary SecretProof))
                prop "PublicKey" (soundInstanceProperty @PublicKey)
                prop "SecretKey" (soundInstanceProperty @SecretKey)
                prop "PassPhrase" (soundInstanceProperty @PassPhrase)
                prop "HDAddressPayload" (soundInstanceProperty @HDAddressPayload)
                prop "RedeemPublicKey" (soundInstanceProperty @RedeemPublicKey)
                prop "RedeemSecretKey" (soundInstanceProperty @RedeemSecretKey)
                prop "Commitment" (soundInstanceProperty @Commitment)
                prop "CommitmentsMap" (soundInstanceProperty @CommitmentsMap)
                prop "VssCertificate" (soundInstanceProperty @VssCertificate)
                prop "Opening" (soundInstanceProperty @Opening)
                prop "GtPayload" (soundInstanceProperty @GtPayload)
                prop "GtProof" (soundInstanceProperty @GtProof)
                prop "DataMsg MCCommitment" (soundInstanceProperty @(DataMsg MCCommitment))
                prop "DataMsg MCOpening" (soundInstanceProperty @(DataMsg MCOpening))
                modifyMaxSuccess (const 50) $ prop "DataMsg MCShares" (soundInstanceProperty @(DataMsg MCShares))
                prop "DataMsg MCVssCertificate" (soundInstanceProperty @(DataMsg MCVssCertificate))
                prop "DHTKey" (soundInstanceProperty @DHTKey)
                prop "DHTData" (soundInstanceProperty @DHTData)
                prop "MessageCode" (soundInstanceProperty @MessageCode)
                prop "HandlerSpec" (soundInstanceProperty @HandlerSpec .&&. extensionPropertyOn @U @HandlerSpec)
                prop "VerInfo" (soundInstanceProperty @VerInfo)
                prop "DlgPayload" (soundInstanceProperty @DlgPayload)
                prop "EpochSlottingData" (soundInstanceProperty @EpochSlottingData)
                prop "SlottingData" (soundInstanceProperty @SlottingData)
                prop "SystemTag" (soundInstanceProperty @SystemTag)
                prop "UpdateVote" (soundInstanceProperty @UpdateVote)
                prop "UpdateData" (soundInstanceProperty @UpdateData)
                prop "BlockVersionModifier" (soundInstanceProperty @BlockVersionModifier)
                prop "UpdateProposal" (soundInstanceProperty @UpdateProposal)
                prop "UpdateProposalToSign" (soundInstanceProperty @UpdateProposalToSign)
                prop "UpdatePayload" (soundInstanceProperty @UpdatePayload)
                prop "VoteState" (soundInstanceProperty @VoteState)
                modifyMaxSuccess (const 50) $ prop "USUndo" (soundInstanceProperty @USUndo)
                prop "UpsExtra" (soundInstanceProperty @UpsExtra)
                prop "DpsExtra" (soundInstanceProperty @DpsExtra)
                prop "UndecidedProposalState" (soundInstanceProperty @UndecidedProposalState)
                prop "DecidedProposalState" (soundInstanceProperty @DecidedProposalState)
                prop "ProposalState" (soundInstanceProperty @ProposalState)
                prop "ConfirmedProposalState" (soundInstanceProperty @ConfirmedProposalState)
                prop "TxIn" (soundInstanceProperty @TxIn .&&. extensionPropertyOn @U24 @TxIn)
                modifyMaxSuccess (const 100) $
                    prop "TxDistribution" (soundInstanceProperty @TxDistribution)
                prop "TxSigData" (soundInstanceProperty @TxSigData)
                prop "TxProof" (soundInstanceProperty @TxProof)
                prop "MainExtraHeaderData" (soundInstanceProperty @MainExtraHeaderData)
                prop "MainExtraBodyData" (soundInstanceProperty @MainExtraBodyData)
                prop "GenesisExtraHeaderData" (soundInstanceProperty @GenesisExtraHeaderData)
                prop "GenesisExtraBodyData" (soundInstanceProperty @GenesisExtraBodyData)
                prop "GtTag" (soundInstanceProperty @GtTag)
                prop "TossModifier" (soundInstanceProperty @TossModifier)
                prop "VssCertData" (soundInstanceProperty @VssCertData)
                prop "GtGlobalState" (soundInstanceProperty @GtGlobalState)
                prop "GtSecretStorage" (soundInstanceProperty @GtSecretStorage)
                prop "GenesisGtData" (soundInstanceProperty @GenesisGtData)
                prop "NewestFirst" (soundInstanceProperty @(NewestFirst NE U))
                prop "OldestFirst" (soundInstanceProperty @(OldestFirst NE U))
                modifyMaxSuccess (const 100) $
                    prop "TxExtra" (soundInstanceProperty @TxExtra)
                -- This runs extremely slow. For now the quickest course of action is to decrease the number of tests performed.
                modifyMaxSuccess (const 10) $
                    prop "TxPayload" (soundInstanceProperty @TxPayload)
                modifyMaxSuccess (const 100) $
                    prop "TxAux" (soundInstanceProperty @TxAux)
                prop "Tx" (soundInstanceProperty @Tx)
                prop "TxOutAux" (soundInstanceProperty @TxOutAux)
                prop "TxOut" (soundInstanceProperty @TxOut)
                modifyMaxSuccess (const 100) $
                    prop "DataMsg TxMsgContents" (soundInstanceProperty @(DataMsg TxMsgContents))
                prop "TxInWitness" (soundInstanceProperty @TxInWitness .&&. extensionPropertyOn @U @TxInWitness)
                prop "Signature a" (soundInstanceProperty @(Signature U))
                prop "Signed a"    (soundInstanceProperty @(Signed U))
                prop "RedeemSignature a"    (soundInstanceProperty @(RedeemSignature U))
                prop "ProxySecretKey w"     (soundInstanceProperty @(ProxySecretKey U))
                prop "ProxySignature w a"   (soundInstanceProperty @(ProxySignature U U))
                prop "AbstractHash SHA256"  (soundInstanceProperty @(AbstractHash SHA256 U))
                prop "DataMsgSKLight" (soundInstanceProperty @(DataMsg ProxySKLight))
                prop "DataMsgSKHeavy" (soundInstanceProperty @(DataMsg ProxySKHeavy))
                prop "DataMsgSKLightConfirmation" (soundInstanceProperty @(DataMsg ProxySKLightConfirmation))
                prop "BackupPhrase" (soundInstanceProperty @BackupPhrase)
                prop "PrevValue a"  (soundInstanceProperty @(PrevValue U))
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
                pendingNoArbitrary "AsBinary VssKeyPair"
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
