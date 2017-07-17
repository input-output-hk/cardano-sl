
-- | Test.Pos.CborSpec specification

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pos.CborSpec
       ( spec
       ) where


import qualified Codec.CBOR.FlatTerm as CBOR
import           Node.Message.Class
import           Pos.Binary.Class
import           Pos.Binary.Communication ()
import           Pos.Binary.Core.Fee ()
import           Pos.Binary.Core.Script ()
import           Pos.Binary.Crypto ()
import           Pos.Binary.GodTossing ()
import           Pos.Binary.Infra ()
import           Pos.Binary.Relay ()
import           Pos.Block.Arbitrary ()
import           Pos.Block.Core
import           Pos.Communication.Protocol
import           Pos.Communication.Types.Relay (DataMsg)
import           Pos.Core.Arbitrary ()
import           Pos.Core.Fee
import           Pos.Core.Genesis.Types
import           Pos.Core.Types
import           Pos.Crypto.HD (HDAddressPayload)
import           Pos.Crypto.RedeemSigning (RedeemPublicKey, RedeemSecretKey)
import           Pos.Crypto.SafeSigning (PassPhrase)
import           Pos.Crypto.SecretSharing (VssPublicKey, VssKeyPair, Secret, Share,
                                           EncShare, SecretProof, SecretSharingExtra)
import           Pos.Crypto.Signing (PublicKey, SecretKey)
import           Pos.DHT.Model.Types
import           Pos.Delegation.Arbitrary ()
import           Pos.Delegation.Types
import           Pos.Explorer
import           Pos.Explorer.Arbitrary ()
import           Pos.Infra.Arbitrary ()
import           Pos.Slotting.Arbitrary ()
import           Pos.Slotting.Types
import           Pos.Ssc.GodTossing
import           Pos.Ssc.GodTossing.Arbitrary ()
import           Pos.Txp hiding (Unknown)
import           Pos.Update.Arbitrary ()
import           Pos.Update.Core
import           Pos.Update.Poll
import           Pos.Util.Chrono
import           Test.Hspec (Spec, describe, it, pendingWith)
import           Test.QuickCheck
import           Universum

import           Test.Hspec.QuickCheck (prop, modifyMaxSuccess)

import           Pos.Data.Attributes
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import qualified Data.ByteString as BS
import qualified Data.Map as M

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

asBinaryIdempotencyProperty :: (Arbitrary a, AsBinaryClass a, Eq a, Show a) => Proxy a -> Property
asBinaryIdempotencyProperty (Proxy :: Proxy a) = forAll (arbitrary :: Gen a) $ \input ->
  (fromBinary . asBinary $ input) === Right input

spec :: Spec
spec = describe "Cbor.Bi instances" $ do
    modifyMaxSuccess (const 50) $ do
      describe "Test instances are sound" $ do
        prop "User" (let u1 = Login "asd" 34 in (deserialize $ serialize u1) === u1)
        prop "MyScript" (soundInstanceProperty @MyScript Proxy)
        prop "X2" (soundSerializationAttributesOfAsProperty @X2 @X1 Proxy Proxy)
      describe "Primitive instances are sound" $ do
        prop "Int64" (soundInstanceProperty @Int64 Proxy)
      describe "Plutus Types' instances are sound" $ do
        prop "Script" (soundInstanceProperty @Script Proxy)
      describe "Core instances are sound" $ do
        prop "UnsignedVarInt" (soundInstanceProperty @(UnsignedVarInt Int) Proxy)
        prop "SignedVarInt" (soundInstanceProperty @(SignedVarInt Int) Proxy)
        prop "FixedSizeInt" (soundInstanceProperty @(FixedSizeInt Int) Proxy)
        prop "UnsignedVarInt" (soundInstanceProperty @(UnsignedVarInt Int64) Proxy)
        prop "SignedVarInt" (soundInstanceProperty @(SignedVarInt Int64) Proxy)
        prop "FixedSizeInt" (soundInstanceProperty @(FixedSizeInt Int64) Proxy)
        prop "UnsignedVarInt" (soundInstanceProperty @(UnsignedVarInt Word) Proxy)
        prop "FixedSizeInt" (soundInstanceProperty @(FixedSizeInt Word) Proxy)
        prop "UnsignedVarInt" (soundInstanceProperty @(UnsignedVarInt Word16) Proxy)
        prop "UnsignedVarInt" (soundInstanceProperty @(UnsignedVarInt Word32) Proxy)
        prop "UnsignedVarInt" (soundInstanceProperty @(UnsignedVarInt Word64) Proxy)
        prop "TinyVarInt" (soundInstanceProperty @TinyVarInt Proxy)
        prop "Coeff" (soundInstanceProperty @Coeff Proxy)
        prop "TxSizeLinear" (soundInstanceProperty @TxSizeLinear Proxy)
        prop "TxFeePolicy" (soundInstanceProperty @TxFeePolicy Proxy .&&. extensionProperty @TxFeePolicy Proxy)
        prop "Timestamp" (soundInstanceProperty @Timestamp Proxy)
        prop "EpochIndex" (soundInstanceProperty @EpochIndex Proxy)
        prop "Attributes" (soundInstanceProperty @(Attributes ()) Proxy)
        prop "Address" (soundInstanceProperty @Address Proxy)
        prop "Coin" (soundInstanceProperty @Coin Proxy)
        prop "CoinPortion" (soundInstanceProperty @CoinPortion Proxy)
        prop "LocalSlotIndex" (soundInstanceProperty @LocalSlotIndex Proxy)
        prop "SlotId" (soundInstanceProperty @SlotId Proxy)
        prop "EpochOrSlot" (soundInstanceProperty @EpochOrSlot Proxy)
        prop "SharedSeed" (soundInstanceProperty @SharedSeed Proxy)
        prop "ChainDifficulty" (soundInstanceProperty @ChainDifficulty Proxy)
        prop "StakeDistribution" (soundInstanceProperty @StakeDistribution Proxy)
        prop "GenesisCoreData" (soundInstanceProperty @GenesisCoreData Proxy)
        prop "ApplicationName" (soundInstanceProperty @ApplicationName Proxy)
        prop "SoftwareVersion" (soundInstanceProperty @SoftwareVersion Proxy)
        prop "BlockVersion" (soundInstanceProperty @BlockVersion Proxy)
        prop "Attributes X1" (soundInstanceProperty @(Attributes X1) Proxy)
        prop "Attributes X2" (soundInstanceProperty @(Attributes X2) Proxy)
        prop "AbstractHash " (soundInstanceProperty @(Attributes X2) Proxy)
        prop "VssPublicKey" (soundInstanceProperty @VssPublicKey Proxy)
        prop "VssKeyPair" (soundInstanceProperty @VssKeyPair Proxy)
        prop "Secret" (soundInstanceProperty @Secret Proxy)
        prop "Share" (soundInstanceProperty @Share Proxy)
        prop "EncShare" (soundInstanceProperty @EncShare Proxy)
        prop "SecretSharingExtra" (soundInstanceProperty @SecretSharingExtra Proxy)
        prop "SecretProof" (soundInstanceProperty @SecretProof Proxy)
        prop "AsBinary VssPublicKey" (    soundInstanceProperty @(AsBinary VssPublicKey) Proxy
                                     .&&. asBinaryIdempotencyProperty @VssPublicKey Proxy
                                     )
        prop "AsBinary Secret" (    soundInstanceProperty @Secret Proxy
                               .&&. asBinaryIdempotencyProperty @Secret Proxy
                               )
        prop "AsBinary Share" (soundInstanceProperty @(AsBinary Share) Proxy)
        prop "AsBinary EncShare" (soundInstanceProperty @(AsBinary EncShare) Proxy)
        prop "AsBinary SecretProof" (soundInstanceProperty @(AsBinary SecretProof) Proxy)
        prop "SecretSharingExtra"   (soundInstanceProperty @SecretSharingExtra Proxy)
        prop "CC.ChainCode" (soundInstanceProperty @(AsBinary SecretProof) Proxy)
        prop "PublicKey" (soundInstanceProperty @PublicKey Proxy)
        prop "SecretKey" (soundInstanceProperty @SecretKey Proxy)
        prop "PassPhrase" (soundInstanceProperty @PassPhrase Proxy)
        prop "HDAddressPayload" (soundInstanceProperty @HDAddressPayload Proxy)
        prop "RedeemPublicKey" (soundInstanceProperty @RedeemPublicKey Proxy)
        prop "RedeemSecretKey" (soundInstanceProperty @RedeemSecretKey Proxy)
        prop "Commitment" (soundInstanceProperty @Commitment Proxy)
        prop "CommitmentsMap" (soundInstanceProperty @CommitmentsMap Proxy)
        prop "VssCertificate" (soundInstanceProperty @VssCertificate Proxy)
        prop "Opening" (soundInstanceProperty @Opening Proxy)
        prop "GtPayload" (soundInstanceProperty @GtPayload Proxy)
        prop "GtProof" (soundInstanceProperty @GtProof Proxy)
        prop "DataMsg MCCommitment" (soundInstanceProperty @(DataMsg MCCommitment) Proxy)
        prop "DataMsg MCOpening" (soundInstanceProperty @(DataMsg MCOpening) Proxy)
        modifyMaxSuccess (const 50) $ prop "DataMsg MCShares" (soundInstanceProperty @(DataMsg MCShares) Proxy)
        prop "DataMsg MCVssCertificate" (soundInstanceProperty @(DataMsg MCVssCertificate) Proxy)
        prop "DHTKey" (soundInstanceProperty @DHTKey Proxy)
        prop "DHTData" (soundInstanceProperty @DHTData Proxy)
        prop "MessageName" (soundInstanceProperty @MessageName Proxy)
        prop "HandlerSpec" (soundInstanceProperty @HandlerSpec Proxy .&&. extensionProperty @HandlerSpec Proxy)
        prop "VerInfo" (soundInstanceProperty @VerInfo Proxy)
        prop "DlgPayload" (soundInstanceProperty @DlgPayload Proxy)
        prop "EpochSlottingData" (soundInstanceProperty @EpochSlottingData Proxy)
        prop "SlottingData" (soundInstanceProperty @SlottingData Proxy)
        prop "SystemTag" (soundInstanceProperty @SystemTag Proxy)
        prop "UpdateVote" (soundInstanceProperty @UpdateVote Proxy)
        prop "UpdateData" (soundInstanceProperty @UpdateData Proxy)
        prop "BlockVersionModifier" (soundInstanceProperty @BlockVersionModifier Proxy)
        prop "UpdateProposal" (soundInstanceProperty @UpdateProposal Proxy)
        prop "UpdateProposalToSign" (soundInstanceProperty @UpdateProposalToSign Proxy)
        prop "UpdatePayload" (soundInstanceProperty @UpdatePayload Proxy)
        prop "VoteState" (soundInstanceProperty @VoteState Proxy)
        modifyMaxSuccess (const 50) $ prop "USUndo" (soundInstanceProperty @USUndo Proxy)
        prop "UpsExtra" (soundInstanceProperty @UpsExtra Proxy)
        prop "DpsExtra" (soundInstanceProperty @DpsExtra Proxy)
        prop "UndecidedProposalState" (soundInstanceProperty @UndecidedProposalState Proxy)
        prop "DecidedProposalState" (soundInstanceProperty @DecidedProposalState Proxy)
        prop "ProposalState" (soundInstanceProperty @ProposalState Proxy)
        prop "ConfirmedProposalState" (soundInstanceProperty @ConfirmedProposalState Proxy)
        prop "TxIn" (soundInstanceProperty @TxIn Proxy)
        prop "TxDistribution" (soundInstanceProperty @TxDistribution Proxy)
        prop "TxSigData" (soundInstanceProperty @TxSigData Proxy)
        prop "TxProof" (soundInstanceProperty @TxProof Proxy)
        prop "MainExtraHeaderData" (soundInstanceProperty @MainExtraHeaderData Proxy)
        prop "MainExtraBodyData" (soundInstanceProperty @MainExtraBodyData Proxy)
        prop "GenesisExtraHeaderData" (soundInstanceProperty @GenesisExtraHeaderData Proxy)
        prop "GenesisExtraBodyData" (soundInstanceProperty @GenesisExtraBodyData Proxy)
        prop "GtTag" (soundInstanceProperty @GtTag Proxy)
        prop "TossModifier" (soundInstanceProperty @TossModifier Proxy)
        prop "VssCertData" (soundInstanceProperty @VssCertData Proxy)
        prop "GtGlobalState" (soundInstanceProperty @GtGlobalState Proxy)
        prop "GtSecretStorage" (soundInstanceProperty @GtSecretStorage Proxy)
        prop "GenesisGtData" (soundInstanceProperty @GenesisGtData Proxy)
        prop "NewestFirst" (soundInstanceProperty @(NewestFirst NE U) Proxy)
        prop "OldestFirst" (soundInstanceProperty @(OldestFirst NE U) Proxy)
        prop "TxExtra" (soundInstanceProperty @TxExtra Proxy)
        -- This runs extremely slow. For now the quickest
        -- course of action is to decrease the number of tests performed, but it's probably
        -- a hot trail for performance testing.
        modifyMaxSuccess (const 10) $
          prop "TxPayload" (soundInstanceProperty @TxPayload Proxy)
        prop "TxAux" (soundInstanceProperty @TxAux Proxy)
        prop "Tx" (soundInstanceProperty @Tx Proxy)
        prop "TxOutAux" (soundInstanceProperty @TxOutAux Proxy)
        prop "TxOut" (soundInstanceProperty @TxOut Proxy)
        prop "DataMsg TxMsgContents" (soundInstanceProperty @(DataMsg TxMsgContents) Proxy)
        prop "TxInWitness" (soundInstanceProperty @TxInWitness Proxy .&&. extensionProperty @TxInWitness Proxy)
        -- Pending specs
        it "(Signature a)"        $ pendingWith "Arbitrary instance requires Bi (not Cbor.Bi) constraint"
        it "(Signed a)"           $ pendingWith "Arbitrary instance requires Bi (not Cbor.Bi) constraint"
        it "(RedeemSignature a)"  $ pendingWith "Arbitrary instance requires Bi (not Cbor.Bi) constraint"
        it "(ProxySecretKey w)"   $ pendingWith "Arbitrary instance requires Bi (not Cbor.Bi) constraint"
        it "(ProxySignature w a)" $ pendingWith "Arbitrary instance requires Bi (not Cbor.Bi) constraint"
        it "AbstractHash SHA256"  $ pendingWith "Arbitrary instance requires Bi (not Cbor.Bi) constraint"
        it "DataMsg ProxySKLight" $ pendingWith "Failing"
        it "DataMsg ProxySKHeavy" $ pendingWith "Failing"
        it "DataMsg ProxySKLightConfirmation" $ pendingWith "Failing"
        it "UserSecret" $ pendingWith "No Eq instance defined"
        it "WalletUserSecret" $ pendingWith "No Eq instance defined"
        -- Pending specs which doesn't have an `Arbitrary` instance defined.
        pendingNoArbitrary "Undo"
        pendingNoArbitrary "BackupPhrase"
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
