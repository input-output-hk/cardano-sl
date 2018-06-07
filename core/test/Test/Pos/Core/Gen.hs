module Test.Pos.Core.Gen
       (
        -- Pos.Core.Block Generators
          genGenesisHash
        , genGenesisHeader

        -- Pos.Core.Common Generators
        , genAddrAttributes
        , genAddress
        , genAddrType
        , genAddrSpendingData
        , genAddrStakeDistribution
        , genCoeff
        , genCoin
        , genCoinPortion
        , genScript
        , genScriptVersion
        , genSlotLeaders
        , genStakeholderId
        , genTxFeePolicy
        , genTxSizeLinear

        -- Pos.Core.Delegation Generators
        , genDlgPayload
        , genHeavyDlgIndex
        , genProxySKBlockInfo
        , genProxySKHeavy

        -- Pos.Core.Slotting Generators
        , genEpochIndex
        , genFlatSlotId
        , genLocalSlotIndex
        , genSlotId
        , genSscPayload

        -- Pos.Core.Ssc Generators
        , genCommitment
        , genCommitmentsMap
        , genCommitmentSignature
        , genOpening
        , genSignedCommitment
        , genVssCertificate
        , genVssCertificatesMap

        -- Pos.Core.Txp Generators
        , genPkWitness
        , genRedeemWitness
        , genScriptWitness
        , genTx
        , genTxAttributes
        , genTxHash
        , genTxId
        , genTxIn
        , genTxIndex
        , genTxInList
        , genTxInWitness
        , genTxOut
        , genTxOutList
        , genTxPayload
        , genTxSig
        , genTxSigData
        , genTxWitness
        , genUnknownWitnessType

        -- Pos.Core.Update Generators
        , genBlockVersion
        , genBlockVersionModifier
        , genHashRaw
        , genSoftforkRule
        , genSoftwareVersion
        , genSystemTag
        , genUpAttributes
        , genUpdateData
        , genUpdateProposal
        , genUpdateProposalToSign
        , genUpsData
       ) where

import           Universum

import           Data.Coerce (coerce)
import           Data.Fixed (Fixed (..))
import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty (fromList)
import           Data.Maybe
import           Data.Time.Units (fromMicroseconds, Millisecond)
import           Data.Vector (singleton)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Pos.Binary.Class (asBinary, Raw (..))
import           Pos.Block.Base (mkGenesisHeader)
import           Pos.Core.Block (GenesisBlockHeader, GenesisBody (..))
import           Pos.Core.Common (Address (..), AddrAttributes (..),
                                  AddrSpendingData (..), AddrStakeDistribution (..),
                                  AddrType (..), Coeff (..), Coin (..),
                                  CoinPortion (..), makeAddress, Script (..),
                                  ScriptVersion, SlotLeaders, StakeholderId,
                                  TxFeePolicy (..), TxSizeLinear (..))
import           Pos.Core.Configuration (GenesisHash (..))
import           Pos.Core.Delegation (HeavyDlgIndex (..), ProxySKHeavy)
import           Pos.Core.Slotting (EpochIndex (..), FlatSlotId,
                                    LocalSlotIndex (..), SlotId (..))
import           Pos.Core.Ssc (Commitment, CommitmentSignature, CommitmentsMap,
                               mkCommitmentsMap,mkVssCertificate, mkVssCertificatesMap, Opening,
                               SignedCommitment, SscPayload (..), VssCertificate (..),
                               VssCertificatesMap (..))
import           Pos.Core.Txp (TxAttributes, Tx (..), TxId, TxIn (..),
                               TxInWitness (..), TxOut (..), TxPayload (..),
                               TxSig, TxSigData (..), TxWitness)
import           Pos.Core.Update (ApplicationName (..), BlockVersion (..),
                                  BlockVersionModifier (..), SoftforkRule(..),
                                  SoftwareVersion (..), SystemTag (..), UpAttributes,
                                  UpdateData (..), UpdateProposal (..),
                                  UpdateProposalToSign  (..))
import           Pos.Crypto (deterministic, Hash, hash, safeCreatePsk, sign)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Delegation.Types (DlgPayload (..), ProxySKBlockInfo)
import           Pos.Ssc.Base (genCommitmentAndOpening)
import           Test.Pos.Crypto.Gen (genAbstractHash, genHDAddressPayload,
                                      genProtocolMagic, genPublicKey,
                                      genRedeemPublicKey, genRedeemSignature,
                                      genSafeSigner, genSecretKey,
                                      genSignature, genSignTag,
                                      genVssPublicKey)
import           Serokell.Data.Memory.Units (Byte)


----------------------------------------------------------------------------
-- Pos.Core.Block Generators
----------------------------------------------------------------------------

genGenesisHash :: Gen GenesisHash
genGenesisHash = do
  sampleText <- Gen.text Range.constantBounded Gen.alphaNum
  pure $ GenesisHash (coerce (hash sampleText :: Hash Text))

genGenesisHeader :: Gen GenesisBlockHeader
genGenesisHeader =
    mkGenesisHeader
        <$> genProtocolMagic
        <*> Gen.choice gens
        <*> genEpochIndex
        <*> (GenesisBody <$> genSlotLeaders)
  where
    gens = [ Left <$> genGenesisHash
           -- , Right <$> genBlockHeader
           ]

----------------------------------------------------------------------------
-- Pos.Core.Common Generators
----------------------------------------------------------------------------

genAddrAttributes :: Gen AddrAttributes
genAddrAttributes = AddrAttributes <$> hap <*> genAddrStakeDistribution
  where
    hap = Gen.maybe genHDAddressPayload

genAddress :: Gen Address
genAddress = makeAddress <$> genAddrSpendingData <*> genAddrAttributes

genAddrType :: Gen AddrType
genAddrType = Gen.choice [ pure ATPubKey
                         , pure ATScript
                         , pure ATRedeem
                         , ATUnknown <$> Gen.word8 Range.constantBounded
                         ]

genAddrSpendingData :: Gen AddrSpendingData
genAddrSpendingData = Gen.choice gens
  where
    gens = [ PubKeyASD <$> genPublicKey
           , ScriptASD <$> genScript
           , RedeemASD <$> genRedeemPublicKey
           , UnknownASD <$> Gen.word8 Range.constantBounded <*> gen32Bytes
           ]

genAddrStakeDistribution :: Gen AddrStakeDistribution
genAddrStakeDistribution = Gen.choice gens
  where
    gens = [ pure BootstrapEraDistr
           , SingleKeyDistr <$> genStakeholderId
           , UnsafeMultiKeyDistr <$> genMap
           ]
    genMap = Gen.map Range.constantBounded genPair
    genPair = do
      si <- genStakeholderId
      cp <- genCoinPortion
      pure (si, cp)

genCoeff :: Gen Coeff
genCoeff = do
    integer <- Gen.integral (Range.constant 0 10)
    pure $ Coeff (MkFixed integer)

genCoin :: Gen Coin
genCoin = Coin <$> Gen.word64 Range.constantBounded

genCoinPortion :: Gen CoinPortion
genCoinPortion = CoinPortion <$> Gen.word64 Range.constantBounded

genScript :: Gen Script
genScript = Script <$> genScriptVersion <*> gen32Bytes

genScriptVersion :: Gen ScriptVersion
genScriptVersion = Gen.word16 Range.constantBounded

genSlotLeaders :: Gen SlotLeaders
genSlotLeaders = do
    stakeHolderList <- Gen.list (Range.constant 0 10) genStakeholderId
    pure $ fromJust $ nonEmpty stakeHolderList

genStakeholderId :: Gen StakeholderId
genStakeholderId = genAbstractHash genPublicKey

genTxFeePolicy :: Gen TxFeePolicy
genTxFeePolicy =
    Gen.choice [ TxFeePolicyTxSizeLinear <$> genTxSizeLinear
               , TxFeePolicyUnknown <$> genWord8 <*> gen32Bytes
               ]

genTxSizeLinear :: Gen TxSizeLinear
genTxSizeLinear = TxSizeLinear <$> genCoeff <*> genCoeff
----------------------------------------------------------------------------
-- Pos.Core.Delegation Generators
----------------------------------------------------------------------------

genHeavyDlgIndex :: Gen HeavyDlgIndex
genHeavyDlgIndex = HeavyDlgIndex <$> genEpochIndex

genDlgPayload :: Gen DlgPayload
genDlgPayload =
    UnsafeDlgPayload <$> Gen.list (Range.constant 0 10) genProxySKHeavy

genProxySKBlockInfo :: Gen ProxySKBlockInfo
genProxySKBlockInfo = do
    pSKHeavy <- genProxySKHeavy
    pubKey <- genPublicKey
    pure $ Just (pSKHeavy,pubKey)

genProxySKHeavy :: Gen ProxySKHeavy
genProxySKHeavy =
    safeCreatePsk
        <$> genProtocolMagic
        <*> genSafeSigner
        <*> genPublicKey
        <*> genHeavyDlgIndex

----------------------------------------------------------------------------
-- Pos.Core.Slotting Generators
----------------------------------------------------------------------------

genEpochIndex :: Gen EpochIndex
genEpochIndex = EpochIndex <$> Gen.word64 Range.constantBounded

genFlatSlotId :: Gen FlatSlotId
genFlatSlotId = Gen.word64 Range.constantBounded

genLocalSlotIndex :: Gen LocalSlotIndex
genLocalSlotIndex = UnsafeLocalSlotIndex <$> Gen.word16 (Range.constant 0 21599)

genSlotId :: Gen SlotId
genSlotId = SlotId <$> genEpochIndex <*> genLocalSlotIndex

----------------------------------------------------------------------------
-- Pos.Core.Ssc Generators
----------------------------------------------------------------------------

data CommitmentOpening = CommitmentOpening
    { unCommitment :: !Commitment
    , unOpening :: !Opening
    }

genCommitment :: Gen Commitment
genCommitment = unCommitment <$> genCommitmentOpening

genCommitmentOpening :: Gen CommitmentOpening
genCommitmentOpening = do
    let numKeys = 128 :: Int
    parties <-
        Gen.integral (Range.constant 4 (fromIntegral numKeys)) :: Gen Integer
    threshold <- Gen.integral (Range.constant 2 (parties - 2)) :: Gen Integer
    vssKeys <- replicateM numKeys genVssPublicKey
    pure
        $ uncurry CommitmentOpening
        $ deterministic "commitmentOpening"
        $ genCommitmentAndOpening threshold (fromList vssKeys)

genCommitmentSignature :: Gen CommitmentSignature
genCommitmentSignature = genSignature $ (,) <$> genEpochIndex <*> genCommitment

genCommitmentsMap :: Gen CommitmentsMap
genCommitmentsMap = mkCommitmentsMap <$> Gen.list range genSignedCommitment
  where
    range = Range.constant 1 100

genOpening :: Gen Opening
genOpening = unOpening <$> genCommitmentOpening

genSignedCommitment :: Gen SignedCommitment
genSignedCommitment =
    (,,) <$> genPublicKey <*> genCommitment <*> genCommitmentSignature

genSscPayload :: Gen SscPayload
genSscPayload = CertificatesPayload <$> genVssCertificatesMap

genVssCertificate :: Gen VssCertificate
genVssCertificate =
    mkVssCertificate
        <$> genProtocolMagic
        <*> genSecretKey
        <*> (asBinary <$> genVssPublicKey)
        <*> genEpochIndex

genVssCertificatesMap :: Gen VssCertificatesMap
genVssCertificatesMap =
    mkVssCertificatesMap <$> Gen.list (Range.constant 0 10) genVssCertificate

----------------------------------------------------------------------------
-- Pos.Core.Txp Generators
----------------------------------------------------------------------------

genPkWitness :: Gen TxInWitness
genPkWitness = PkWitness <$> genPublicKey <*> genTxSig

genRedeemWitness :: Gen TxInWitness
genRedeemWitness =
    RedeemWitness <$> genRedeemPublicKey <*> genRedeemSignature genTxSigData

genScriptWitness :: Gen TxInWitness
genScriptWitness = ScriptWitness <$> genScript <*> genScript

genTx :: Gen Tx
genTx = UnsafeTx <$> genTxInList <*> genTxOutList <*> genTxAttributes

genTxAttributes :: Gen TxAttributes
genTxAttributes = pure $ mkAttributes ()

genTxHash :: Gen (Hash Tx)
genTxHash = hash <$> genTx

genTxIn :: Gen TxIn
genTxIn = Gen.choice gens
  where
    gens = [ TxInUtxo <$> genTxId <*> genWord32
           , TxInUnknown <$> genWord8 <*> gen32Bytes
           ]

genTxInList :: Gen (NonEmpty TxIn)
genTxInList = Gen.nonEmpty (Range.constant 1 100) genTxIn

genTxOut :: Gen TxOut
genTxOut = TxOut <$> genAddress <*> genCoin

genTxOutList :: Gen (NonEmpty TxOut)
genTxOutList = Gen.nonEmpty (Range.constant 1 100) genTxOut

genTxId :: Gen TxId
genTxId = hash <$> genTx

genTxIndex :: Gen Word32
genTxIndex = Gen.word32 (Range.constant 1 10)

genTxPayload :: Gen TxPayload
genTxPayload =
    UnsafeTxPayload
        <$> Gen.list (Range.constant 1 10) genTx
        <*> Gen.list (Range.constant 1 10) genTxWitness

genTxSig :: Gen TxSig
genTxSig =
    sign <$> genProtocolMagic <*> genSignTag <*> genSecretKey <*> genTxSigData

genTxSigData :: Gen TxSigData
genTxSigData = TxSigData <$> genTxHash

genTxInWitness :: Gen TxInWitness
genTxInWitness = Gen.choice gens
  where
    gens = [ genPkWitness
           , genRedeemWitness
           , genScriptWitness
           , genUnknownWitnessType
           ]

genTxWitness :: Gen TxWitness
genTxWitness = singleton <$> genTxInWitness

genUnknownWitnessType :: Gen TxInWitness
genUnknownWitnessType =
    UnknownWitnessType <$> Gen.word8 Range.constantBounded <*> gen32Bytes

----------------------------------------------------------------------------
-- Pos.Core.Update Generators
----------------------------------------------------------------------------

genApplicationName :: Gen ApplicationName
genApplicationName =
    ApplicationName <$> Gen.text (Range.constant 0 10) Gen.alphaNum

genBlockVersion :: Gen BlockVersion
genBlockVersion =
    BlockVersion
        <$> Gen.word16 Range.constantBounded
        <*> Gen.word16 Range.constantBounded
        <*> Gen.word8 Range.constantBounded

genBlockVersionModifier :: Gen BlockVersionModifier
genBlockVersionModifier =
    BlockVersionModifier
        <$> (Gen.maybe genScriptVersion)
        <*> (Gen.maybe genMillisecond)
        <*> (Gen.maybe genByte)
        <*> (Gen.maybe genByte)
        <*> (Gen.maybe genByte)
        <*> (Gen.maybe genByte)
        <*> (Gen.maybe genCoinPortion)
        <*> (Gen.maybe genCoinPortion)
        <*> (Gen.maybe genCoinPortion)
        <*> (Gen.maybe genCoinPortion)
        <*> (Gen.maybe genFlatSlotId)
        <*> (Gen.maybe genSoftforkRule)
        <*> (Gen.maybe genTxFeePolicy)
        <*> (Gen.maybe genEpochIndex)


genHashRaw :: Gen (Hash Raw)
genHashRaw = genAbstractHash $ Raw <$> gen32Bytes

genSoftforkRule :: Gen SoftforkRule
genSoftforkRule =
    SoftforkRule <$> genCoinPortion <*> genCoinPortion <*> genCoinPortion

genSoftwareVersion :: Gen SoftwareVersion
genSoftwareVersion =
    SoftwareVersion
        <$> genApplicationName
        <*> Gen.word32 Range.constantBounded

genSystemTag :: Gen SystemTag
genSystemTag = SystemTag <$> Gen.text (Range.constant 0 10) Gen.alphaNum

genUpAttributes :: Gen UpAttributes
genUpAttributes = pure $ mkAttributes ()

genUpdateData :: Gen UpdateData
genUpdateData =
    UpdateData
        <$> genHashRaw
        <*> genHashRaw
        <*> genHashRaw
        <*> genHashRaw

genUpdateProposal :: Gen UpdateProposal
genUpdateProposal =
    UnsafeUpdateProposal
        <$> genBlockVersion
        <*> genBlockVersionModifier
        <*> genSoftwareVersion
        <*> genUpsData
        <*> genUpAttributes
        <*> genPublicKey
        <*> genSignature genUpdateProposalToSign

genUpdateProposalToSign :: Gen UpdateProposalToSign
genUpdateProposalToSign =
    UpdateProposalToSign
        <$> genBlockVersion
        <*> genBlockVersionModifier
        <*> genSoftwareVersion
        <*> genUpsData
        <*> genUpAttributes

genUpsData :: Gen (HM.HashMap SystemTag UpdateData)
genUpsData = do
    hMapSize <- Gen.int (Range.constant 0 20)
    sysTagList <- Gen.list (Range.singleton hMapSize) genSystemTag
    upDataList <- Gen.list (Range.singleton hMapSize) genUpdateData
    pure $ HM.fromList $ zip sysTagList upDataList

----------------------------------------------------------------------------
-- Helper Generators
----------------------------------------------------------------------------

genBytes :: Int -> Gen ByteString
genBytes n = Gen.bytes (Range.singleton n)

genByte :: Gen Byte
genByte = Gen.integral (Range.constant 0 10)

gen32Bytes :: Gen ByteString
gen32Bytes = genBytes 32

genMillisecond :: Gen Millisecond
genMillisecond = fromMicroseconds <$> Gen.integral (Range.constant 0 1000000)

genWord32 :: Gen Word32
genWord32 = Gen.word32 Range.constantBounded

genWord8 :: Gen Word8
genWord8 = Gen.word8 Range.constantBounded
