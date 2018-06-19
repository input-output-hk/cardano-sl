module Test.Pos.Core.Gen
       (
        -- Pos.Core.Block Generators
          genBlockBodyAttributes
        , genBlockHeader
        , genBlockHeaderAttributes
        , genBlockSignature
        , genGenesisBlockHeader
        , genGenesisBody
        , genGenesisConsensusData
        , genGenesisHash
        , genGenesisProof
        , genMainBlockHeader
        , genMainBody
        , genMainConsensusData
        , genMainExtraBodyData
        , genMainExtraHeaderData
        , genMainProof
        , genMainToSign

        -- Pos.Core.Common Generators
        , genAddrAttributes
        , genAddress
        , genAddrSpendingData
        , genAddrStakeDistribution
        , genAddrType
        , genBlockCount
        , genChainDifficulty
        , genCoeff
        , genCoin
        , genCoinPortion
        , genScript
        , genScriptVersion
        , genSharedSeed
        , genSlotLeaders
        , genStakeholderId
        , genStakesList
        , genStakesMap
        , genTxFeePolicy
        , genTxSizeLinear

        -- Pos.Core.Configuration Generators
        , genGenesisConfiguration
        , genCoreConfiguration

        -- Pos.Core.Delegation Generators
        , genDlgPayload
        , genHeavyDlgIndex
        , genLightDlgIndices
        , genProxySKBlockInfo
        , genProxySKHeavy

        -- Pos.Core.Genesis Generators
        , genFakeAvvmOptions
        , genGenesisAvvmBalances
        , genGenesisDelegation
        , genGenesisProtocolConstants
        , genGenesisSpec
        , genTestnetBalanceOptions

        -- Pos.Core.ProtocolConstants
        , genVssMaxTTL
        , genVssMinTTL

        -- Pos.Core.Slotting Generators
        , genEpochIndex
        , genEpochOrSlot
        , genFlatSlotId
        , genLocalSlotIndex
        , genSlotCount
        , genSlotId
        , genTimeDiff
        , genTimestamp

        -- Pos.Core.Ssc Generators
        , genCommitment
        , genCommitmentsMap
        , genCommitmentSignature
        , genInnerSharesMap
        , genSharesMap
        , genOpening
        , genOpeningsMap
        , genSscPayload
        , genSscProof
        , genSharesDistribution
        , genSignedCommitment
        , genVssCertificate
        , genVssCertificatesHash
        , genVssCertificatesMap

        -- Pos.Core.Txp Generators
        , genPkWitness
        , genRedeemWitness
        , genScriptWitness
        , genTx
        , genTxAttributes
        , genTxAux
        , genTxHash
        , genTxId
        , genTxIn
        , genTxInList
        , genTxInWitness
        , genTxOut
        , genTxOutAux
        , genTxOutList
        , genTxPayload
        , genTxProof
        , genTxSig
        , genTxSigData
        , genTxWitness
        , genUnknownWitnessType

        -- Pos.Core.Update Generators
        , genApplicationName
        , genBlockVersion
        , genBlockVersionData
        , genBlockVersionModifier
        , genHashRaw
        , genSoftforkRule
        , genSoftwareVersion
        , genSystemTag
        , genUpAttributes
        , genUpdateData
        , genUpdatePayload
        , genUpdateProof
        , genUpdateProposal
        , genUpdateProposals
        , genUpdateProposalToSign
        , genUpdateVote
        , genUpId
        , genUpsData
        , genVoteId

        -- Pos.Data.Attributes Generators
        , genAttributes

        -- Pos.Merkle Generators
        , genMerkleRoot
        , genMerkleTree
       ) where

import           Universum

import           Data.Coerce (coerce)
import           Data.Either (either)
import           Data.Fixed (Fixed (..))
import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty (fromList)
import           Data.Maybe
import           Data.Time.Units (fromMicroseconds, Microsecond, Millisecond)
import           Data.Vector (singleton)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Pos.Binary.Class (asBinary, Bi, Raw (..))
import           Pos.Core.Block (BlockBodyAttributes, BlockHeader (..),
                                 BlockHeaderAttributes, BlockSignature (..),
                                 GenesisBlockHeader, GenesisBody (..),
                                 GenesisConsensusData (..), GenesisProof (..),
                                 MainBlockHeader, MainBody (..),
                                 MainConsensusData (..),
                                 MainExtraBodyData (..),
                                 MainExtraHeaderData (..), MainProof (..),
                                 MainToSign (..), mkMainHeader, mkGenesisHeader)
import           Pos.Core.Common (Address (..), AddrAttributes (..),
                                  AddrSpendingData (..),
                                  AddrStakeDistribution (..), AddrType (..),
                                  BlockCount (..), ChainDifficulty (..),
                                  Coeff (..), Coin (..), CoinPortion (..),
                                  makeAddress, Script (..), ScriptVersion,
                                  SharedSeed (..), SlotLeaders, StakeholderId,
                                  StakesList, StakesMap, TxFeePolicy (..),
                                  TxSizeLinear (..))
import           Pos.Core.Configuration (CoreConfiguration (..),
                                         GenesisConfiguration (..),
                                         GenesisHash (..))
import           Pos.Core.Delegation (HeavyDlgIndex (..), LightDlgIndices (..),
                                      ProxySKHeavy, DlgPayload (..), ProxySKBlockInfo)
import           Pos.Core.Genesis (FakeAvvmOptions (..),
                                   GenesisAvvmBalances (..),
                                   GenesisDelegation (..),
                                   GenesisInitializer (..),
                                   GenesisProtocolConstants (..),
                                   GenesisSpec (..), mkGenesisSpec,
                                   TestnetBalanceOptions (..))
import           Pos.Core.ProtocolConstants (VssMinTTL (..), VssMaxTTL (..))
import           Pos.Core.Slotting (EpochIndex (..), EpochOrSlot (..),
                                    FlatSlotId, LocalSlotIndex (..),
                                    SlotCount (..), SlotId (..), TimeDiff (..),
                                    Timestamp (..))
import           Pos.Core.Ssc (Commitment, CommitmentSignature, CommitmentsMap,
                               mkCommitmentsMap, mkSscProof, mkVssCertificate,
                               mkVssCertificatesMap, Opening, OpeningsMap,
                               InnerSharesMap, SharesDistribution, SharesMap,
                               SignedCommitment, SscPayload (..), SscProof,
                               VssCertificate (..), VssCertificatesHash,
                               VssCertificatesMap (..), randCommitmentAndOpening)
import           Pos.Core.Txp (Tx (..), TxAttributes, TxAux (..), TxId,
                               TxIn (..), TxInWitness (..), TxOut (..),
                               TxOutAux (..), TxPayload (..), TxProof (..),
                               TxSig, TxSigData (..), TxWitness)
import           Pos.Core.Update (ApplicationName (..), BlockVersion (..),
                                  BlockVersionData (..),
                                  BlockVersionModifier (..), SoftforkRule (..),
                                  SoftwareVersion (..), SystemTag (..),
                                  UpAttributes, UpdateData (..),
                                  UpdatePayload (..), UpdateProof,
                                  UpdateProposal (..), UpdateProposals,
                                  UpdateProposalToSign  (..), UpdateVote (..),
                                  UpId, VoteId)
import           Pos.Crypto (deterministic, Hash, hash, safeCreatePsk, sign)
import           Pos.Data.Attributes (Attributes (..), mkAttributes)
import           Pos.Merkle (mkMerkleTree, mtRoot, MerkleRoot(..),
                             MerkleTree (..))
import           Serokell.Data.Memory.Units (Byte)

import           Test.Pos.Crypto.Gen (genAbstractHash, genDecShare,
                                      genHDAddressPayload,genProtocolMagic,
                                      genProxySignature,genPublicKey,
                                      genRedeemPublicKey, genRedeemSignature,
                                      genSafeSigner, genSecretKey,
                                      genSignature, genSignTag,
                                      genVssPublicKey)


----------------------------------------------------------------------------
-- Pos.Core.Block Generators
----------------------------------------------------------------------------

genBlockBodyAttributes :: Gen BlockBodyAttributes
genBlockBodyAttributes = pure $ mkAttributes ()

genBlockHeader :: Gen BlockHeader
genBlockHeader =
    Gen.choice [ BlockHeaderGenesis <$> genGenesisBlockHeader
               , BlockHeaderMain <$> genMainBlockHeader
               ]

genBlockHeaderAttributes :: Gen BlockHeaderAttributes
genBlockHeaderAttributes = pure $ mkAttributes ()

genBlockSignature :: Gen BlockSignature
genBlockSignature =
    Gen.choice
        [ BlockSignature
              <$> genSignature genMainToSign
        , BlockPSignatureLight
              <$> genProxySignature genMainToSign genLightDlgIndices
        , BlockPSignatureHeavy
              <$> genProxySignature genMainToSign genHeavyDlgIndex
        ]

genGenesisBlockHeader :: Gen GenesisBlockHeader
genGenesisBlockHeader =
    mkGenesisHeader
        <$> genProtocolMagic
        <*> Gen.choice gens
        <*> genEpochIndex
        <*> genGenesisBody
  where
    gens = [ Left <$> genGenesisHash
           , Right <$> genBlockHeader
           ]

genGenesisBody :: Gen GenesisBody
genGenesisBody = GenesisBody <$> genSlotLeaders

genGenesisConsensusData :: Gen GenesisConsensusData
genGenesisConsensusData =
    GenesisConsensusData
        <$> genEpochIndex
        <*> genChainDifficulty

genGenesisHash :: Gen GenesisHash
genGenesisHash = do
  sampleText <- Gen.text Range.constantBounded Gen.alphaNum
  pure $ GenesisHash (coerce (hash sampleText :: Hash Text))

genGenesisProof :: Gen GenesisProof
genGenesisProof = GenesisProof <$> genAbstractHash genSlotLeaders

genMainBody :: Gen MainBody
genMainBody =
    MainBody
        <$> genTxPayload
        <*> genSscPayload
        <*> genDlgPayload
        <*> genUpdatePayload

genMainBlockHeader :: Gen MainBlockHeader
genMainBlockHeader =
    mkMainHeader
        <$> genProtocolMagic
        <*> (Left <$> genGenesisHash)
        <*> genSlotId
        <*> genSecretKey
        <*> genProxySKBlockInfo
        <*> genMainBody
        <*> genMainExtraHeaderData

genMainConsensusData :: Gen MainConsensusData
genMainConsensusData =
    MainConsensusData
        <$> genSlotId
        <*> genPublicKey
        <*> genChainDifficulty
        <*> genBlockSignature


genMainExtraBodyData :: Gen MainExtraBodyData
genMainExtraBodyData = MainExtraBodyData <$> genBlockBodyAttributes

genMainExtraHeaderData :: Gen MainExtraHeaderData
genMainExtraHeaderData =
    MainExtraHeaderData
        <$> genBlockVersion
        <*> genSoftwareVersion
        <*> genBlockHeaderAttributes
        <*> genAbstractHash genMainExtraBodyData

genMainProof :: Gen MainProof
genMainProof =
    MainProof
        <$> genTxProof
        <*> genSscProof
        <*> genAbstractHash genDlgPayload
        <*> genUpdateProof

genMainToSign :: Gen MainToSign
genMainToSign =
    MainToSign
        <$> genAbstractHash genBlockHeader
        <*> genMainProof
        <*> genSlotId
        <*> genChainDifficulty
        <*> genMainExtraHeaderData

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

genBlockCount :: Gen BlockCount
genBlockCount = BlockCount <$> Gen.word64 Range.constantBounded

genChainDifficulty :: Gen ChainDifficulty
genChainDifficulty = ChainDifficulty <$> genBlockCount

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

genSharedSeed :: Gen SharedSeed
genSharedSeed = SharedSeed <$> gen32Bytes

genSlotLeaders :: Gen SlotLeaders
genSlotLeaders = do
    stakeHolderList <- Gen.list (Range.constant 0 10) genStakeholderId
    pure $ fromJust $ nonEmpty stakeHolderList

genStakeholderId :: Gen StakeholderId
genStakeholderId = genAbstractHash genPublicKey

genStakesList :: Gen StakesList
genStakesList = Gen.list range gen
  where
    gen = (,) <$> genStakeholderId <*> genCoin
    range = Range.constant 0 1000

genStakesMap :: Gen StakesMap
genStakesMap = genCustomHashMap genStakeholderId genCoin

genTxFeePolicy :: Gen TxFeePolicy
genTxFeePolicy =
    Gen.choice [ TxFeePolicyTxSizeLinear <$> genTxSizeLinear
               , TxFeePolicyUnknown <$> genWord8 <*> gen32Bytes
               ]

genTxSizeLinear :: Gen TxSizeLinear
genTxSizeLinear = TxSizeLinear <$> genCoeff <*> genCoeff

----------------------------------------------------------------------------
-- Pos.Core.Configuration Generators
----------------------------------------------------------------------------

genGenesisConfiguration :: Gen GenesisConfiguration
genGenesisConfiguration =
    Gen.choice [ GCSrc
                     <$> Gen.string (Range.constant 10 25) Gen.alphaNum
                     <*> genHashRaw
               , GCSpec <$> genGenesisSpec
               ]

genCoreConfiguration :: Gen CoreConfiguration
genCoreConfiguration =
    CoreConfiguration
        <$> genGenesisConfiguration
        <*> genWord8

----------------------------------------------------------------------------
-- Pos.Core.Delegation Generators
----------------------------------------------------------------------------

genDlgPayload :: Gen DlgPayload
genDlgPayload =
    UnsafeDlgPayload <$> Gen.list (Range.constant 0 10) genProxySKHeavy

genHeavyDlgIndex :: Gen HeavyDlgIndex
genHeavyDlgIndex = HeavyDlgIndex <$> genEpochIndex

genLightDlgIndices :: Gen LightDlgIndices
genLightDlgIndices =
    LightDlgIndices <$> ((,) <$> genEpochIndex <*> genEpochIndex)

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
-- Pos.Core.Genesis Generators
----------------------------------------------------------------------------

genFakeAvvmOptions :: Gen FakeAvvmOptions
genFakeAvvmOptions =
    FakeAvvmOptions
        <$> Gen.word Range.constantBounded
        <*> Gen.word64 Range.constantBounded

genGenesisDelegation :: Gen GenesisDelegation
genGenesisDelegation =
    UnsafeGenesisDelegation
        <$> customHashMapGen genStakeholderId genProxySKHeavy

genGenesisInitializer :: Gen GenesisInitializer
genGenesisInitializer =
    GenesisInitializer
        <$> genTestnetBalanceOptions
        <*> genFakeAvvmOptions
        <*> genCoinPortion
        <*> Gen.bool
        <*> Gen.integral (Range.constant 0 10)

genGenesisProtocolConstants :: Gen GenesisProtocolConstants
genGenesisProtocolConstants =
    GenesisProtocolConstants
        <$> Gen.int (Range.constant 0 100)
        <*> genProtocolMagic
        <*> genVssMaxTTL
        <*> genVssMinTTL

genGenesisSpec :: Gen GenesisSpec
genGenesisSpec = mkGenSpec >>=  either (error . toText) pure
    where
        mkGenSpec = mkGenesisSpec
                      <$> genGenesisAvvmBalances
                      <*> genSharedSeed
                      <*> genGenesisDelegation
                      <*> genBlockVersionData
                      <*> genGenesisProtocolConstants
                      <*> genGenesisInitializer

genTestnetBalanceOptions :: Gen TestnetBalanceOptions
genTestnetBalanceOptions =
    TestnetBalanceOptions
        <$> Gen.word Range.constantBounded
        <*> Gen.word Range.constantBounded
        <*> Gen.word64 Range.constantBounded
        <*> Gen.double (Range.constant 0 10)
        <*> Gen.bool
----------------------------------------------------------------------------
-- Pos.Core.ProtocolConstants Generators
----------------------------------------------------------------------------

genVssMaxTTL :: Gen VssMaxTTL
genVssMaxTTL = VssMaxTTL <$> genWord32

genVssMinTTL :: Gen VssMinTTL
genVssMinTTL = VssMinTTL <$> genWord32

----------------------------------------------------------------------------
-- Pos.Core.Slotting Generators
----------------------------------------------------------------------------

genEpochIndex :: Gen EpochIndex
genEpochIndex = EpochIndex <$> Gen.word64 Range.constantBounded

genEpochOrSlot :: Gen EpochOrSlot
genEpochOrSlot =
    Gen.choice [ EpochOrSlot . Left <$> genEpochIndex
               , EpochOrSlot . Right <$> genSlotId
               ]

genFlatSlotId :: Gen FlatSlotId
genFlatSlotId = Gen.word64 Range.constantBounded

genLocalSlotIndex :: Gen LocalSlotIndex
genLocalSlotIndex = UnsafeLocalSlotIndex <$> Gen.word16 (Range.constant 0 21599)

genSlotCount :: Gen SlotCount
genSlotCount = SlotCount <$> Gen.word64 Range.constantBounded

genSlotId :: Gen SlotId
genSlotId = SlotId <$> genEpochIndex <*> genLocalSlotIndex

genTimeDiff :: Gen TimeDiff
genTimeDiff = TimeDiff <$> genMicrosecond

genTimestamp :: Gen Timestamp
genTimestamp = Timestamp <$> genMicrosecond

----------------------------------------------------------------------------
-- Pos.Core.Ssc Generators
----------------------------------------------------------------------------

genGenesisAvvmBalances :: Gen GenesisAvvmBalances
genGenesisAvvmBalances = GenesisAvvmBalances <$> customHashMapGen genRedeemPublicKey genCoin

genCommitment :: Gen Commitment
genCommitment = fst <$> genCommitmentOpening

genCommitmentOpening :: Gen (Commitment, Opening)
genCommitmentOpening = do
    let numKeys = 128 :: Int
    parties <-
        Gen.integral (Range.constant 4 (fromIntegral numKeys)) :: Gen Integer
    threshold <- Gen.integral (Range.constant 2 (parties - 2)) :: Gen Integer
    vssKeys <- replicateM numKeys genVssPublicKey
    pure
        $ deterministic "commitmentOpening"
        $ randCommitmentAndOpening threshold (fromList vssKeys)

genCommitmentSignature :: Gen CommitmentSignature
genCommitmentSignature = genSignature $ (,) <$> genEpochIndex <*> genCommitment

genCommitmentsMap :: Gen CommitmentsMap
genCommitmentsMap = mkCommitmentsMap <$> Gen.list range genSignedCommitment
  where
    range = Range.constant 1 100

genInnerSharesMap :: Gen InnerSharesMap
genInnerSharesMap = do
    hMS <- Gen.int (Range.constant 0 20)
    stakeholderId <- Gen.list (Range.singleton hMS) genStakeholderId
    nonEmptyDS <- Gen.nonEmpty (Range.singleton hMS) (asBinary <$> genDecShare)
    pure $ HM.fromList $ zip stakeholderId [nonEmptyDS]

genOpening :: Gen Opening
genOpening = snd <$> genCommitmentOpening

genOpeningsMap :: Gen OpeningsMap
genOpeningsMap = do
    hMapSize <- Gen.int (Range.constant 0 20)
    stakeholderId <- Gen.list (Range.singleton hMapSize) genStakeholderId
    opening <- Gen.list (Range.singleton hMapSize) genOpening
    pure $ HM.fromList $ zip stakeholderId opening

genSharesDistribution :: Gen SharesDistribution
genSharesDistribution = genCustomHashMap genStakeholderId genWord16
  where
    genWord16 = Gen.word16 Range.constantBounded

genSharesMap :: Gen SharesMap
genSharesMap = do
    hMapSize <- Gen.int (Range.constant 0 20)
    stakeholderId <- Gen.list (Range.singleton hMapSize) genStakeholderId
    innerSharesMap <- Gen.list (Range.singleton hMapSize) genInnerSharesMap
    pure $ HM.fromList $ zip stakeholderId innerSharesMap

genSignedCommitment :: Gen SignedCommitment
genSignedCommitment =
    (,,) <$> genPublicKey <*> genCommitment <*> genCommitmentSignature

genSscPayload :: Gen SscPayload
genSscPayload =
    Gen.choice
        [ CertificatesPayload <$> genVssCertificatesMap
        , CommitmentsPayload <$> genCommitmentsMap <*> genVssCertificatesMap
        , OpeningsPayload <$> genOpeningsMap <*> genVssCertificatesMap
        , SharesPayload <$> genSharesMap <*> genVssCertificatesMap
        ]

genSscProof :: Gen SscProof
genSscProof = mkSscProof <$> genSscPayload

genVssCertificate :: Gen VssCertificate
genVssCertificate =
    mkVssCertificate
        <$> genProtocolMagic
        <*> genSecretKey
        <*> (asBinary <$> genVssPublicKey)
        <*> genEpochIndex

genVssCertificatesHash :: Gen VssCertificatesHash
genVssCertificatesHash =
    hash <$> genCustomHashMap genStakeholderId genVssCertificate

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

genTxAux :: Gen TxAux
genTxAux = TxAux <$> genTx <*> genTxWitness

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

genTxOutAux :: Gen TxOutAux
genTxOutAux = TxOutAux <$> genTxOut

genTxOutList :: Gen (NonEmpty TxOut)
genTxOutList = Gen.nonEmpty (Range.constant 1 100) genTxOut

genTxId :: Gen TxId
genTxId = hash <$> genTx

genTxPayload :: Gen TxPayload
genTxPayload =
    UnsafeTxPayload
        <$> Gen.list (Range.constant 1 10) genTx
        <*> Gen.list (Range.constant 1 10) genTxWitness

genTxProof :: Gen TxProof
genTxProof =
    TxProof
        <$> genWord32
        <*> genMerkleRoot genTx
        <*> genAbstractHash (Gen.list (Range.constant 1 20) genTxWitness)

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

genBlockVersionData :: Gen BlockVersionData
genBlockVersionData =
    BlockVersionData
        <$> genScriptVersion
        <*> genMillisecond
        <*> genByte
        <*> genByte
        <*> genByte
        <*> genByte
        <*> genCoinPortion
        <*> genCoinPortion
        <*> genCoinPortion
        <*> genCoinPortion
        <*> genFlatSlotId
        <*> genSoftforkRule
        <*> genTxFeePolicy
        <*> genEpochIndex


genBlockVersionModifier :: Gen BlockVersionModifier
genBlockVersionModifier =
    BlockVersionModifier
        <$> Gen.maybe genScriptVersion
        <*> Gen.maybe genMillisecond
        <*> Gen.maybe genByte
        <*> Gen.maybe genByte
        <*> Gen.maybe genByte
        <*> Gen.maybe genByte
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genFlatSlotId
        <*> Gen.maybe genSoftforkRule
        <*> Gen.maybe genTxFeePolicy
        <*> Gen.maybe genEpochIndex

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

genUpdatePayload :: Gen UpdatePayload
genUpdatePayload =
    UpdatePayload
        <$> Gen.maybe genUpdateProposal
        <*> Gen.list (Range.constant 0 10) genUpdateVote

genUpdateProof :: Gen UpdateProof
genUpdateProof = genAbstractHash genUpdatePayload

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

genUpdateProposals :: Gen UpdateProposals
genUpdateProposals = genCustomHashMap genUpId genUpdateProposal

genUpdateProposalToSign :: Gen UpdateProposalToSign
genUpdateProposalToSign =
    UpdateProposalToSign
        <$> genBlockVersion
        <*> genBlockVersionModifier
        <*> genSoftwareVersion
        <*> genUpsData
        <*> genUpAttributes

genUpId :: Gen UpId
genUpId = genAbstractHash genUpdateProposal

genUpsData :: Gen (HM.HashMap SystemTag UpdateData)
genUpsData = do
    hMapSize <- Gen.int (Range.constant 0 20)
    sysTagList <- Gen.list (Range.singleton hMapSize) genSystemTag
    upDataList <- Gen.list (Range.singleton hMapSize) genUpdateData
    pure $ HM.fromList $ zip sysTagList upDataList

genUpdateVote :: Gen UpdateVote
genUpdateVote =
    UnsafeUpdateVote
        <$> genPublicKey
        <*> genUpId
        <*> Gen.bool
        <*> genSignature ((,) <$> genUpId <*> Gen.bool)

genVoteId :: Gen VoteId
genVoteId = (,,) <$> genUpId <*> genPublicKey <*> Gen.bool

----------------------------------------------------------------------------
-- Pos.Data.Attributes Generators
----------------------------------------------------------------------------

genAttributes :: Gen a -> Gen (Attributes a)
genAttributes genA =  mkAttributes <$> genA

----------------------------------------------------------------------------
-- Pos.Merkle Generators
----------------------------------------------------------------------------

genMerkleTree :: Bi a => Gen a -> Gen (MerkleTree a)
genMerkleTree genA = mkMerkleTree <$> Gen.list (Range.constant 0 100) genA

genMerkleRoot :: Bi a => Gen a -> Gen (MerkleRoot a)
genMerkleRoot genA = mtRoot <$> genMerkleTree genA

----------------------------------------------------------------------------
-- Helper Generators
----------------------------------------------------------------------------

customHashMapGen
    :: (Hashable k, Eq k)
    => Gen k -> Gen v -> Gen (HM.HashMap k v)
customHashMapGen keyGen valGen =
    HM.fromList
        <$> (Gen.list (Range.constant 1 10) $ (,) <$> keyGen <*> valGen)

genBytes :: Int -> Gen ByteString
genBytes n = Gen.bytes (Range.singleton n)

genByte :: Gen Byte
genByte = Gen.integral (Range.constant 0 10)

gen32Bytes :: Gen ByteString
gen32Bytes = genBytes 32

genCustomHashMap
    :: (Hashable k, Eq k)
    => Gen k -> Gen v -> Gen (HM.HashMap k v)
genCustomHashMap genK genV = HM.fromList <$> Gen.list range gen
  where
    gen = (,) <$> genK <*> genV
    range = Range.constant 0 1000

genMillisecond :: Gen Millisecond
genMillisecond = fromMicroseconds <$> Gen.integral (Range.constant 0 1000000)

genMicrosecond :: Gen Microsecond
genMicrosecond = fromMicroseconds <$> Gen.integral (Range.constant 0 1000000)

genWord32 :: Gen Word32
genWord32 = Gen.word32 Range.constantBounded

genWord8 :: Gen Word8
genWord8 = Gen.word8 Range.constantBounded
