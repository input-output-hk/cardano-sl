module Test.Pos.Core.ExampleHelpers
       (  -- Example data

          exampleAddrSpendingData_PubKey
        , exampleAddress
        , exampleAddress1
        , exampleAddress2
        , exampleAddress3
        , exampleAddress4
        , exampleBlockVersion
        , exampleBlockVersionData
        , exampleBlockVersionModifier
        , exampleCommitment
        , exampleCommitmentsMap
        , exampleCommitmentSignature
        , exampleChainDifficulty
        , exampleEpochIndex
        , exampleInnerSharesMap
        , exampleOpening
        , exampleOpeningsMap
        , examplePublicKey
        , examplePublicKeys
        , exampleRedeemPublicKey
        , exampleScript
        , exampleSecretKey
        , exampleSecretKeys
        , exampleSharedSeed
        , exampleSharesDistribution
        , exampleSignedCommitment
        , exampleSlotId
        , exampleSlotLeaders
        , exampleSoftwareVersion
        , exampleSscPayload
        , exampleSscProof
        , exampleStakeholderId
        , exampleStakeholderIds
        , exampleStakesList
        , exampleSystemTag
        , exampleUpdateData
        , exampleUpdatePayload
        , exampleUpdateProof
        , exampleUpdateProposal
        , exampleUpdateProposalToSign
        , exampleUpdateVote
        , exampleUpAttributes
        , exampleUpId
        , exampleVoteId
        , exampleVssCertificate
        , exampleVssCertificatesHash
        , exampleVssCertificatesMap
        , exampleVssPublicKeys
        , staticSafeSigners

        -- Helpers
        , feedPM
        , feedPC
        , feedPMC
        , feedEpochSlots
        , feedPMEpochSlots
       ) where

import           Universum

import qualified Crypto.SCRAPE as Scrape
import           Data.Coerce (coerce)
import           Data.Fixed (Fixed (..))
import qualified Data.HashMap.Strict as HM
import           Data.List ((!!))
import           Data.List.NonEmpty (fromList)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Time.Units (Millisecond)
import qualified Hedgehog as H
import           Serokell.Data.Memory.Units (Byte)

import qualified Cardano.Crypto.Wallet as CC
import           Pos.Binary.Class (Raw (..), asBinary)
import           Pos.Core.Attributes (Attributes, mkAttributes)
import           Pos.Core.Common (AddrAttributes (..), AddrSpendingData (..),
                     AddrStakeDistribution (..), Address (..), BlockCount (..),
                     ChainDifficulty (..), Coeff (..), Coin (..),
                     CoinPortion (..), Script (..), ScriptVersion,
                     SharedSeed (..), SlotLeaders, StakeholderId, StakesList,
                     TxFeePolicy (..), TxSizeLinear (..), addressHash,
                     coinPortionDenominator, makeAddress, mkMultiKeyDistr)
import           Pos.Core.ProtocolConstants (ProtocolConstants, pcEpochSlots)
import           Pos.Core.Slotting (EpochIndex (..), FlatSlotId,
                     LocalSlotIndex (..), SlotCount, SlotId (..))
import           Pos.Core.Ssc (Commitment, CommitmentSignature, CommitmentsMap,
                     InnerSharesMap, Opening, OpeningsMap, SharesDistribution,
                     SignedCommitment, SscPayload (..), SscProof (..),
                     VssCertificate (..), VssCertificatesHash,
                     VssCertificatesMap (..), mkCommitmentsMap,
                     mkVssCertificate, mkVssCertificatesMap,
                     randCommitmentAndOpening)
import           Pos.Core.Update (ApplicationName (..), BlockVersion (..),
                     BlockVersionData (..), BlockVersionModifier (..),
                     SoftforkRule (..), SoftwareVersion (..), SystemTag (..),
                     UpAttributes, UpId, UpdateData (..), UpdatePayload (..),
                     UpdateProof, UpdateProposal, UpdateProposalToSign (..),
                     UpdateVote (..), VoteId, mkUpdateProof,
                     mkUpdateProposalWSign, mkUpdateVoteSafe)
import           Pos.Crypto (EncShare (..), HDAddressPayload (..),
                     ProtocolMagic (..), RedeemPublicKey, SafeSigner (..),
                     Secret (..), SecretKey (..), SecretProof (..),
                     SignTag (..), VssKeyPair, VssPublicKey (..), abstractHash,
                     decryptShare, deterministic, deterministicVssKeyGen, hash,
                     redeemDeterministicKeyGen, sign, toVssPublicKey)
import           Pos.Crypto.Signing (PublicKey (..))

import           Test.Pos.Core.Gen (genProtocolConstants)
import           Test.Pos.Crypto.Bi (getBytes)
import           Test.Pos.Crypto.Gen (genProtocolMagic)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

feedPM :: (ProtocolMagic -> H.Gen a) -> H.Gen a
feedPM genA = genA =<< genProtocolMagic

feedPC :: (ProtocolConstants -> H.Gen a) -> H.Gen a
feedPC genA = genA =<< genProtocolConstants

feedPMC :: (ProtocolMagic -> ProtocolConstants -> H.Gen a) -> H.Gen a
feedPMC genA = do
    pm <- genProtocolMagic
    pc <- genProtocolConstants
    genA pm pc

feedEpochSlots :: (SlotCount -> H.Gen a) -> H.Gen a
feedEpochSlots genA = genA =<< pcEpochSlots <$> genProtocolConstants

feedPMEpochSlots :: (ProtocolMagic -> SlotCount -> H.Gen a) -> H.Gen a
feedPMEpochSlots genA = do
    pm <- genProtocolMagic
    epochSlots <- pcEpochSlots <$> genProtocolConstants
    genA pm epochSlots

--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleAttributes :: Attributes ()
exampleAttributes = mkAttributes ()

exampleBlockVersion :: BlockVersion
exampleBlockVersion = BlockVersion 1 1 1

exampleBlockVersionData :: BlockVersionData
exampleBlockVersionData = BlockVersionData
                              (999 :: ScriptVersion)
                              (999 :: Millisecond)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (99 :: FlatSlotId)
                              sfrule
                              (TxFeePolicyTxSizeLinear tslin)
                              (EpochIndex 99)
    where
        tslin = TxSizeLinear c1' c2'
        c1' = Coeff (MkFixed 999)
        c2' = Coeff (MkFixed 77)
        sfrule = (SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99))

exampleChainDifficulty :: ChainDifficulty
exampleChainDifficulty = ChainDifficulty (BlockCount 9999)

exampleCommitment :: Commitment
exampleCommitment = fst exampleCommitmentOpening

exampleCommitmentOpening :: (Commitment, Opening)
exampleCommitmentOpening =
  let numKeys   = 128 :: Int
      -- parties   = 20 :: Integer
      threshold = 15 :: Integer
      vssKeys   = replicate numKeys exampleVssPublicKey
  in  deterministic "commitmentOpening"
      $ randCommitmentAndOpening threshold (fromList vssKeys)

exampleCommitmentSignature :: CommitmentSignature
exampleCommitmentSignature =
    sign
      (ProtocolMagic 0)
      SignForTestingOnly
      exampleSecretKey
      (exampleEpochIndex, exampleCommitment)

exampleCommitmentsMap :: CommitmentsMap
exampleCommitmentsMap =
    let numCommitments = 1
        signedCommitments = replicate numCommitments exampleSignedCommitment
    in  mkCommitmentsMap signedCommitments

exampleEpochIndex :: EpochIndex
exampleEpochIndex = EpochIndex 14

exampleOpening :: Opening
exampleOpening = snd exampleCommitmentOpening

exampleOpeningsMap :: OpeningsMap
exampleOpeningsMap =
    let mapSize = 1
        stakeholderIds = replicate mapSize exampleStakeholderId
        openings = replicate mapSize exampleOpening
    in  HM.fromList $ zip stakeholderIds openings

exampleSafeSigner :: Int -> SafeSigner
exampleSafeSigner offset = staticSafeSigners!!offset

exampleSharesDistribution :: SharesDistribution
exampleSharesDistribution =
    let mapSize = 1
        stakeholderIds = replicate mapSize exampleStakeholderId
        word16s = [1337]
    in  HM.fromList $ zip stakeholderIds word16s

exampleSignedCommitment :: SignedCommitment
exampleSignedCommitment =
    (examplePublicKey, exampleCommitment, exampleCommitmentSignature)

exampleStakeholderId :: StakeholderId
exampleStakeholderId = abstractHash examplePublicKey :: StakeholderId

exampleStakeholderIds :: Int -> Int -> [StakeholderId]
exampleStakeholderIds offset l = map abstractHash $ examplePublicKeys offset l

exampleVssKeyPairs :: Int -> Int -> [VssKeyPair]
exampleVssKeyPairs offset count = map (toPair . (*offset)) [0..count]
    where
        toPair start = deterministicVssKeyGen (getBytes start 32)

exampleVssPublicKey :: VssPublicKey
exampleVssPublicKey = toVssPublicKey mkVssKeyPair
  where
    mkVssKeyPair = deterministicVssKeyGen $ (getBytes 0 32)

exampleVssPublicKeys :: Int -> Int -> [VssPublicKey]
exampleVssPublicKeys offset count = map (toKey . (*offset)) [0..count]
    where
        toKey start = toVssPublicKey . deterministicVssKeyGen $ (getBytes start 32)

exampleBlockVersionModifier :: BlockVersionModifier
exampleBlockVersionModifier = BlockVersionModifier
                              (Just (999 :: ScriptVersion))
                              (Just (999 :: Millisecond))
                              (Just (999 :: Byte))
                              (Just (999 :: Byte))
                              (Just (999 :: Byte))
                              (Just (999 :: Byte))
                              (Just $ CoinPortion 99)
                              (Just $ CoinPortion 99)
                              (Just $ CoinPortion 99)
                              (Just $ CoinPortion 99)
                              (Just (99 :: FlatSlotId))
                              (Just sfrule')
                              (Just $ TxFeePolicyTxSizeLinear tslin')
                              (Just $ EpochIndex 99)
    where
        tslin' = TxSizeLinear co1 co2
        co1 = Coeff (MkFixed 999)
        co2 = Coeff (MkFixed 77)
        sfrule' = (SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99))

exampleSlotId :: SlotId
exampleSlotId = SlotId (EpochIndex 11) (UnsafeLocalSlotIndex 47)

exampleAddrSpendingData_PubKey :: AddrSpendingData
exampleAddrSpendingData_PubKey = PubKeyASD examplePublicKey

examplePublicKey :: PublicKey
examplePublicKey = pk
  where [pk] = examplePublicKeys 16 1 -- 16 could be any number, as we take the first key

examplePublicKeys :: Int -> Int -> [PublicKey]
examplePublicKeys offset count = map (toKey . (*offset)) [0..count-1]
  where
    toKey start = let Right pk = PublicKey <$> CC.xpub (getBytes start 64)
                   in pk

exampleRedeemPublicKey :: RedeemPublicKey
exampleRedeemPublicKey = fromJust (fst <$> redeemDeterministicKeyGen (getBytes 0 32))

-- In order to get the key starting at byte 10, we generate two with offsets of 10
-- between them and take the second.
exampleSecretKey :: SecretKey
exampleSecretKey = (exampleSecretKeys 10 2) !! 1

exampleSecretKeys :: Int -> Int -> [SecretKey]
exampleSecretKeys offset count = map (toKey . (*offset)) [0..count-1]
  where
    toKey start = let Right sk = SecretKey <$> CC.xprv (getBytes start 128)
                   in sk

-- Lifted from genSharedSecret in `Pos.Crypto.SecretSharing`.
-- added `deterministic` in third guard.

exampleSharedSecret
    :: Scrape.Threshold -> NonEmpty VssPublicKey -> (Secret, SecretProof, [(VssPublicKey, EncShare)])
exampleSharedSecret t ps
    | t <= 1     = error "genSharedSecret: threshold must be > 1"
    | t >= n - 1 = error "genSharedSecret: threshold must be > n-1"
    | otherwise  = convertRes . deterministic "ss" $ Scrape.escrow t (coerce sorted)
  where
    n = fromIntegral (length ps)
    sorted = sort (toList ps)
    convertRes (gen, secret, shares, comms, proof, pproofs) =
        (coerce secret,
         SecretProof gen proof pproofs comms,
         zip sorted (coerce shares))

-- Not sure why you don't use `VssPublicKey` for the `InnerSharesMap`
-- as you use `VssPublicKey`s to generate `DecShare`s.

exampleInnerSharesMap :: Scrape.Threshold -> Int -> InnerSharesMap
exampleInnerSharesMap count offset =
    HM.fromList $ zipWith
                      (\x y -> ((addressHash x), fromList [asBinary y]))
                          (pubKeys)
                          (decShares)
        where
          -- generate VssPublicKey and VssSecretKey pairs.
          vssKeyPairs = exampleVssKeyPairs offset $ fromIntegral (count+1)
          -- generate SharedSecrets from the VssPublicKeys generated above.
          ss = exampleSharedSecret (count) (fromList $ map toVssPublicKey vssKeyPairs)
          -- filter `VssPublicKeys`s and their corresponding `EncShare`s.
          encShares (_, _, pKeSlist) = map snd pKeSlist
          -- generate `PublicKey`s
          pubKeys = examplePublicKeys 1 $ fromIntegral (count+1)
          -- generate `DecShares`
          decShares =
            [deterministic "ss" $ decryptShare pr es | pr <- vssKeyPairs, es <- encShares ss]

exampleScript :: Script
exampleScript = Script 601 (getBytes 4 32)

exampleStakesList :: StakesList
exampleStakesList = zip sis coins
  where
    sis   = map abstractHash (examplePublicKeys 15 3)
    coins = map Coin [79, 44, 9999999]

exampleSlotLeaders :: SlotLeaders
exampleSlotLeaders = map abstractHash (fromList (examplePublicKeys 16 3))

exampleSystemTag :: SystemTag
exampleSystemTag = (exampleSystemTags 0 1) !! 0

exampleSystemTags :: Int -> Int -> [SystemTag]
exampleSystemTags offset count = map (toSystemTag . (*offset)) [0..count-1]
  where
    toSystemTag start = SystemTag (getText start 16)

exampleUpAttributes :: UpAttributes
exampleUpAttributes = exampleAttributes

exampleUpdateData :: UpdateData
exampleUpdateData = (exampleUpdateDatas 10 2) !! 1

exampleUpdateDatas :: Int -> Int -> [UpdateData]
exampleUpdateDatas offset count = map (toUpdateData . (*offset)) [0..count-1]
  where
    toUpdateData start =
      let h = hash $ Raw (getBytes start 128)
      in  UpdateData h h h h

exampleUpId :: UpId
exampleUpId = hash exampleUpdateProposal

exampleUpdatePayload :: UpdatePayload
exampleUpdatePayload = UpdatePayload up uv
  where
    up = Just exampleUpdateProposal
    uv = [exampleUpdateVote]

exampleUpdateProof :: UpdateProof
exampleUpdateProof = mkUpdateProof exampleUpdatePayload

exampleUpdateProposal :: UpdateProposal
exampleUpdateProposalToSign :: UpdateProposalToSign
(exampleUpdateProposal, exampleUpdateProposalToSign) =
    ( mkUpdateProposalWSign pm bv bvm sv hm ua ss
    , UpdateProposalToSign bv bvm sv hm ua )
  where
    pm  = ProtocolMagic 0
    bv  = exampleBlockVersion
    bvm = exampleBlockVersionModifier
    sv  = exampleSoftwareVersion
    hm  = HM.fromList $ zip (exampleSystemTags 10 5) (exampleUpdateDatas 10 5)
    ua  = exampleUpAttributes
    ss  = exampleSafeSigner 0

exampleUpdateVote :: UpdateVote
exampleUpdateVote = mkUpdateVoteSafe pm ss ui ar
  where
    pm = ProtocolMagic 0
    ss = exampleSafeSigner 0
    ui = exampleUpId
    ar = True

-- | ```type VoteId = (UpId, PublicKey, Bool)```
exampleVoteId :: VoteId
exampleVoteId = (exampleUpId, examplePublicKey, False)

exampleVssCertificate :: VssCertificate
exampleVssCertificate =
    mkVssCertificate
        (ProtocolMagic 0)
        exampleSecretKey
        (asBinary (toVssPublicKey $ deterministicVssKeyGen ("golden" :: ByteString)))
        (EpochIndex 11)

exampleVssCertificates :: Int -> Int -> [VssCertificate]
exampleVssCertificates offset num =  map vssCert [0..num-1]
    where
        secretKeyList = (exampleSecretKeys offset num)
        vssCert index = mkVssCertificate
                           (ProtocolMagic 0)
                           (secretKeyList !! index)
                           (asBinary (toVssPublicKey $ deterministicVssKeyGen (getBytes index 128)))
                           (EpochIndex 122)

exampleVssCertificatesMap :: Int -> Int -> VssCertificatesMap
exampleVssCertificatesMap offset num = mkVssCertificatesMap $ exampleVssCertificates offset num


exampleVssCertificatesHash :: Int -> Int -> VssCertificatesHash
exampleVssCertificatesHash offset len =
    hash . getVssCertificatesMap $ exampleVssCertificatesMap offset len

staticSafeSigners :: [SafeSigner]
staticSafeSigners = map FakeSigner (exampleSecretKeys 1 6)

-- | Changing existing values in this string will break existing golden
-- tests, but it us OK to append more data to the end.
staticText :: Text
staticText
    = "Kmyw4lDSE5S4fSH6etNouiXezCyEjKc3tG4ja0kFjO8qzai26ZMPUEJfEy15ox5kJ0uKD\
    \bi7i6dLXkuesVZ9JfHgjrctsLFt2NvovXnchsOvX05Y6LohlTNt5mkPFhUoXu1EZSJTIy\
    \3fTU53b412r4AEusD7tcdRgH47yTr5hMO63bJnYBbmNperLHfiT1lP0MLQLh1J1DfoYBs\
    \auoJOzvtAgvjHo6UFttnK6vZ3Cknpuob6uMS2MkJKmuoQsqsAYcRDWbJ2Rgw4bm2ndTM4\
    \zFfuRDKvdrL6sDkuPNPYqxMWlqnXjSbU0eLtceZuKgXLHR8cdvsEvywt4JaZUQhnbq3Vl\
    \7nZqcXdoi4XGTCgSGcGp8N0SDVhvkVh0QF1RVpWPnOMyYISJvuaHfo1zXMdq9tEdtJfID"

getText :: Int -> Int -> Text
getText offset len = T.take len $ T.drop offset staticText

exampleSoftwareVersion :: SoftwareVersion
exampleSoftwareVersion = SoftwareVersion (ApplicationName "Golden") 99

exampleSscProof :: SscProof
exampleSscProof = CommitmentsProof (hash exampleCommitmentsMap)
                                   (exampleVssCertificatesHash 10 4)

exampleSscPayload :: SscPayload
exampleSscPayload = SharesPayload exampleSharesMap (exampleVssCertificatesMap 10 4)
  where
    exampleSharesMap = HM.fromList $ [(exampleStakeholderId, exampleInnerSharesMap 3 1)]

exampleAddress :: Address
exampleAddress = makeAddress exampleAddrSpendingData_PubKey attrs
  where
    attrs = AddrAttributes hap BootstrapEraDistr
    hap = Just (HDAddressPayload (getBytes 32 32))

exampleAddress1 :: Address
exampleAddress1 = makeAddress easd attrs
  where
    easd = PubKeyASD pk
    [pk] = examplePublicKeys 24 1
    attrs = AddrAttributes hap BootstrapEraDistr
    hap = Nothing

exampleAddress2 :: Address
exampleAddress2 = makeAddress easd attrs
  where
    easd = RedeemASD exampleRedeemPublicKey
    attrs = AddrAttributes hap asd
    hap = Just (HDAddressPayload (getBytes 15 32))
    asd = SingleKeyDistr exampleStakeholderId

exampleAddress3 :: Address
exampleAddress3 = makeAddress easd attrs
  where
    easd = ScriptASD exampleScript
    attrs = AddrAttributes hap exampleMultiKeyDistr
    hap = Just (HDAddressPayload (getBytes 17 32))

exampleAddress4 :: Address
exampleAddress4 = makeAddress easd attrs
  where
    easd = UnknownASD 7 "test value"
    attrs = AddrAttributes Nothing (SingleKeyDistr sId)
    [sId] = exampleStakeholderIds 7 1

exampleMultiKeyDistr :: AddrStakeDistribution
exampleMultiKeyDistr = case mkMultiKeyDistr (M.fromList pairs) of
    Left err -> error $
        "exampleMultiKeyDistr: improperly constructed stake map: " <> show err
    Right asd -> asd
  where
    pairs = zip stakeIds (map CoinPortion (remainderCP : coinPortions))
    stakeIds = map abstractHash (examplePublicKeys 7 4)
    coinPortions = [ (10 :: Word64) ^ (12 :: Word64)
                   , ( 7 :: Word64) ^ (11 :: Word64)
                   , ( 6 :: Word64) ^ (14 :: Word64)
                   ]
    remainderCP = coinPortionDenominator - sum coinPortions

exampleSharedSeed :: SharedSeed
exampleSharedSeed = SharedSeed (getBytes 8 32)
