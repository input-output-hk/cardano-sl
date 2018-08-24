module Test.Pos.Core.ExampleHelpers
       (  -- Example data

          exampleAddrSpendingData_PubKey
        , exampleAddress
        , exampleAddress1
        , exampleAddress2
        , exampleAddress3
        , exampleAddress4
        , exampleBlockVersion
        , exampleBlockVersionData0
        , exampleBlockVersionData1
        , exampleBlockVersionData2
        , exampleBlockVersionModifier
        , exampleCommitment
        , exampleCommitmentsMap
        , exampleCommitmentSignature
        , exampleChainDifficulty
        , exampleEpochIndex
        , exampleGenesisWStakeholders
        , exampleGenesisVssCertificatesMap
        , exampleGenesisNonAvvmBalances0
        , exampleGenesisNonAvvmBalances1
        , exampleGenesisNonAvvmBalances2
        , exampleGenesisAvvmBalances0
        , exampleGenesisAvvmBalances1
        , exampleGenesisAvvmBalances2
        , exampleGenesisConfiguration_GCSpec0
        , exampleGenesisConfiguration_GCSpec1
        , exampleGenesisConfiguration_GCSpec2
        , exampleGenesisConfiguration_GCSrc
        , exampleGenesisData0
        , exampleGenesisData1
        , exampleGenesisData2
        , exampleGenesisDelegation
        , exampleGenesisInitializer0
        , exampleGenesisInitializer1
        , exampleGenesisInitializer2
        , exampleHashTx
        , exampleInnerSharesMap
        , exampleLightDlgIndices
        , exampleOpening
        , exampleOpeningsMap
        , exampleGenesisProtocolConstants0
        , exampleGenesisProtocolConstants1
        , exampleGenesisProtocolConstants2
        , exampleProxySKBlockInfo
        , examplePublicKey
        , exampleRedeemPublicKey
        , exampleRedeemSignature
        , exampleScript
        , exampleSecretKey
        , exampleSecretKeys
        , exampleSharedSeed0
        , exampleSharedSeed1
        , exampleSharedSeed2
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
        , exampleTxId
        , exampleTxInList
        , exampleTxInUnknown
        , exampleTxInUtxo
        , exampleTxPayload
        , exampleTxProof
        , exampleTxOut
        , exampleTxOutList
        , exampleTxSig
        , exampleTxSigData
        , exampleTxWitness
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
        , staticHeavyDlgIndexes
        , staticProxySKHeavys

        -- Helpers
        , feedPM
        , feedPC
        , feedPMC
       ) where

import           Universum

import qualified Crypto.SCRAPE as Scrape
import qualified Crypto.Sign.Ed25519 as Ed25519
import           Data.Coerce (coerce)
import           Data.Fixed (Fixed (..))
import qualified Data.HashMap.Strict as HM
import           Data.List (zipWith4, (!!))
import           Data.List.NonEmpty (fromList)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Time.Units (Millisecond)
import qualified Data.Vector as V
import qualified Hedgehog as H
import           Serokell.Data.Memory.Units (Byte)
import qualified Serokell.Util.Base16 as B16

import qualified Cardano.Crypto.Wallet as CC
import           Pos.Binary.Class (Raw (..), asBinary)
import           Pos.Core.Attributes (Attributes, mkAttributes)
import           Pos.Core.Common (AddrAttributes (..), AddrSpendingData (..),
                     AddrStakeDistribution (..), Address (..), BlockCount (..),
                     ChainDifficulty (..), Coeff (..), Coin (..),
                     CoinPortion (..), IsBootstrapEraAddr (..), Script (..),
                     ScriptVersion, SharedSeed (..), SlotLeaders,
                     StakeholderId, StakesList, TxFeePolicy (..),
                     TxSizeLinear (..), addressHash, coinPortionDenominator,
                     makeAddress, makePubKeyAddress, mkMultiKeyDistr)
import           Pos.Core.Configuration
import           Pos.Core.Delegation (HeavyDlgIndex (..), LightDlgIndices (..),
                     ProxySKBlockInfo, ProxySKHeavy)
import           Pos.Core.Genesis (FakeAvvmOptions (..),
                     GenesisAvvmBalances (..), GenesisData (..),
                     GenesisDelegation (..), GenesisInitializer (..),
                     GenesisNonAvvmBalances (..),
                     GenesisProtocolConstants (..), GenesisSpec (..),
                     GenesisVssCertificatesMap (..), GenesisWStakeholders (..),
                     TestnetBalanceOptions (..))
import           Pos.Core.Merkle (mkMerkleTree, mtRoot)
import           Pos.Core.ProtocolConstants (ProtocolConstants, VssMaxTTL (..),
                     VssMinTTL (..))
import           Pos.Core.Slotting (EpochIndex (..), FlatSlotId,
                     LocalSlotIndex (..), SlotId (..), Timestamp (..))
import           Pos.Core.Ssc (Commitment, CommitmentSignature, CommitmentsMap,
                     InnerSharesMap, Opening, OpeningsMap, SharesDistribution,
                     SignedCommitment, SscPayload (..), SscProof (..),
                     VssCertificate (..), VssCertificatesHash,
                     VssCertificatesMap (..), mkCommitmentsMap,
                     mkVssCertificate, mkVssCertificatesMap,
                     randCommitmentAndOpening)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxId, TxIn (..),
                     TxInWitness (..), TxOut (..), TxPayload (..),
                     TxProof (..), TxSig, TxSigData (..), TxWitness,
                     mkTxPayload)
import           Pos.Core.Update (ApplicationName (..), BlockVersion (..),
                     BlockVersionData (..), BlockVersionModifier (..),
                     SoftforkRule (..), SoftwareVersion (..), SystemTag (..),
                     UpAttributes, UpId, UpdateData (..), UpdatePayload (..),
                     UpdateProof, UpdateProposal, UpdateProposalToSign (..),
                     UpdateVote (..), VoteId, mkUpdateProof,
                     mkUpdateProposalWSign, mkUpdateVoteSafe)
import           Pos.Crypto (AbstractHash (..), EncShare (..),
                     HDAddressPayload (..), Hash, ProtocolMagic (..),
                     RedeemPublicKey, RedeemSignature, SafeSigner (..),
                     Secret (..), SecretKey (..), SecretProof (..),
                     SignTag (..), VssKeyPair, VssPublicKey (..), abstractHash,
                     decryptShare, deterministic, deterministicVssKeyGen, hash,
                     redeemDeterministicKeyGen, redeemSign, safeCreatePsk,
                     sign, toVssPublicKey)
import           Pos.Crypto.Signing (ProxyCert (..), ProxySecretKey (..),
                     PublicKey (..), RedeemPublicKey (..))

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


--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleAttributes :: Attributes ()
exampleAttributes = mkAttributes ()

exampleBlockVersion :: BlockVersion
exampleBlockVersion = BlockVersion 1 1 1

exampleBlockVersionData0 :: BlockVersionData
exampleBlockVersionData0 = BlockVersionData
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

exampleBlockVersionData1 :: BlockVersionData
exampleBlockVersionData1 = BlockVersionData
    { bvdScriptVersion = 56903
    , bvdSlotDuration = 379 :: Millisecond
    , bvdMaxBlockSize = 0 :: Byte
    , bvdMaxHeaderSize = 2 :: Byte
    , bvdMaxTxSize = 8 :: Byte
    , bvdMaxProposalSize = 0 :: Byte
    , bvdMpcThd = CoinPortion
        { getCoinPortion = 340531572846619
        }
    , bvdHeavyDelThd = CoinPortion
        { getCoinPortion = 136666896062087
        }
    , bvdUpdateVoteThd = CoinPortion
        { getCoinPortion = 6813701006338
        }
    , bvdUpdateProposalThd = CoinPortion
        { getCoinPortion = 497008830503149
        }
    , bvdUpdateImplicit = 6473853538638325423
    , bvdSoftforkRule = SoftforkRule
        { srInitThd = CoinPortion
            { getCoinPortion = 960317883358477
            }
        , srMinThd = CoinPortion
            { getCoinPortion = 532535816427207
            }
        , srThdDecrement = CoinPortion
            { getCoinPortion = 329013992078399
            }
        }
    , bvdTxFeePolicy =
        TxFeePolicyTxSizeLinear $
        TxSizeLinear (Coeff $ MkFixed 172) (Coeff $ MkFixed 8)
    , bvdUnlockStakeEpoch = EpochIndex
        { getEpochIndex = 13346688070232230243
        }
    }

exampleBlockVersionData2 :: BlockVersionData
exampleBlockVersionData2 = BlockVersionData
    { bvdScriptVersion = 9061
    , bvdSlotDuration = 734 :: Millisecond
    , bvdMaxBlockSize = 4 :: Byte
    , bvdMaxHeaderSize = 5 :: Byte
    , bvdMaxTxSize = 6 :: Byte
    , bvdMaxProposalSize = 0 :: Byte
    , bvdMpcThd = CoinPortion
        { getCoinPortion = 236107662480767
        }
    , bvdHeavyDelThd = CoinPortion
        { getCoinPortion = 433334544134126
        }
    , bvdUpdateVoteThd = CoinPortion
        { getCoinPortion = 934785458282543
        }
    , bvdUpdateProposalThd = CoinPortion
        { getCoinPortion = 642506074573997
        }
    , bvdUpdateImplicit = 13112058965239099249
    , bvdSoftforkRule = SoftforkRule
        { srInitThd = CoinPortion
            { getCoinPortion = 46944805160625
            }
        , srMinThd = CoinPortion
            { getCoinPortion = 195823507728266
            }
        , srThdDecrement = CoinPortion
            { getCoinPortion = 747866672432320
            }
        }
    , bvdTxFeePolicy =
        TxFeePolicyTxSizeLinear $
        TxSizeLinear (Coeff $ MkFixed 31) (Coeff $ MkFixed 84)
    , bvdUnlockStakeEpoch = EpochIndex
        { getEpochIndex = 3647707432224754741
        }
    }

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

exampleRedeemSignature :: RedeemSignature TxSigData
exampleRedeemSignature = redeemSign (ProtocolMagic 0) SignForTestingOnly rsk exampleTxSigData
    where
        rsk = fromJust (snd <$> redeemDeterministicKeyGen (getBytes 0 32))

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

exampleTxAux :: TxAux
exampleTxAux = TxAux tx exampleTxWitness
  where
    tx = UnsafeTx exampleTxInList exampleTxOutList (mkAttributes ())

exampleTxId :: TxId
exampleTxId = exampleHashTx

exampleTxInList :: (NonEmpty TxIn)
exampleTxInList = fromList [exampleTxInUtxo]

exampleTxInUnknown :: TxIn
exampleTxInUnknown = TxInUnknown 47 ("forty seven" :: ByteString)

exampleTxInUtxo :: TxIn
exampleTxInUtxo = TxInUtxo exampleHashTx 47 -- TODO: loop here

exampleTxOut :: TxOut
exampleTxOut = TxOut (makePubKeyAddress (IsBootstrapEraAddr True) pkey) (Coin 47)
    where
        Right pkey = PublicKey <$> CC.xpub (getBytes 0 64)

exampleTxOutList :: (NonEmpty TxOut)
exampleTxOutList = fromList [exampleTxOut]

exampleTxProof :: TxProof
exampleTxProof = TxProof 32 mroot hashWit
  where
    mroot = mtRoot $ mkMerkleTree [(UnsafeTx exampleTxInList exampleTxOutList (mkAttributes ()))]
    hashWit = hash $ [(V.fromList [(PkWitness examplePublicKey exampleTxSig)])]

exampleTxSig :: TxSig
exampleTxSig = sign (ProtocolMagic 0) SignForTestingOnly exampleSecretKey exampleTxSigData

exampleTxSigData :: TxSigData
exampleTxSigData = TxSigData exampleHashTx

exampleTxWitness :: TxWitness
exampleTxWitness = V.fromList [(PkWitness examplePublicKey exampleTxSig)]

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


staticHeavyDlgIndexes :: [HeavyDlgIndex]
staticHeavyDlgIndexes = map (HeavyDlgIndex . EpochIndex) [5,1,3,27,99,247]

staticProtocolMagics :: [ProtocolMagic]
staticProtocolMagics = map ProtocolMagic [0..5]

staticProxySKHeavys :: [ProxySKHeavy]
staticProxySKHeavys = zipWith4 safeCreatePsk
                               staticProtocolMagics staticSafeSigners
                               (examplePublicKeys 1 6) staticHeavyDlgIndexes

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

exampleTxPayload :: TxPayload
exampleTxPayload = mkTxPayload [exampleTxAux]

exampleSoftwareVersion :: SoftwareVersion
exampleSoftwareVersion = SoftwareVersion (ApplicationName "Golden") 99

exampleHashTx :: Hash Tx
exampleHashTx = coerce (hash "golden" :: Hash Text)

exampleSscProof :: SscProof
exampleSscProof = CommitmentsProof (hash exampleCommitmentsMap)
                                   (exampleVssCertificatesHash 10 4)

exampleSscPayload :: SscPayload
exampleSscPayload = SharesPayload exampleSharesMap (exampleVssCertificatesMap 10 4)
  where
    exampleSharesMap = HM.fromList $ [(exampleStakeholderId, exampleInnerSharesMap 3 1)]

exampleProxySKBlockInfo :: ProxySKBlockInfo
exampleProxySKBlockInfo = Just (staticProxySKHeavys !! 0, examplePublicKey)

exampleLightDlgIndices :: LightDlgIndices
exampleLightDlgIndices = LightDlgIndices (EpochIndex 7, EpochIndex 88)

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

exampleGenesisConfiguration_GCSrc :: GenesisConfiguration
exampleGenesisConfiguration_GCSrc =
    GCSrc "dRaMwdYsH3QA3dChe" (abstractHash (Raw "Test"))

exampleGenesisConfiguration_GCSpec0 :: GenesisConfiguration
exampleGenesisConfiguration_GCSpec0 =
    GCSpec $ UnsafeGenesisSpec
        exampleGenesisAvvmBalances0
        exampleSharedSeed0
        exampleGenesisDelegation
        exampleBlockVersionData0
        exampleGenesisProtocolConstants0
        exampleGenesisInitializer0

exampleGenesisConfiguration_GCSpec1 :: GenesisConfiguration
exampleGenesisConfiguration_GCSpec1 =
    GCSpec $ UnsafeGenesisSpec
        exampleGenesisAvvmBalances1
        exampleSharedSeed1
        exampleGenesisDelegation
        exampleBlockVersionData1
        exampleGenesisProtocolConstants1
        exampleGenesisInitializer1

exampleGenesisConfiguration_GCSpec2 :: GenesisConfiguration
exampleGenesisConfiguration_GCSpec2 =
    GCSpec $ UnsafeGenesisSpec
        exampleGenesisAvvmBalances2
        exampleSharedSeed2
        exampleGenesisDelegation
        exampleBlockVersionData2
        exampleGenesisProtocolConstants2
        exampleGenesisInitializer2

exampleGenesisData0 :: GenesisData
exampleGenesisData0 =
    GenesisData
        { gdBootStakeholders = exampleGenesisWStakeholders
        , gdHeavyDelegation = exampleGenesisDelegation
        -- Timestamps are stored as seconds, so they are rounded to
        -- the nearest 10^6.
        , gdStartTime = Timestamp { getTimestamp = 421337000000 }
        , gdVssCerts = exampleGenesisVssCertificatesMap 10 4
        , gdNonAvvmBalances = exampleGenesisNonAvvmBalances0
        , gdBlockVersionData = exampleBlockVersionData0
        , gdProtocolConsts = exampleGenesisProtocolConstants0
        , gdAvvmDistr = exampleGenesisAvvmBalances0
        , gdFtsSeed = exampleSharedSeed0
        }

exampleGenesisData1 :: GenesisData
exampleGenesisData1 =
    GenesisData
        { gdBootStakeholders = exampleGenesisWStakeholders
        , gdHeavyDelegation = exampleGenesisDelegation
        , gdStartTime = Timestamp { getTimestamp = 3131000000 }
        , gdVssCerts = exampleGenesisVssCertificatesMap 0 10
        , gdNonAvvmBalances = exampleGenesisNonAvvmBalances1
        , gdBlockVersionData = exampleBlockVersionData1
        , gdProtocolConsts = exampleGenesisProtocolConstants1
        , gdAvvmDistr = exampleGenesisAvvmBalances1
        , gdFtsSeed = exampleSharedSeed1
        }

exampleGenesisData2 :: GenesisData
exampleGenesisData2 =
    GenesisData
        { gdBootStakeholders = exampleGenesisWStakeholders
        , gdHeavyDelegation = exampleGenesisDelegation
        , gdStartTime = Timestamp { getTimestamp = 3735000000 }
        , gdVssCerts = exampleGenesisVssCertificatesMap 8 5
        , gdNonAvvmBalances = exampleGenesisNonAvvmBalances2
        , gdBlockVersionData = exampleBlockVersionData2
        , gdProtocolConsts = exampleGenesisProtocolConstants2
        , gdAvvmDistr = exampleGenesisAvvmBalances2
        , gdFtsSeed = exampleSharedSeed2
        }

exampleGenesisWStakeholders :: GenesisWStakeholders
exampleGenesisWStakeholders =
    let mapSize = 1
        stakeholderIds = replicate mapSize exampleStakeholderId
        word16s :: [Word16]
        word16s = [1337]
    in  GenesisWStakeholders { getGenesisWStakeholders =
            M.fromList $ zip stakeholderIds word16s }

exampleGenesisVssCertificatesMap :: Int -> Int -> GenesisVssCertificatesMap
exampleGenesisVssCertificatesMap offset num =
    GenesisVssCertificatesMap { getGenesisVssCertificatesMap =
        exampleVssCertificatesMap offset num }

exampleGenesisNonAvvmBalances0 :: GenesisNonAvvmBalances
exampleGenesisNonAvvmBalances0 =
    GenesisNonAvvmBalances {getGenesisNonAvvmBalances =
        (HM.fromList [ (exampleAddress, coin)
                     , (exampleAddress1, coin1)
                     ]) }
  where
    coin  = Coin {getCoin = 36524597913081152}
    coin1 = Coin {getCoin = 37343863242999412}

exampleGenesisNonAvvmBalances1 :: GenesisNonAvvmBalances
exampleGenesisNonAvvmBalances1 =
    GenesisNonAvvmBalances {getGenesisNonAvvmBalances =
        (HM.fromList [ (exampleAddress, coin)
                     , (exampleAddress1, coin1)
                     , (exampleAddress2, coin2)
                     ]) }
  where
    coin  = Coin {getCoin = 1234567}
    coin1 = Coin {getCoin = 1337}
    coin2 = Coin {getCoin = 12552661871782587}

exampleGenesisNonAvvmBalances2 :: GenesisNonAvvmBalances
exampleGenesisNonAvvmBalances2 =
    GenesisNonAvvmBalances {getGenesisNonAvvmBalances =
        (HM.fromList [ (exampleAddress, coin)
                     , (exampleAddress1, coin1)
                     , (exampleAddress2, coin2)
                     , (exampleAddress3, coin3)
                     , (exampleAddress4, coin4)
                     ]) }
  where
    coin  = Coin {getCoin = 4873920820}
    coin1 = Coin {getCoin = 8274687}
    coin2 = Coin {getCoin = 12552661871782587}
    coin3 = Coin {getCoin = 189377823442}
    coin4 = Coin {getCoin = 4672189289312323}

exampleGenesisAvvmBalances0 :: GenesisAvvmBalances
exampleGenesisAvvmBalances0 =
    GenesisAvvmBalances {getGenesisAvvmBalances =
        (HM.fromList [(RedeemPublicKey (Ed25519.PublicKey fstRedKey)
                     , Coin {getCoin = 36524597913081152})
                     ,(RedeemPublicKey (Ed25519.PublicKey  sndRedKey)
                     ,Coin {getCoin = 37343863242999412})
                     ]) }
  where
    fstRedKey = hexToBS "e2a1773a2a82d10c30890cbf84eccbdc1aaaee9204\
                        \96424d36e868039d9cb519"
    sndRedKey = hexToBS "9cdabcec332abbc6fdf883ca5bf3a8afddca69bfea\
                        \c14c013304da88ac032fe6"

exampleGenesisAvvmBalances1 :: GenesisAvvmBalances
exampleGenesisAvvmBalances1 =
    GenesisAvvmBalances {getGenesisAvvmBalances =
        (HM.fromList [(RedeemPublicKey (Ed25519.PublicKey fstRedKey)
                     , Coin {getCoin = 434210906})
                     ,(RedeemPublicKey (Ed25519.PublicKey  sndRedKey)
                     ,Coin {getCoin = 172323403})
                     ]) }
  where
    fstRedKey = hexToBS "a75fe437a908c252a8f3e9601df15d593a1a2a589c\
                        \65b9519b0fab24f9396bdf"
    sndRedKey = hexToBS "296ca6970e61bf9854aea38d2cae9b3019a5c63fd28\
                        \ff4a0e9d366c825c788e4"

exampleGenesisAvvmBalances2 :: GenesisAvvmBalances
exampleGenesisAvvmBalances2 =
    GenesisAvvmBalances {getGenesisAvvmBalances =
        (HM.fromList [(RedeemPublicKey (Ed25519.PublicKey fstRedKey)
                     , Coin {getCoin = 630099400})
                     ,(RedeemPublicKey (Ed25519.PublicKey  sndRedKey)
                     ,Coin {getCoin = 37343863242999412})
                     ]) }
  where
    fstRedKey = hexToBS "81a329ab75009b925efc26c334919509650b865dd6\
                        \3d6f8db257e0245f04d324"
    sndRedKey = hexToBS "72043ce554335af1193adeb04639c1e1e2f7aaf203\
                        \d997f8032856677c2ce05f"

exampleSharedSeed0 :: SharedSeed
exampleSharedSeed0 = SharedSeed (getBytes 8 32)

exampleSharedSeed1 :: SharedSeed
exampleSharedSeed1 = SharedSeed (getBytes 16 32)

exampleSharedSeed2 :: SharedSeed
exampleSharedSeed2 = SharedSeed (getBytes 24 32)

exampleGenesisDelegation :: GenesisDelegation
exampleGenesisDelegation = UnsafeGenesisDelegation (HM.fromList
    [( addressHash issuePubKey
     , UnsafeProxySecretKey
         { pskOmega =
             HeavyDlgIndex $ EpochIndex 68300481033
         , pskIssuerPk = issuePubKey
         , pskDelegatePk =
             PublicKey (CC.XPub { CC.xpubPublicKey = pskDelPubKey
                                , CC.xpubChaincode = pskDelChainCode})
         , pskCert =
             ProxyCert (fromRight (error "Something went wrong") $ sig)
         }
      )]
    )
  where
    issuePubKey = PublicKey (CC.XPub { CC.xpubPublicKey = pskPubKey
                                     , CC.xpubChaincode = pskChainCode})
    sig = CC.xsignature (hexToBS "bae5422af5405e3803154a4ad986da5d14cf624d670\
                                 \1c5c78a79ec73777f74e13973af83752114d9f18166\
                                 \085997fc81e432cab7fee99a275d8bf138ad04e103")
    pskPubKey = hexToBS "e2a1773a2a82d10c30890cbf84eccbdc1aaaee920496424d36e8\
                        \68039d9cb519"
    pskChainCode = CC.ChainCode (hexToBS "21b25efe033d9b00d4f02ccd9cdabcec332\
                                         \abbc6fdf883ca5bf3a8aff4aac27e")
    pskDelPubKey = hexToBS "ddca69bfeac14c013304da88ac032ee63281ab036c1b1b918\
                           \8e4b174b303f43e"
    pskDelChainCode = CC.ChainCode (hexToBS "55163b178e999b9fd50637b2edab8c85\
                                            \8a879ac3c4bd3e610095419a19696573")

exampleGenesisProtocolConstants0 :: GenesisProtocolConstants
exampleGenesisProtocolConstants0 = GenesisProtocolConstants
    { gpcK = 37
    , gpcProtocolMagic = ProtocolMagic {getProtocolMagic = 1783847074}
    , gpcVssMaxTTL = VssMaxTTL {getVssMaxTTL = 1477558317}
    , gpcVssMinTTL = VssMinTTL {getVssMinTTL = 744040476}}

exampleGenesisProtocolConstants1 :: GenesisProtocolConstants
exampleGenesisProtocolConstants1 = GenesisProtocolConstants
    { gpcK = 64
    , gpcProtocolMagic = ProtocolMagic {getProtocolMagic = 135977977}
    , gpcVssMaxTTL = VssMaxTTL {getVssMaxTTL = 126106167}
    , gpcVssMinTTL = VssMinTTL {getVssMinTTL = 310228653}}

exampleGenesisProtocolConstants2 :: GenesisProtocolConstants
exampleGenesisProtocolConstants2 = GenesisProtocolConstants
    { gpcK = 2
    , gpcProtocolMagic = ProtocolMagic {getProtocolMagic = 1780893186}
    , gpcVssMaxTTL = VssMaxTTL {getVssMaxTTL = 402296078}
    , gpcVssMinTTL = VssMinTTL {getVssMinTTL = 1341799941}}

exampleGenesisInitializer0 :: GenesisInitializer
exampleGenesisInitializer0 = GenesisInitializer
    {giTestBalance = TestnetBalanceOptions
        {tboPoors = 2448641325904532856
        , tboRichmen = 14071205313513960321
        , tboTotalBalance = 10953275486128625216
        , tboRichmenShare = 4.2098713311249885
        , tboUseHDAddresses = True}
        , giFakeAvvmBalance = FakeAvvmOptions
            {faoCount = 17853231730478779264
            , faoOneBalance = 15087947214890024355}
            , giAvvmBalanceFactor = CoinPortion
                 {getCoinPortion = 366832547637728}
                 , giUseHeavyDlg = False
                 , giSeed = 0}

exampleGenesisInitializer1 :: GenesisInitializer
exampleGenesisInitializer1 = GenesisInitializer
    { giTestBalance = TestnetBalanceOptions
        { tboPoors = 6042739228893020332
        , tboRichmen = 6300032953394836094
        , tboTotalBalance = 15800731757646603171
        , tboRichmenShare = 3.7001504124217166
        , tboUseHDAddresses = False
        }
    , giFakeAvvmBalance = FakeAvvmOptions
        { faoCount = 1863091022267159357
        , faoOneBalance = 15821780831116413482
        }
    , giAvvmBalanceFactor = CoinPortion
        { getCoinPortion = 794919302027403
        }
    , giUseHeavyDlg = False
    , giSeed = 1
    }

exampleGenesisInitializer2 :: GenesisInitializer
exampleGenesisInitializer2 = GenesisInitializer
    { giTestBalance = TestnetBalanceOptions
        { tboPoors = 354896133480201129
        , tboRichmen = 6001143654143911879
        , tboTotalBalance = 911040566224079710
        , tboRichmenShare = 6.2349745920068775
        , tboUseHDAddresses = False
        }
    , giFakeAvvmBalance = FakeAvvmOptions
        { faoCount = 108112649620073880
        , faoOneBalance = 7896368278086760723
        }
    , giAvvmBalanceFactor = CoinPortion
        { getCoinPortion = 2106983080171
        }
    , giUseHeavyDlg = True
    , giSeed = 3
    }

hexToBS :: Text -> ByteString
hexToBS ts = case B16.decode ts of
    Left err -> error $ "decode failed: " <> show err
    Right bs -> bs
