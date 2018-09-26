module Test.Pos.Chain.Ssc.Example
       ( exampleCommitment
       , exampleCommitmentsMap
       , exampleCommitmentSignature
       , exampleInnerSharesMap
       , exampleOpening
       , exampleOpeningsMap
       , exampleSharesDistribution
       , exampleSignedCommitment
       , exampleSscPayload
       , exampleSscProof
       , exampleVssCertificate
       , exampleVssCertificatesHash
       , exampleVssCertificatesMap
       ) where

import           Universum

import qualified Crypto.SCRAPE as Scrape
import           Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import           Data.List ((!!))
import           Data.List.NonEmpty (fromList)

import           Pos.Binary.Class (asBinary)
import           Pos.Chain.Ssc (Commitment, CommitmentSignature, CommitmentsMap,
                     InnerSharesMap, Opening, OpeningsMap, SharesDistribution,
                     SignedCommitment, SscPayload (..), SscProof (..),
                     VssCertificate (..), VssCertificatesHash,
                     VssCertificatesMap (..), mkCommitmentsMap,
                     mkVssCertificate, mkVssCertificatesMap,
                     randCommitmentAndOpening)
import           Pos.Core (EpochIndex (..), addressHash)
import           Pos.Crypto (EncShare (..), ProtocolMagic (..),
                     ProtocolMagicId (..), RequiresNetworkMagic (..),
                     Secret (..), SecretProof (..), SignTag (..), VssKeyPair,
                     VssPublicKey (..), decryptShare, deterministic,
                     deterministicVssKeyGen, hash, sign, toVssPublicKey)

import           Test.Pos.Core.ExampleHelpers (exampleEpochIndex,
                     examplePublicKey, examplePublicKeys, exampleSecretKey,
                     exampleSecretKeys, exampleStakeholderId)
import           Test.Pos.Crypto.Bi (getBytes)

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
      (ProtocolMagic { getProtocolMagicId = ProtocolMagicId 0
                     , getRequiresNetworkMagic = NMMustBeNothing
                     })
      SignForTestingOnly
      exampleSecretKey
      (exampleEpochIndex, exampleCommitment)

exampleCommitmentsMap :: CommitmentsMap
exampleCommitmentsMap =
    let numCommitments = 1
        signedCommitments = replicate numCommitments exampleSignedCommitment
    in  mkCommitmentsMap signedCommitments

exampleOpening :: Opening
exampleOpening = snd exampleCommitmentOpening

exampleOpeningsMap :: OpeningsMap
exampleOpeningsMap =
    let mapSize = 1
        stakeholderIds = replicate mapSize exampleStakeholderId
        openings = replicate mapSize exampleOpening
    in  HM.fromList $ zip stakeholderIds openings

exampleSharesDistribution :: SharesDistribution
exampleSharesDistribution =
    let mapSize = 1
        stakeholderIds = replicate mapSize exampleStakeholderId
        word16s = [1337]
    in  HM.fromList $ zip stakeholderIds word16s

exampleSignedCommitment :: SignedCommitment
exampleSignedCommitment =
    (examplePublicKey, exampleCommitment, exampleCommitmentSignature)

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

exampleVssKeyPairs :: Int -> Int -> [VssKeyPair]
exampleVssKeyPairs offset count = map (toPair . (*offset)) [0..count]
    where
        toPair start = deterministicVssKeyGen (getBytes start 32)

exampleVssPublicKey :: VssPublicKey
exampleVssPublicKey = toVssPublicKey mkVssKeyPair
  where
    mkVssKeyPair = deterministicVssKeyGen $ (getBytes 0 32)

exampleVssCertificate :: VssCertificate
exampleVssCertificate =
    mkVssCertificate
       (ProtocolMagic { getProtocolMagicId = ProtocolMagicId 0
                      , getRequiresNetworkMagic = NMMustBeNothing
                      })
        exampleSecretKey
        (asBinary (toVssPublicKey $ deterministicVssKeyGen ("golden" :: ByteString)))
        (EpochIndex 11)

exampleVssCertificates :: Int -> Int -> [VssCertificate]
exampleVssCertificates offset num =  map vssCert [0..num-1]
    where
        secretKeyList = (exampleSecretKeys offset num)
        vssCert index = mkVssCertificate
                           (ProtocolMagic { getProtocolMagicId = ProtocolMagicId 0
                                          , getRequiresNetworkMagic = NMMustBeNothing
                                          })
                           (secretKeyList !! index)
                           (asBinary (toVssPublicKey $ deterministicVssKeyGen (getBytes index 128)))
                           (EpochIndex 122)

exampleVssCertificatesMap :: Int -> Int -> VssCertificatesMap
exampleVssCertificatesMap offset num = mkVssCertificatesMap $ exampleVssCertificates offset num


exampleVssCertificatesHash :: Int -> Int -> VssCertificatesHash
exampleVssCertificatesHash offset len =
    hash . getVssCertificatesMap $ exampleVssCertificatesMap offset len

exampleSscProof :: SscProof
exampleSscProof = CommitmentsProof (hash exampleCommitmentsMap)
                                   (exampleVssCertificatesHash 10 4)

exampleSscPayload :: SscPayload
exampleSscPayload = SharesPayload exampleSharesMap (exampleVssCertificatesMap 10 4)
  where
    exampleSharesMap = HM.fromList $ [(exampleStakeholderId, exampleInnerSharesMap 3 1)]
