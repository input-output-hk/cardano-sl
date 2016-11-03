-- | Function related to Mpc messages.

module Pos.Types.Mpc
       ( genCommitmentAndOpening
       , isCommitmentId
       , isCommitmentIdx
       , isOpeningId
       , isOpeningIdx
       , isSharesId
       , isSharesIdx
       , mkSignedCommitment
       , secretToFtsSeed
       , xorFtsSeed

       -- * Verification
       , verifyCommitment
       , verifyCommitmentSignature
       , verifySignedCommitment
       , verifyOpening
       -- , verifyMpcData
       -- TODO: I moved this to State.Storage to resolve import cycles.
       -- Don't have time to fix it right now because it prevents a merge
       -- and nobody can push to develop until the merge is complete. Bad.
       -- — @neongreen
       ) where

import qualified Data.ByteString     as BS (pack, zipWith)
import qualified Data.HashMap.Strict as HM
import           Data.Ix             (inRange)
import           Data.List.NonEmpty  (NonEmpty)
import           Serokell.Util       (VerificationRes, verifyGeneric)
import           Universum

import           Pos.Constants       (k)
import           Pos.Crypto          (PublicKey, Secret, SecretKey, Threshold,
                                      VssPublicKey, genSharedSecret, getDhSecret,
                                      runSecureRandom, secretToDhSecret, sign, verify,
                                      verifyEncShare, verifySecretProof)
import           Pos.Types.Types     (Commitment (..), EpochIndex, FtsSeed (..),
                                      LocalSlotIndex, Opening (..), SignedCommitment,
                                      SlotId (..))

-- | Convert Secret to FtsSeed.
secretToFtsSeed :: Secret -> FtsSeed
secretToFtsSeed = FtsSeed . getDhSecret . secretToDhSecret

-- | Generate securely random FtsSeed.
genCommitmentAndOpening
    :: MonadIO m
    => Threshold -> NonEmpty VssPublicKey -> m (Commitment, Opening)
genCommitmentAndOpening n pks =
    liftIO . runSecureRandom . fmap convertRes . genSharedSecret n $ pks
  where
    convertRes (extra, secret, proof, shares) =
        ( Commitment
          { commExtra = extra
          , commProof = proof
          , commShares = HM.fromList $ zip (toList pks) shares
          }
        , Opening secret)

-- | Verify that Commitment is correct.
verifyCommitment :: Commitment -> Bool
verifyCommitment Commitment {..} = all verifyCommitmentDo $ HM.toList commShares
  where
    verifyCommitmentDo = uncurry (verifyEncShare commExtra)

-- | Verify signature in SignedCommitment using public key and epoch index.
verifyCommitmentSignature :: PublicKey -> EpochIndex -> SignedCommitment -> Bool
verifyCommitmentSignature pk epoch (comm, commSig) =
    verify pk (epoch, comm) commSig

-- | Verify SignedCommitment using public key and epoch index.
verifySignedCommitment :: PublicKey -> EpochIndex -> SignedCommitment -> VerificationRes
verifySignedCommitment pk epoch sc =
    verifyGeneric
        [ ( verifyCommitmentSignature pk epoch sc
          , "commitment has bad signature (e. g. for wrong epoch)")
        , ( verifyCommitment (fst sc)
          , "commitment itself is bad (e. g. bad shares")
        ]

-- | Verify that Secret provided with Opening corresponds to given commitment.
verifyOpening :: Commitment -> Opening -> Bool
verifyOpening Commitment {..} (Opening secret) =
    verifySecretProof commExtra secret commProof

-- | Apply bitwise xor to two FtsSeeds
xorFtsSeed :: FtsSeed -> FtsSeed -> FtsSeed
xorFtsSeed (FtsSeed a) (FtsSeed b) =
    FtsSeed $ BS.pack (BS.zipWith xor a b) -- fast due to rewrite rules

-- | Make signed commitment from commitment and epoch index using secret key.
mkSignedCommitment :: SecretKey -> EpochIndex -> Commitment -> SignedCommitment
mkSignedCommitment sk i c = (c, sign sk (i, c))

isCommitmentIdx :: LocalSlotIndex -> Bool
isCommitmentIdx = inRange (0, k - 1)

isOpeningIdx :: LocalSlotIndex -> Bool
isOpeningIdx = inRange (2 * k, 3 * k - 1)

isSharesIdx :: LocalSlotIndex -> Bool
isSharesIdx = inRange (4 * k, 5 * k - 1)

isCommitmentId :: SlotId -> Bool
isCommitmentId = isCommitmentIdx . siSlot

isOpeningId :: SlotId -> Bool
isOpeningId = isOpeningIdx . siSlot

isSharesId :: SlotId -> Bool
isSharesId = isSharesIdx . siSlot
