-- | Function related to Mpc messages.

module Pos.Types.Mpc
       ( ftsSeedLength
       , secretToFtsSeed
       , genCommitmentAndOpening
       , verifyCommitment
       , verifyOpening
       ) where

import           Control.Lens        (over, _1)
import qualified Data.Binary         as Bin
import qualified Data.HashMap.Strict as HM
import           Universum

import           Pos.Crypto          (EncShare, Secret, SecretSharingExtra, Threshold,
                                      VssPublicKey, genSharedSecret, runSecureRandom,
                                      verifyEncShare, verifySecretProof)
import           Pos.Types.Types     (Commitment (..), FtsSeed (..), Opening (..))

-- | Length of FtsSeed which is currently constant known in compile
-- time.
-- FIXME: do something!
ftsSeedLength :: Integral a => a
ftsSeedLength = 40

-- | Convert Secret to FtsSeed.
-- FIXME: result is 33 bytes long!
secretToFtsSeed :: Secret -> FtsSeed
secretToFtsSeed = FtsSeed . toS . Bin.encode

-- | Generate securely random FtsSeed.
genCommitmentAndOpening
    :: MonadIO m
    => Threshold -> [VssPublicKey] -> m (Commitment, Opening)
genCommitmentAndOpening n pks =
    liftIO . runSecureRandom . fmap convertRes . genSharedSecret n $ pks
  where
    convertRes (extra, secret, proof, shares) =
        ( Commitment
          { commExtra = extra
          , commProof = proof
          , commShares = HM.fromList $ zip pks shares
          }
        , Opening secret)

-- | Verify that Commitment is correct.
verifyCommitment :: Commitment -> Bool
verifyCommitment Commitment {..} = all verifyCommitmentDo $ HM.toList commShares
  where
    verifyCommitmentDo = uncurry (verifyEncShare commExtra)

-- | Verify that Secret provided with Opening corresponds to given commitment.
verifyOpening :: Commitment -> Opening -> Bool
verifyOpening Commitment {..} (Opening secret) =
    verifySecretProof commExtra secret commProof
