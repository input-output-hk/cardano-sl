-- | Function related to Mpc messages.

module Pos.Types.Mpc
       ( genCommitmentAndOpening
       , secretToFtsSeed
       , verifyCommitment
       , verifyOpening
       , xorFtsSeed
       ) where

import qualified Data.ByteString     as BS (pack, zipWith)
import qualified Data.HashMap.Strict as HM
import           Universum

import           Pos.Crypto          (Secret, Threshold, VssPublicKey, genSharedSecret,
                                      getDhSecret, runSecureRandom, secretToDhSecret,
                                      verifyEncShare, verifySecretProof)
import           Pos.Types.Types     (Commitment (..), FtsSeed (..), Opening (..))

-- | Convert Secret to FtsSeed.
secretToFtsSeed :: Secret -> FtsSeed
secretToFtsSeed = FtsSeed . getDhSecret . secretToDhSecret

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

xorFtsSeed :: FtsSeed -> FtsSeed -> FtsSeed
xorFtsSeed (FtsSeed a) (FtsSeed b) =
    FtsSeed $ BS.pack (BS.zipWith xor a b) -- fast due to rewrite rules
