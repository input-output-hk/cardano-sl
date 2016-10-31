-- | Function related to Mpc messages.

module Pos.Types.Mpc
       ( genCommitmentAndOpening
       , hasCommitment
       , hasOpening
       , hasShares
       , mkSignedCommitment
       , secretToFtsSeed
       , verifyCommitment
       , verifyOpening
       , xorFtsSeed
       ) where

import           Control.Lens        ((^.))
import qualified Data.ByteString     as BS (pack, zipWith)
import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty  (NonEmpty)
import           Universum

import           Pos.Crypto          (PublicKey, Secret, SecretKey, Threshold,
                                      VssPublicKey, genSharedSecret, getDhSecret,
                                      runSecureRandom, secretToDhSecret, sign,
                                      verifyEncShare, verifySecretProof)
import           Pos.Types.Types     (Commitment (..), EpochIndex, FtsSeed (..), MpcData,
                                      Opening (..), SignedCommitment, mdCommitments,
                                      mdOpenings, mdShares)

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

-- | Verify that Secret provided with Opening corresponds to given commitment.
verifyOpening :: Commitment -> Opening -> Bool
verifyOpening Commitment {..} (Opening secret) =
    verifySecretProof commExtra secret commProof

xorFtsSeed :: FtsSeed -> FtsSeed -> FtsSeed
xorFtsSeed (FtsSeed a) (FtsSeed b) =
    FtsSeed $ BS.pack (BS.zipWith xor a b) -- fast due to rewrite rules

-- | Make signed commitment from commitment and epoch index using secret key.
mkSignedCommitment :: SecretKey -> EpochIndex -> Commitment -> SignedCommitment
mkSignedCommitment sk i c = (c, sign sk (i, c))

hasCommitment :: PublicKey -> MpcData -> Bool
hasCommitment pk md = HM.member pk (md ^. mdCommitments)

hasOpening :: PublicKey -> MpcData -> Bool
hasOpening pk md = HM.member pk (md ^. mdOpenings)

hasShares :: PublicKey -> MpcData -> Bool
hasShares pk md = HM.member pk (md ^. mdShares)
