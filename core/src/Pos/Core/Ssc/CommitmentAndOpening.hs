module Pos.Core.Ssc.CommitmentAndOpening
       ( randCommitmentAndOpening
       ) where

import           Universum

import qualified Crypto.Random as Rand
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import           Formatting (build, sformat, (%))

import           Pos.Binary.Class (asBinary)
import           Pos.Core.Ssc.Commitment (Commitment (..))
import           Pos.Core.Ssc.Opening (Opening (..))
import           Pos.Crypto (Threshold, VssPublicKey, genSharedSecret)


-- | Generate random SharedSeed.
randCommitmentAndOpening
    :: Rand.MonadRandom m
    => Threshold -> NonEmpty VssPublicKey -> m (Commitment, Opening)
randCommitmentAndOpening t pks
    | t <= 1 = error $ sformat
        ("randCommitmentAndOpening: threshold ("%build%") must be > 1") t
    | t >= n - 1 = error $ sformat
        ("randCommitmentAndOpening: threshold ("%build%") must be < n-1"%
         " (n = "%build%")") t n
    | otherwise = convertRes <$> genSharedSecret t pks
  where
    n = fromIntegral (length pks)
    convertRes (secret, proof, shares) =
        ( Commitment
          { commProof = proof
          , commShares = HM.fromList $ map toPair $ NE.groupWith fst shares
          }
        , Opening $ asBinary secret)
    toPair ne@(x:|_) = (asBinary (fst x), NE.map (asBinary . snd) ne)
