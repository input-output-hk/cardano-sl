module Pos.Ssc.GodTossing.SecretStorage
       ( getOurCommitments
       , getOurOpening
       , addOurCommitment
       , ssSetNewEpoch
       ) where

import           Control.Lens             ((^.), _2)
import qualified Data.HashMap.Strict      as HM
import           Universum

import           Pos.Crypto               (SecretProof)
import           Pos.DB                   (MonadDB)
import           Pos.DB.Misc              (getSecretStorage, putSecretStorage)
import           Pos.Ssc.GodTossing.Types (Commitment (commProof), GtSecretStorage (..),
                                           Opening, SignedCommitment, SscGodTossing)
import           Pos.Types                (EpochIndex)
import           Pos.Util                 (AsBinary)

getOurCommitments :: MonadDB SscGodTossing m => EpochIndex -> m [SignedCommitment]
getOurCommitments epoch = do
    getOurCommitmentsDo <$> getSecretStorage
  where
    getOurCommitmentsDo GtSecretStorage {..}
        | gssEpoch == epoch = map fst . toList $ gssSecrets
        | otherwise = []

getOurOpening
    :: MonadDB SscGodTossing m
    => AsBinary SecretProof -> m (Maybe Opening)
getOurOpening proof = fmap snd . HM.lookup proof . gssSecrets <$> getSecretStorage

-- [FIXME] This function doesn't care about concurrency! It should be atomic.
-- Old code didn't care too, btw.
addOurCommitment
    :: MonadDB SscGodTossing m
    => SignedCommitment -> Opening -> EpochIndex -> m ()
addOurCommitment comm open epoch = do
    GtSecretStorage {..} <- getSecretStorage
    if | epoch /= gssEpoch -> pass
       | otherwise ->
           putSecretStorage
               GtSecretStorage
               { gssSecrets =
                     HM.insert (commProof $ comm ^. _2) (comm, open) gssSecrets
               , ..
               }

ssSetNewEpoch :: MonadDB SscGodTossing m => EpochIndex -> m ()
ssSetNewEpoch epoch = do
    GtSecretStorage {..} <- getSecretStorage
    if | epoch == gssEpoch -> pass
       | otherwise ->
           putSecretStorage
               GtSecretStorage {gssSecrets = mempty, gssEpoch = epoch}
