module Pos.Ssc.GodTossing.SecretStorage
       ( getOurCommitment
       , getOurOpening
       , putOurSecret
       ) where

import           Universum

import           Pos.DB                   (MonadDB)
import           Pos.DB.Misc              (getSecretStorage, putSecretStorage)
import           Pos.Ssc.GodTossing.Core  (Opening, SignedCommitment)
import           Pos.Ssc.GodTossing.Types (GtSecretStorage (..), SscGodTossing)
import           Pos.Types                (EpochIndex)

-- | Get our commitment for given epoch if it's known.
getOurCommitment
    :: MonadDB SscGodTossing m
    => EpochIndex -> m (Maybe SignedCommitment)
getOurCommitment epoch =
    getSecretStorage <&> \case
        Nothing -> Nothing
        Just GtSecretStorage {..}
            | gssEpoch == epoch -> Just gssCommitment
            | otherwise -> Nothing

-- | Get our opening corresponding for given epoch if it's known.
getOurOpening
    :: MonadDB SscGodTossing m
    => EpochIndex -> m (Maybe Opening)
getOurOpening epoch =
    getSecretStorage <&> \case
        Nothing -> Nothing
        Just GtSecretStorage {..}
            | gssEpoch == epoch -> Just gssOpening
            | otherwise -> Nothing

-- [FIXME] This function doesn't care about concurrency! It should be atomic.
-- Old code didn't care too, btw.
-- | Put our secret for given epoch.
putOurSecret
    :: MonadDB SscGodTossing m
    => SignedCommitment -> Opening -> EpochIndex -> m ()
putOurSecret comm open epoch =
    putSecretStorage $
    GtSecretStorage {gssCommitment = comm, gssOpening = open, gssEpoch = epoch}
