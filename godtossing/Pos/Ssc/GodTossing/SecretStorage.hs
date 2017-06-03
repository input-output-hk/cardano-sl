module Pos.Ssc.GodTossing.SecretStorage
       ( getOurCommitment
       , getOurOpening
       , putOurSecret
       ) where

import           Universum

import           Pos.Binary.GodTossing.Types ()
import           Pos.Core                    (EpochIndex)
import           Pos.DB                      (MonadRealDB)
import           Pos.DB.Misc.Common          (miscGetBi, miscPutBi)
import           Pos.Ssc.GodTossing.Core     (Opening, SignedCommitment)
import           Pos.Ssc.GodTossing.Types    (GtSecretStorage (..))

-- | Get our commitment for given epoch if it's known.
getOurCommitment
    :: MonadRealDB m
    => EpochIndex -> m (Maybe SignedCommitment)
getOurCommitment epoch =
    getSecretStorage <&> \case
        Nothing -> Nothing
        Just GtSecretStorage {..}
            | gssEpoch == epoch -> Just gssCommitment
            | otherwise -> Nothing

-- | Get our opening corresponding for given epoch if it's known.
getOurOpening
    :: MonadRealDB m
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
    :: MonadRealDB m
    => SignedCommitment -> Opening -> EpochIndex -> m ()
putOurSecret comm open epoch =
    putSecretStorage $
    GtSecretStorage {gssCommitment = comm, gssOpening = open, gssEpoch = epoch}

----------------------------------------------------------------------------
-- DB
----------------------------------------------------------------------------

getSecretStorage :: MonadRealDB m => m (Maybe GtSecretStorage)
getSecretStorage = miscGetBi secretStorageKey

putSecretStorage :: MonadRealDB m => GtSecretStorage -> m ()
putSecretStorage = miscPutBi secretStorageKey

secretStorageKey :: ByteString
secretStorageKey = "gtSecretStorageKey"
