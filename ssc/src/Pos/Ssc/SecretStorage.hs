module Pos.Ssc.SecretStorage
       ( getOurCommitment
       , getOurOpening
       , putOurSecret
       ) where

import           Universum

import           Pos.Binary.Ssc.Types ()
import           Pos.Core (EpochIndex)
import           Pos.Core.Ssc (Opening, SignedCommitment)
import           Pos.DB (MonadDB, MonadDBRead)
import           Pos.DB.Misc.Common (miscGetBi, miscPutBi)
import           Pos.Ssc.Types (SscSecretStorage (..))

-- | Get our commitment for given epoch if it's known.
getOurCommitment
    :: MonadDBRead m
    => EpochIndex -> m (Maybe SignedCommitment)
getOurCommitment epoch =
    getSecretStorage <&> \case
        Nothing -> Nothing
        Just SscSecretStorage {..}
            | sssEpoch == epoch -> Just sssCommitment
            | otherwise -> Nothing

-- | Get our opening corresponding for given epoch if it's known.
getOurOpening
    :: MonadDBRead m
    => EpochIndex -> m (Maybe Opening)
getOurOpening epoch =
    getSecretStorage <&> \case
        Nothing -> Nothing
        Just SscSecretStorage {..}
            | sssEpoch == epoch -> Just sssOpening
            | otherwise -> Nothing

-- [FIXME] This function doesn't care about concurrency! It should be atomic.
-- Old code didn't care too, btw.
-- | Put our secret for given epoch.
putOurSecret
    :: MonadDB m
    => SignedCommitment -> Opening -> EpochIndex -> m ()
putOurSecret comm open epoch =
    putSecretStorage $
    SscSecretStorage {sssCommitment = comm, sssOpening = open, sssEpoch = epoch}

----------------------------------------------------------------------------
-- DB
----------------------------------------------------------------------------

getSecretStorage :: MonadDBRead m => m (Maybe SscSecretStorage)
getSecretStorage = miscGetBi secretStorageKey

putSecretStorage :: MonadDB m => SscSecretStorage -> m ()
putSecretStorage = miscPutBi secretStorageKey

secretStorageKey :: ByteString
secretStorageKey = "gtSecretStorageKey"
