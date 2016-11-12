{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications       #-}

module Pos.Ssc.DynamicState.State where

import           Pos.State.State          (WorkModeDB, queryDisk, updateDisk)
import           Pos.Crypto               (SecretKey)
import Pos.Types (EpochIndex)
import qualified Pos.State.Acidic         as A
import           Pos.Ssc.DynamicState.Base    (Opening, SignedCommitment)
import           Universum
import           Pos.Crypto                   (Share, VssKeyPair,
                                               PublicKey, toPublic)
import Pos.Ssc.DynamicState.Instance          (SscDynamicState)
import Pos.Ssc.DynamicState.Base              (mkSignedCommitment, genCommitmentAndOpening)
import           Control.Lens                 (view, _2, _3)
import           Crypto.Random                (seedNew, seedToInteger)

--TODO next code in this module look like boolshit, probably it should move to Pos.State.State
 
getSecret :: WorkModeDB SscDynamicState m 
          => m (Maybe (PublicKey, SignedCommitment, Opening))
getSecret = queryDisk @SscDynamicState A.GetToken

-- | Generate new commitment and opening and use them for the current
-- epoch. Assumes that the genesis block has already been generated and
-- processed by MPC (when the genesis block is processed, the secret is
-- cleared) (otherwise 'generateNewSecret' will fail because 'A.SetSecret'
-- won't set the secret if there's one already).
-- Nothing is returned if node is not ready.

generateAndSetNewSecret
    :: WorkModeDB SscDynamicState m
    => SecretKey
    -> EpochIndex                         -- ^ Current epoch
    -> m (Maybe (SignedCommitment, Opening))
generateAndSetNewSecret sk epoch = do
    -- TODO: I think it's safe here to perform 3 operations which aren't
    -- grouped into a single transaction here, but I'm still a bit nervous.
    threshold <- queryDisk @SscDynamicState (A.GetThreshold epoch)
    participants <- queryDisk @SscDynamicState (A.GetParticipants epoch)
    case (,) <$> threshold <*> participants of
        Nothing -> return Nothing
        Just (th, ps) -> do
            (comm, op) <-
                first (mkSignedCommitment sk epoch) <$>
                genCommitmentAndOpening th ps
            Just (comm, op) <$ updateDisk @SscDynamicState (A.SetToken (toPublic sk, comm, op))


getOurCommitment :: WorkModeDB SscDynamicState m => m (Maybe SignedCommitment)
getOurCommitment = fmap (view _2) <$> getSecret

getOurOpening :: WorkModeDB SscDynamicState m => m (Maybe Opening)
getOurOpening = fmap (view _3) <$> getSecret

getOurShares :: WorkModeDB SscDynamicState m => VssKeyPair -> m (HashMap PublicKey Share)
getOurShares ourKey = do
    randSeed <- liftIO seedNew
    queryDisk @SscDynamicState $ A.GetOurShares ourKey (seedToInteger randSeed)
