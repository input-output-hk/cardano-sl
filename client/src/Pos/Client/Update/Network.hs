-- | Functions for operating with messages of update system

{-# LANGUAGE RankNTypes #-}

module Pos.Client.Update.Network
       ( submitVote
       , submitUpdateProposal
       ) where

import           Universum

import           Formatting (sformat, (%))
import           System.Wlog (logInfo)

import           Pos.Communication.Message ()
import           Pos.Crypto (SafeSigner, SignTag (SignUSVote), hash, hashHexF, safeSign,
                             safeToPublic)
import           Pos.DB.Class (MonadGState)
import           Pos.Diffusion.Types (Diffusion)
import qualified Pos.Diffusion.Types as Diffusion (Diffusion (sendUpdateProposal, sendVote))
import           Pos.Update (UpId, UpdateProposal, UpdateVote (..))
import           Pos.WorkMode.Class (MinWorkMode)

-- | Send UpdateVote to given addresses
submitVote
    :: (MinWorkMode m, MonadGState m)
    => Diffusion m
    -> UpdateVote
    -> m ()
submitVote diffusion = Diffusion.sendVote diffusion

-- | Send UpdateProposal with one positive vote to given addresses
submitUpdateProposal
    :: (MinWorkMode m, MonadGState m)
    => Diffusion m
    -> [SafeSigner]
    -> UpdateProposal
    -> m ()
submitUpdateProposal diffusion ss prop = do
    let upid = hash prop
    let votes = constructVotes upid
    sendUpdateProposal diffusion upid prop votes
  where
    createVote upid (signer, signature) =
        UpdateVote
            { uvKey        = safeToPublic signer
            , uvProposalId = upid
            , uvDecision   = True
            , uvSignature  = signature
            }
    constructVotes :: UpId -> [UpdateVote]
    constructVotes upid =
        let signatures = map (\s -> safeSign SignUSVote s (upid, True)) ss
        in map (createVote upid) (zip ss signatures)

-- Send UpdateProposal to given address.
sendUpdateProposal
    :: (MinWorkMode m, MonadGState m)
    => Diffusion m
    -> UpId
    -> UpdateProposal
    -> [UpdateVote]
    -> m ()
sendUpdateProposal diffusion upid proposal votes = do
    logInfo $ sformat ("Announcing proposal with id "%hashHexF) upid
    Diffusion.sendUpdateProposal diffusion upid proposal votes
