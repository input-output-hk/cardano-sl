-- | Functions for operating with messages of update system

{-# LANGUAGE RankNTypes #-}

module Pos.Client.Update.Network
       ( submitVote
       , submitUpdateProposal
       ) where

import           Universum

import           Formatting (sformat, (%))
import           System.Wlog (logInfo)

import           Pos.Crypto (ProtocolMagic, SafeSigner, hash, hashHexF)
import           Pos.Infra.Diffusion.Types (Diffusion)
import qualified Pos.Infra.Diffusion.Types as Diffusion
                     (Diffusion (sendUpdateProposal, sendVote))
import           Pos.Update (UpId, UpdateProposal, UpdateVote (..),
                     mkUpdateVoteSafe)
import           Pos.WorkMode.Class (MinWorkMode)

-- | Send UpdateVote to given addresses
submitVote
    :: Diffusion m
    -> UpdateVote
    -> m ()
submitVote diffusion = Diffusion.sendVote diffusion

-- | Send UpdateProposal with one positive vote to given addresses
submitUpdateProposal
    :: (MinWorkMode m)
    => ProtocolMagic
    -> Diffusion m
    -> [SafeSigner]
    -> UpdateProposal
    -> m ()
submitUpdateProposal pm diffusion ss prop = do
    let upid  = hash prop
    let votes = [mkUpdateVoteSafe pm signer upid True | signer <- ss]
    sendUpdateProposal diffusion upid prop votes

-- Send UpdateProposal to given address.
sendUpdateProposal
    :: (MinWorkMode m)
    => Diffusion m
    -> UpId
    -> UpdateProposal
    -> [UpdateVote]
    -> m ()
sendUpdateProposal diffusion upid proposal votes = do
    logInfo $ sformat ("Announcing proposal with id "%hashHexF) upid
    Diffusion.sendUpdateProposal diffusion upid proposal votes
