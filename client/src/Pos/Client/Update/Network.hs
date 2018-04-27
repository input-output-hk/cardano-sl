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
import           Pos.Core (protocolMagic)
import           Pos.Crypto (SafeSigner, hash, hashHexF)
import           Pos.DB.Class (MonadGState)
import           Pos.Diffusion.Types (Diffusion)
import qualified Pos.Diffusion.Types as Diffusion (Diffusion (sendUpdateProposal, sendVote))
import           Pos.Update (UpId, UpdateProposal, UpdateVote (..), mkUpdateVoteSafe)
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
    let upid  = hash prop
    let votes = [mkUpdateVoteSafe protocolMagic signer upid True | signer <- ss]
    sendUpdateProposal diffusion upid prop votes

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
