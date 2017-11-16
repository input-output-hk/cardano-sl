{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module Pos.Diffusion.Full
    ( diffusionLayerFull
    ) where

import           Universum

import qualified Data.Map as M
import           Mockable (throw)
import           Mockable.Production (Production)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (MsgType (..), Origin (..))
import qualified System.Metrics as Monitoring

import           Pos.Communication          (NodeId, VerInfo, EnqueueMsg, makeEnqueueMsg)
import           Pos.Communication.Relay.Logic (invReqDataFlowTK)
import           Pos.Core.Block (Block, MainBlockHeader, BlockHeader)
import           Pos.Core.Types (HeaderHash)
import           Pos.Core.Txp (TxAux)
import           Pos.Core.Update (UpId, UpdateVote, UpdateProposal)
import qualified Pos.Diffusion.Full.Block  as Diffusion.Block
import qualified Pos.Diffusion.Full.Txp    as Diffusion.Txp
import qualified Pos.Diffusion.Full.Update as Diffusion.Update
import           Pos.Diffusion.Types (Diffusion (..), DiffusionLayer (..), GetBlocksError (..))
import           Pos.Logic.Types (Logic (..))
import           Pos.Network.CLI (NetworkConfigOpts (..), intNetworkConfigOpts)
import           Pos.Network.Types (Bucket, initQueue)
import           Pos.Ssc.Message (MCOpening, MCShares, MCCommitment, MCVssCertificate)
import           Pos.Util.OutboundQueue (EnqueuedConversation (..))
import           Pos.WorkMode (WorkMode)

-- | The full diffusion layer.
--
-- Constraints on 'd' arise from existing implementation of various things like
-- 'invReqDataFlowTK'. These represent in essence other logic layer dependencies
-- expressed against the monad itself rather than contained in a record.
-- Thankfully 'WorkMode ctx d' satisfies them all.
--
-- NB: we could do the whole logic/diffusion layer interface using typeclasses
-- against a monad, but we'd end up with a bunch of reader constraints over
-- the values that would otherwise appear in the Logic and Diffusion records.
-- That's to say, we'd have to do the same work anyway, but then even more
-- work to juggle the instances.
diffusionLayerFull
    :: forall ctx d x .
       ( WorkMode ctx d )
    => NetworkConfigOpts
    -> Maybe Monitoring.Store
    -> VerInfo
    -> ((Logic d -> Production (DiffusionLayer d)) -> Production x)
    -> Production x
diffusionLayerFull networkConfigOpts mEkgStore ourVerInfo expectLogic =
    bracket acquire release $ \_ -> expectLogic $ \logic -> do

        -- Read in network topology / configuration / policies.
        networkConfig <- intNetworkConfigOpts networkConfigOpts

        -- Make the outbound queue using network policies.
        oq :: OQ.OutboundQ (EnqueuedConversation d) NodeId Bucket <-
            initQueue networkConfig mEkgStore 

        let enqueue :: EnqueueMsg d
            enqueue = makeEnqueueMsg ourVerInfo $ \msgType k -> do
                itList <- OQ.enqueue oq msgType (EnqueuedConversation (msgType, k))
                let itMap = M.fromList itList
                return ((>>= either throw return) <$> itMap)

            getBlocks :: NodeId
                      -> BlockHeader
                      -> [HeaderHash]
                      -> d (Either GetBlocksError [Block])
            getBlocks = Diffusion.Block.getBlocks logic enqueue

            requestTip :: (BlockHeader -> NodeId -> d t) -> d (Map NodeId (d t))
            requestTip = Diffusion.Block.requestTip enqueue

            announceBlock :: MainBlockHeader -> d ()
            announceBlock = void . Diffusion.Block.announceBlock logic enqueue

            sendTx :: TxAux -> d Bool
            sendTx = Diffusion.Txp.sendTx enqueue

            sendUpdateProposal :: UpId -> UpdateProposal -> [UpdateVote] -> d ()
            sendUpdateProposal = Diffusion.Update.sendUpdateProposal enqueue

            sendVote :: UpdateVote -> d ()
            sendVote = Diffusion.Update.sendVote enqueue

            -- FIXME
            -- SSC stuff has a 'waitUntilSend' motif before it. Must remember to
            -- investigate that and port it if necessary...
            -- No, it really should be the logic layer which decides when to send
            -- things.
            --
            -- TODO put these into a Pos.Diffusion.Full.Ssc module.
            sendSscCert :: MCVssCertificate -> d ()
            sendSscCert = void . invReqDataFlowTK "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic)

            sendSscOpening :: MCOpening -> d ()
            sendSscOpening = void . invReqDataFlowTK "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic)

            sendSscShares :: MCShares -> d ()
            sendSscShares = void . invReqDataFlowTK "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic)

            sendSscCommitment :: MCCommitment -> d ()
            sendSscCommitment = void . invReqDataFlowTK "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic)

            -- Bracket kademlia and network-transport, create a node. This
            -- will be very involved. Should make it top-level I think.
            runDiffusionLayer :: d ()
            runDiffusionLayer = runDiffusionLayerFull oq

            diffusion :: Diffusion d
            diffusion = Diffusion {..}

        return DiffusionLayer {..}

  where
    -- TBD will we need any resources here?
    acquire = pure ()
    release = \_ -> pure ()

-- | Create kademlia, network-transport, and run the outbound queue's
-- dequeue thread.
runDiffusionLayerFull :: Applicative d => OQ.OutboundQ (EnqueuedConversation d) NodeId Bucket -> d ()
runDiffusionLayerFull _ = pure ()
