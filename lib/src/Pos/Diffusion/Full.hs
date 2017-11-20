{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RankNTypes          #-}

module Pos.Diffusion.Full
    ( diffusionLayerFull
    ) where

import           Universum hiding (bracket)

import           Control.Monad.Fix (MonadFix)
import qualified Data.Map as M
import           Data.Reflection (give)
import           Formatting (sformat, shown, (%))
import           Mockable (Mockable, Bracket, bracket, Catch, Throw, throw, withAsync)
import           Mockable.Production (Production)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (MsgType (..), Origin (..))
import           Network.QDisc.Fair (fairQDisc)
import qualified Network.Transport as NT (closeTransport)
import           Network.Transport.Abstract (Transport)
import           Network.Transport.Concrete (concrete)
import qualified Network.Transport.TCP as TCP
import           Node (Node, NodeAction (..), simpleNodeEndPoint, NodeEnvironment (..), defaultNodeEnvironment, node, waitForDispatcher)
import           Node.Conversation (Converse, converseWith)
import qualified System.Metrics as Monitoring
import           System.Random (newStdGen)
import           System.Wlog (WithLogger, CanLog, logError, getLoggerName, usingLoggerName)
import qualified System.Wlog as Logger
#ifdef linux_HOST_OS
import qualified System.Systemd.Daemon as Systemd
#endif

import           Pos.Communication (NodeId, VerInfo (..), PeerData, PackingType, EnqueueMsg, makeEnqueueMsg, bipPacking, Listener, MkListeners (..), HandlerSpecs, InSpecs (..), OutSpecs (..))
import           Pos.Communication.Limits (SscLimits (..), UpdateLimits (..), TxpLimits (..), BlockLimits (..), HasUpdateLimits, HasTxpLimits, HasBlockLimits, HasSscLimits)
import           Pos.Communication.Relay.Logic (invReqDataFlowTK)
--import           Pos.Communication.Server (allListeners)
import           Pos.Configuration (HasNodeConfiguration, conversationEstablishTimeout, networkConnectionTimeout)
import           Pos.Core.Block (Block, MainBlockHeader, BlockHeader)
import           Pos.Core.Coin (coinPortionToDouble)
import           Pos.Core.Configuration (protocolMagic)
import           Pos.Core.Types (ProtocolMagic (..), HeaderHash, BlockVersionData (..))
import           Pos.Core.Txp (TxAux)
import           Pos.Core.Update (UpId, UpdateVote, UpdateProposal)
import           Pos.DHT.Real (KademliaDHTInstance, KademliaParams (..), startDHTInstance, stopDHTInstance)
import qualified Pos.Diffusion.Full.Block  as Diffusion.Block
import qualified Pos.Diffusion.Full.Txp    as Diffusion.Txp
import qualified Pos.Diffusion.Full.Update as Diffusion.Update
import           Pos.Diffusion.Full.Types  (DiffusionWorkMode)
import           Pos.Diffusion.Types (Diffusion (..), DiffusionLayer (..), GetBlocksError (..))
import           Pos.Logic.Types (Logic (..))
import           Pos.Network.CLI (NetworkConfigOpts (..), intNetworkConfigOpts)
import           Pos.Network.Types (NetworkConfig (..), Topology (..), Bucket, initQueue)
import           Pos.Ssc.Message (MCOpening, MCShares, MCCommitment, MCVssCertificate)
import           Pos.Update.Configuration (lastKnownBlockVersion)
import           Pos.Util.OutboundQueue (EnqueuedConversation (..))

-- | The full diffusion layer.
--
-- NB: we could do the whole logic/diffusion layer interface using typeclasses
-- against a monad, but we'd end up with a bunch of reader constraints over
-- the values that would otherwise appear in the Logic and Diffusion records.
-- That's to say, we'd have to do the same work anyway, but then even more
-- work to juggle the instances.
diffusionLayerFull
    :: forall d x .
       ( DiffusionWorkMode d
       , MonadFix d
       )
    => NetworkConfigOpts
    -> Maybe Monitoring.Store
    -> ((Logic d -> Production (DiffusionLayer d)) -> Production x)
    -> Production x
diffusionLayerFull networkConfigOpts mEkgStore expectLogic =
    bracket acquire release $ \_ -> expectLogic $ \logic -> do

        -- Read in network topology / configuration / policies.
        networkConfig <- intNetworkConfigOpts networkConfigOpts

        -- Make the outbound queue using network policies.
        oq :: OQ.OutboundQ (EnqueuedConversation d) NodeId Bucket <-
            initQueue networkConfig mEkgStore 

        let -- VerInfo is a diffusion-layer-specific thing. It's only used for
            -- negotiating with peers.
            ourVerInfo :: VerInfo
            ourVerInfo = VerInfo (getProtocolMagic protocolMagic) lastKnownBlockVersion ins outs

            ins :: HandlerSpecs
            InSpecs ins = inSpecs mkL

            -- TODO get OutSpecs that were formerly defined on workers (which
            -- makes no sense with the diffusion/logic split).
            outs :: HandlerSpecs
            OutSpecs outs = outSpecs mkL

            mkL :: MkListeners d
            mkL = error "listeners" -- allListeners oq (ncTopology networkConfig) enqueue

            listeners :: VerInfo -> [Listener d]
            listeners = mkListeners mkL ourVerInfo

            -- Bracket kademlia and network-transport, create a node. This
            -- will be very involved. Should make it top-level I think.
            runDiffusionLayer :: d ()
            runDiffusionLayer = runDiffusionLayerFull networkConfig ourVerInfo oq listeners

            enqueue :: EnqueueMsg d
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
            sendTx = give txpLimits $ Diffusion.Txp.sendTx enqueue

            sendUpdateProposal :: UpId -> UpdateProposal -> [UpdateVote] -> d ()
            sendUpdateProposal = give updateLimits $ Diffusion.Update.sendUpdateProposal enqueue

            sendVote :: UpdateVote -> d ()
            sendVote = give updateLimits $ Diffusion.Update.sendVote enqueue

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

            -- Derive various message size limits from the latest adopted
            -- block version data (provided by the logic layer).

            sscLimits :: SscLimits d
            sscLimits = SscLimits
                -- Copied from existing implementation, formerly in
                -- Pos.Communication.Limits.
                { commitmentsNumLimit = succ . ceiling . recip . coinPortionToDouble . bvdMpcThd <$> getAdoptedBVData logic
                }

            txpLimits :: TxpLimits d
            txpLimits = TxpLimits
                { maxTxSize = bvdMaxTxSize <$> getAdoptedBVData logic
                }

            updateLimits :: UpdateLimits d
            updateLimits = UpdateLimits
                -- Copied from existing implementation, formerly in
                -- Pos.Communication.Limits
                { updateVoteNumLimit = succ . ceiling . recip . coinPortionToDouble . bvdUpdateVoteThd <$> getAdoptedBVData logic
                , maxProposalSize = bvdMaxProposalSize <$> getAdoptedBVData logic
                }

            blockLimits :: BlockLimits d
            blockLimits = BlockLimits
                { maxBlockSize = bvdMaxBlockSize <$> getAdoptedBVData logic
                , maxHeaderSize = bvdMaxHeaderSize <$> getAdoptedBVData logic
                }

            withLimits
                :: ((HasUpdateLimits d, HasTxpLimits d, HasSscLimits d, HasBlockLimits d) => x) -> x
            withLimits x =
                give sscLimits $
                    give txpLimits $
                    give updateLimits $
                    give blockLimits $ x

            diffusion :: Diffusion d
            diffusion = Diffusion {..}

        -- seq withLimits so that Werror doesn't stop compilation.
        return (withLimits `seq` DiffusionLayer {..})

  where
    -- TBD will we need any resources here?
    acquire = pure ()
    release = \_ -> pure ()

-- FIXME TBD move the remainder into a separate module?

-- | Create kademlia, network-transport, and run the outbound queue's
-- dequeue thread.
runDiffusionLayerFull
    :: forall d .
       ( DiffusionWorkMode d, MonadFix d )
    => NetworkConfig KademliaParams
    -> VerInfo
    -> OQ.OutboundQ (EnqueuedConversation d) NodeId Bucket
    -> (VerInfo -> [Listener d])
    -> d ()
runDiffusionLayerFull networkConfig ourVerInfo oq listeners =
    bracketTransport (ncTcpAddr networkConfig) $ \(transport :: Transport d) ->
        bracketKademlia networkConfig $ \_ ->
            timeWarpNode transport ourVerInfo listeners $ \theNode converse ->
                withAsync (OQ.dequeueThread oq (sendMsgFromConverse converse)) $ \_ -> do
                    -- TODO EKG stuff.
                    -- Both logic and diffusion will use EKG so we don't
                    -- set it up here in the diffusion layer.
                    notifyReady
                    -- Wait until the dispatcher finishes (the transport
                    -- shuts down normally or exceptionally).
                    -- The expectation is that some user interface component
                    -- controls when the node goes down, and terminates the
                    -- diffusion layer via asynchronous exception (KillThread).
                    -- In the case of the typical full cardano node, the
                    -- user interface is "wait for control+c".
                    waitForDispatcher theNode

sendMsgFromConverse
    :: Converse PackingType PeerData d
    -> OQ.SendMsg d (EnqueuedConversation d) NodeId
sendMsgFromConverse converse (EnqueuedConversation (_, k)) nodeId =
    converseWith converse nodeId (k nodeId)

-- | Bring up a time-warp node. It will come down when the continuation ends.
timeWarpNode
    :: forall d t .
       ( DiffusionWorkMode d, MonadFix d )
    => Transport d
    -> VerInfo
    -> (VerInfo -> [Listener d])
    -> (Node d -> Converse PackingType PeerData d -> d t)
    -> d t
timeWarpNode transport ourVerInfo listeners k = do
    stdGen <- liftIO newStdGen
    node mkTransport mkReceiveDelay mkConnectDelay stdGen bipPacking ourVerInfo nodeEnv $ \theNode ->
        NodeAction listeners $ k theNode
  where
    mkTransport = simpleNodeEndPoint transport
    mkReceiveDelay = const (pure Nothing)
    mkConnectDelay = const (pure Nothing)
    nodeEnv = defaultNodeEnvironment { nodeAckTimeout = conversationEstablishTimeout }

----------------------------------------------------------------------------
-- Kademlia
----------------------------------------------------------------------------

createKademliaInstance ::
       (HasNodeConfiguration, MonadIO m, Mockable Catch m, Mockable Throw m, CanLog m)
    => KademliaParams
    -> Word16 -- ^ Default port to bind to.
    -> m KademliaDHTInstance
createKademliaInstance kp defaultPort =
    usingLoggerName "kademlia" (startDHTInstance instConfig defaultBindAddress)
  where
    instConfig = kp {kpPeers = ordNub $ kpPeers kp}
    defaultBindAddress = ("0.0.0.0", defaultPort)

-- | RAII for 'KademliaDHTInstance'.
bracketKademliaInstance
    :: (HasNodeConfiguration, MonadIO m, Mockable Catch m, Mockable Throw m, Mockable Bracket m, CanLog m)
    => KademliaParams
    -> Word16 -- ^ Default port to bind to.
    -> (KademliaDHTInstance -> m a)
    -> m a
bracketKademliaInstance kp defaultPort action =
    bracket (createKademliaInstance kp defaultPort) stopDHTInstance action

-- | The 'NodeParams' contain enough information to determine whether a Kademlia
-- instance should be brought up. Use this to safely acquire/release one.
bracketKademlia
    :: (HasNodeConfiguration, MonadIO m, Mockable Catch m, Mockable Throw m, Mockable Bracket m, CanLog m)
    => NetworkConfig KademliaParams
    -> (NetworkConfig KademliaDHTInstance -> m a)
    -> m a
bracketKademlia nc@NetworkConfig {..} action = case ncTopology of
    -- cases that need Kademlia
    TopologyP2P{topologyKademlia = kp, ..} ->
      bracketKademliaInstance kp ncDefaultPort $ \kinst ->
        k $ TopologyP2P{topologyKademlia = kinst, ..}
    TopologyTraditional{topologyKademlia = kp, ..} ->
      bracketKademliaInstance kp ncDefaultPort $ \kinst ->
        k $ TopologyTraditional{topologyKademlia = kinst, ..}
    TopologyRelay{topologyOptKademlia = Just kp, ..} ->
      bracketKademliaInstance kp ncDefaultPort $ \kinst ->
        k $ TopologyRelay{topologyOptKademlia = Just kinst, ..}
    TopologyCore{topologyOptKademlia = Just kp, ..} ->
      bracketKademliaInstance kp ncDefaultPort $ \kinst ->
        k $ TopologyCore{topologyOptKademlia = Just kinst, ..}

    -- cases that don't
    TopologyRelay{topologyOptKademlia = Nothing, ..} ->
        k $ TopologyRelay{topologyOptKademlia = Nothing, ..}
    TopologyCore{topologyOptKademlia = Nothing, ..} ->
        k $ TopologyCore{topologyOptKademlia = Nothing, ..}
    TopologyBehindNAT{..} ->
        k $ TopologyBehindNAT{..}
    TopologyAuxx{..} ->
        k $ TopologyAuxx{..}
  where
    k topology = action (nc { ncTopology = topology })

data MissingKademliaParams = MissingKademliaParams
    deriving (Show)

instance Exception MissingKademliaParams

----------------------------------------------------------------------------
-- Transport
----------------------------------------------------------------------------

createTransportTCP
    :: (HasNodeConfiguration, MonadIO n, MonadIO m, WithLogger m, Mockable Throw m)
    => TCP.TCPAddr
    -> m (Transport n, m ())
createTransportTCP addrInfo = do
    loggerName <- getLoggerName
    let tcpParams =
            (TCP.defaultTCPParameters
             { TCP.transportConnectTimeout =
                   Just $ fromIntegral networkConnectionTimeout
             , TCP.tcpNewQDisc = fairQDisc $ \_ -> return Nothing
             -- Will check the peer's claimed host against the observed host
             -- when new connections are made. This prevents an easy denial
             -- of service attack.
             , TCP.tcpCheckPeerHost = True
             , TCP.tcpServerExceptionHandler = \e ->
                     usingLoggerName (loggerName <> "transport") $
                         logError $ sformat ("Exception in tcp server: " % shown) e
             })
    transportE <-
        liftIO $ TCP.createTransport addrInfo tcpParams
    case transportE of
        Left e -> do
            logError $ sformat ("Error creating TCP transport: " % shown) e
            throw e
        Right transport -> return (concrete transport, liftIO $ NT.closeTransport transport)

-- | RAII for 'Transport'.
bracketTransport
    :: (HasNodeConfiguration, MonadIO m, MonadIO n, Mockable Throw m, Mockable Bracket m, WithLogger m)
    => TCP.TCPAddr
    -> (Transport n -> m a)
    -> m a
bracketTransport tcpAddr k =
    bracket (createTransportTCP tcpAddr) snd (k . fst)

-- | Notify process manager tools like systemd the node is ready.
-- Available only on Linux for systems where `libsystemd-dev` is installed.
-- It defaults to a noop for all the other platforms.
notifyReady :: (MonadIO m, WithLogger m) => m ()
#ifdef linux_HOST_OS
notifyReady = do
    res <- liftIO Systemd.notifyReady
    case res of
        Just () -> return ()
        Nothing -> Logger.logWarning "notifyReady failed to notify systemd."
#else
notifyReady = return ()
#endif
