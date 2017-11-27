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
import           Node (Node, NodeAction (..), simpleNodeEndPoint, NodeEnvironment (..), defaultNodeEnvironment, node)
import           Node.Conversation (Converse, converseWith, Conversation)
import qualified System.Metrics as Monitoring
import           System.Random (newStdGen)
import           System.Wlog (WithLogger, CanLog, logError, getLoggerName, usingLoggerName)

import           Pos.Block.Network.Types (MsgGetHeaders, MsgHeaders, MsgGetBlocks, MsgBlock)
import           Pos.Communication (NodeId, VerInfo (..), PeerData, PackingType, EnqueueMsg, makeEnqueueMsg, bipPacking, Listener, MkListeners (..), HandlerSpecs, InSpecs (..), OutSpecs (..), createOutSpecs, toOutSpecs, convH, InvOrDataTK, MsgSubscribe, makeSendActions, SendActions, Msg)
import           Pos.Communication.Limits (SscLimits (..), UpdateLimits (..), TxpLimits (..), BlockLimits (..))
import           Pos.Communication.Relay.Logic (invReqDataFlowTK)
import           Pos.Communication.Util (wrapListener)
import           Pos.Configuration (HasNodeConfiguration, conversationEstablishTimeout, networkConnectionTimeout)
import           Pos.Core.Block (Block, MainBlockHeader, BlockHeader)
import           Pos.Core.Coin (coinPortionToDouble)
import           Pos.Core.Configuration (protocolMagic)
import           Pos.Core.Types (HeaderHash, BlockVersionData (..), StakeholderId)
import           Pos.Core.Txp (TxAux)
import           Pos.Core.Update (UpId, UpdateVote, UpdateProposal)
import           Pos.Crypto.Configuration (ProtocolMagic (..))
import           Pos.DHT.Real (KademliaDHTInstance, KademliaParams (..), startDHTInstance, stopDHTInstance)
import qualified Pos.Diffusion.Full.Block      as Diffusion.Block
import qualified Pos.Diffusion.Full.Delegation as Diffusion.Delegation
import qualified Pos.Diffusion.Full.Ssc        as Diffusion.Ssc
import qualified Pos.Diffusion.Full.Txp        as Diffusion.Txp
import qualified Pos.Diffusion.Full.Update     as Diffusion.Update
import           Pos.Diffusion.Full.Types  (DiffusionWorkMode)
import           Pos.Diffusion.Types (Diffusion (..), DiffusionLayer (..), GetBlocksError (..))
import           Pos.Logic.Types (Logic (..))
import           Pos.Network.CLI (NetworkConfigOpts (..), intNetworkConfigOpts)
import           Pos.Network.Types (NetworkConfig (..), Topology (..), Bucket, initQueue,
                                    topologySubscribers, SubscriptionWorker (..),
                                    topologySubscriptionWorker)
import           Pos.Ssc.Message (MCOpening, MCShares, MCCommitment, MCVssCertificate)
import           Pos.Diffusion.Subscription.Common (subscriptionListeners)
import           Pos.Diffusion.Subscription.Dns (dnsSubscriptionWorker)
import           Pos.Diffusion.Subscription.Dht (dhtSubscriptionWorker)
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
            ourVerInfo = VerInfo (getProtocolMagic protocolMagic) lastKnownBlockVersion ins (outs <> workerOuts)

            ins :: HandlerSpecs
            InSpecs ins = inSpecs mkL

            -- The out specs come not just from listeners but also from workers.
            -- Workers in the existing implementation were bundled up in
            --   allWorkers :: ([WorkerSpec m], OutSpecs)
            -- and they performed logic layer tasks, so having out specs defined
            -- by them doesn't make sense.
            -- For the first iteration, we just dump those out specs here, since
            -- we know in the diffusion layer the set of all requests that might
            -- be made.
            --
            -- Find below a definition of each of the worker out specs,
            -- copied from Pos.Worker (allWorkers). Each one was manually
            -- inspected to determine the out specs.
            --
            -- FIXME this system must change. Perhaps replace it with a
            -- version number?
            outs :: HandlerSpecs
            OutSpecs outs = outSpecs mkL

            workerOuts :: HandlerSpecs
            OutSpecs workerOuts = mconcat
                [ sscWorkerOutSpecs
                , securityWorkerOutSpecs
                , usWorkerOutSpecs
                , blockWorkerOutSpecs
                , delegationWorkerOutSpecs
                , slottingWorkerOutSpecs
                , subscriptionWorkerOutSpecs
                , dhtWorkerOutSpecs
                ]

            -- An onNewSlotWorker and a localWorker. Latter is mempty. Former
            -- actually does the ssc stuff.
            sscWorkerOutSpecs = mconcat
                [ createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCCommitment))
                , createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCOpening))
                , createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCShares))
                , createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCVssCertificate))
                ]

            -- A single worker checkForReceivedBlocksWorker with
            -- requestTipOuts from Pos.Block.Network.
            securityWorkerOutSpecs = toOutSpecs
                [ convH (Proxy :: Proxy MsgGetHeaders)
                        (Proxy :: Proxy MsgHeaders)
                ]

            -- Definition of usWorkers plainly shows the out specs = mempty.
            usWorkerOutSpecs = mempty

            -- announceBlockOuts from blkCreatorWorker
            -- announceBlockOuts from blkMetricCheckerWorker
            -- along with the retrieval worker outs which also include
            -- announceBlockouts.
            blockWorkerOutSpecs = mconcat
                [ announceBlockOuts
                , announceBlockOuts
                , announceBlockOuts <> toOutSpecs [ convH (Proxy :: Proxy MsgGetBlocks)
                                                          (Proxy :: Proxy MsgBlock)
                                                  ]
                ]

            announceBlockOuts = toOutSpecs [ convH (Proxy :: Proxy MsgHeaders)
                                                   (Proxy :: Proxy MsgGetHeaders)
                                           ]

            -- It's a local worker, no out specs.
            delegationWorkerOutSpecs = mempty

            -- Plainly mempty from the definition of allWorkers.
            slottingWorkerOutSpecs = mempty

            -- Copied from existing implementation but
            -- FIXME it will be wrong when the patch to include a keepalive
            -- is merged. That shall be the first test of this inspec/outspec
            -- system I suppose.
            subscriptionWorkerOutSpecs = case topologySubscriptionWorker (ncTopology networkConfig) of
                Just (SubscriptionWorkerBehindNAT _) -> specs
                Just (SubscriptionWorkerKademlia __ _ _ _) -> specs
                _ -> mempty
              where
                specs = toOutSpecs [ convH (Proxy @MsgSubscribe) (Proxy @Void) ]

            -- It's a localOnNewSlotWorker, so mempty.
            dhtWorkerOutSpecs = mempty

            mkL :: MkListeners d
            --mkL = error "listeners" -- allListeners oq (ncTopology networkConfig) enqueue
            mkL = mconcat $
                [ lmodifier "block"       $ give blockLimits $ Diffusion.Block.blockListeners logic oq
                , lmodifier "tx"          $ give txpLimits $ Diffusion.Txp.txListeners logic oq enqueue
                , lmodifier "update"      $ give updateLimits $ Diffusion.Update.updateListeners logic oq enqueue
                , lmodifier "delegation"  $ Diffusion.Delegation.delegationListeners logic oq enqueue
                , lmodifier "ssc"         $ give sscLimits $ Diffusion.Ssc.sscListeners logic oq enqueue
                ] ++ [
                  lmodifier "subscription" $ subscriptionListeners oq subscriberNodeType
                | Just (subscriberNodeType, _) <- [topologySubscribers (ncTopology networkConfig)]
                ]

            lmodifier lname mkLs = mkLs { mkListeners = mkListeners' }
              where
                mkListeners' v p =
                    let ls = mkListeners mkLs v p
                        f = wrapListener ("server" <> lname)
                    in  map f ls

            listeners :: VerInfo -> [Listener d]
            listeners = mkListeners mkL ourVerInfo

            -- Bracket kademlia and network-transport, create a node. This
            -- will be very involved. Should make it top-level I think.
            runDiffusionLayer :: forall y . d y -> d y
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
            getBlocks = give blockLimits $ Diffusion.Block.getBlocks logic enqueue

            requestTip :: (BlockHeader -> NodeId -> d t) -> d (Map NodeId (d t))
            requestTip = give blockLimits $ Diffusion.Block.requestTip enqueue

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

            diffusion :: Diffusion d
            diffusion = Diffusion {..}

        return DiffusionLayer {..}

  where
    -- TBD will we need any resources here?
    acquire = pure ()
    release = \_ -> pure ()

-- FIXME TBD move the remainder into a separate module?

-- | Create kademlia, network-transport, and run the outbound queue's
-- dequeue thread.
runDiffusionLayerFull
    :: forall d x .
       ( DiffusionWorkMode d, MonadFix d )
    => NetworkConfig KademliaParams
    -> VerInfo
    -> OQ.OutboundQ (EnqueuedConversation d) NodeId Bucket
    -> (VerInfo -> [Listener d])
    -> d x
    -> d x
runDiffusionLayerFull networkConfig ourVerInfo oq listeners action =
    bracketTransport (ncTcpAddr networkConfig) $ \(transport :: Transport d) ->
        bracketKademlia networkConfig $ \networkConfig' ->
            timeWarpNode transport ourVerInfo listeners $ \_ converse ->
                withAsync (OQ.dequeueThread oq (sendMsgFromConverse converse)) $ \_ -> do
                    -- TODO EKG stuff.
                    -- Both logic and diffusion will use EKG so we don't
                    -- set it up here in the diffusion layer, but we do
                    -- register some counters and gauges.
                    --
                    -- Subscription worker bypasses the outbound queue and uses
                    -- send actions directly.
                    let sendActions :: SendActions d
                        sendActions = makeSendActions ourVerInfo oqEnqueue converse
                    withAsync (subscriptionThread networkConfig' sendActions) $ \_ ->
                        action
  where
    oqEnqueue :: Msg -> (NodeId -> VerInfo -> Conversation PackingType d t) -> d (Map NodeId (d t))
    oqEnqueue msgType k = do
        itList <- OQ.enqueue oq msgType (EnqueuedConversation (msgType, k))
        let itMap = M.fromList itList
        return ((>>= either throw return) <$> itMap)
    subscriptionThread nc sactions = case topologySubscriptionWorker (ncTopology nc) of
        Just (SubscriptionWorkerBehindNAT dnsDomains) -> do
            keepaliveTimer <- error "TIMER"
            -- TODO update to use logic layer to get next slot interval.
            let retryInterval = pure 20000
            dnsSubscriptionWorker oq networkConfig dnsDomains keepaliveTimer retryInterval sactions
        Just (SubscriptionWorkerKademlia kinst nodeType valency fallbacks) ->
            dhtSubscriptionWorker oq kinst nodeType valency fallbacks sactions
        Nothing -> pure ()

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
