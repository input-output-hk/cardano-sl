{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}

module Pos.Diffusion.Full
    ( FullDiffusionConfiguration (..)
    , diffusionLayerFull
    , diffusionLayerFullExposeInternals
    , FullDiffusionInternals (..)
    , RunFullDiffusionInternals (..)
    ) where

import           Universum
import           Nub (ordNub)

import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Functor.Contravariant (contramap)
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import           Data.Time.Units (Microsecond, Millisecond, Second)
import           Formatting (Format)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (MsgType (..), Origin (..))
import           Network.Transport (Transport)
import           Node (Node, NodeAction (..), simpleNodeEndPoint, NodeEnvironment (..), defaultNodeEnvironment, node)
import           Node.Conversation (Converse, converseWith, Conversation)
import           System.Random (newStdGen)

import           Pos.Block.Network (MsgGetHeaders, MsgHeaders, MsgGetBlocks, MsgBlock)
import           Pos.Communication (NodeId, VerInfo (..), PeerData, PackingType,
                                    EnqueueMsg, makeEnqueueMsg, bipPacking, Listener,
                                    MkListeners (..), HandlerSpecs, InSpecs (..),
                                    OutSpecs (..), createOutSpecs, toOutSpecs, convH,
                                    InvOrDataTK, MsgSubscribe, MsgSubscribe1,
                                    makeSendActions, SendActions, Msg)
import           Pos.Communication.Relay.Logic (invReqDataFlowTK)
import           Pos.Core (BlockVersionData (..), BlockVersion, HeaderHash, ProxySKHeavy,
                           StakeholderId, ProtocolConstants (..))
import           Pos.Core.Block (Block, BlockHeader, MainBlockHeader)
import           Pos.Core.Ssc (Opening, InnerSharesMap, SignedCommitment, VssCertificate)
import           Pos.Core.Txp (TxAux)
import           Pos.Core.Update (UpId, UpdateProposal, UpdateVote)
import           Pos.Crypto.Configuration (ProtocolMagic (..))
import           Pos.DHT.Real (KademliaDHTInstance (..), KademliaParams (..),
                               startDHTInstance, stopDHTInstance,
                               kademliaJoinNetworkNoThrow, kademliaJoinNetworkRetry)
import qualified Pos.Diffusion.Full.Block as Diffusion.Block
import qualified Pos.Diffusion.Full.Delegation as Diffusion.Delegation
import qualified Pos.Diffusion.Full.Ssc as Diffusion.Ssc
import qualified Pos.Diffusion.Full.Txp as Diffusion.Txp
import qualified Pos.Diffusion.Full.Update as Diffusion.Update
import           Pos.Diffusion.Subscription.Common (subscriptionListeners)
import           Pos.Diffusion.Subscription.Dht (dhtSubscriptionWorker)
import           Pos.Diffusion.Subscription.Dns (dnsSubscriptionWorker)
import           Pos.Diffusion.Transport.TCP (bracketTransportTCP)
import           Pos.Diffusion.Types (Diffusion (..), DiffusionLayer (..), SubscriptionStatus)
import           Pos.Logic.Types (Logic (..))
import           Pos.Network.Types (NetworkConfig (..), Bucket (..), initQueue,
                                    topologySubscribers, SubscriptionWorker (..),
                                    NodeType,  topologySubscriptionWorker,
                                    topologyRunKademlia, topologyHealthStatus)
import           Pos.Reporting.Health.Types (HealthStatus (..))
import           Pos.Reporting.Ekg (EkgNodeMetrics (..), registerEkgNodeMetrics)
import           Pos.Ssc.Message (MCOpening (..), MCShares (..), MCCommitment (..), MCVssCertificate (..))
import           Pos.Util.Chrono (OldestFirst)
import           Pos.Util.OutboundQueue (EnqueuedConversation (..))
import           Pos.Util.Timer (Timer, newTimer)
import           Pos.Util.Trace (Trace, Severity (Error))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
{-# ANN module ("HLint: ignore Use whenJust" :: Text) #-}
{-# ANN module ("HLint: ignore Use record patterns" :: Text) #-}

data FullDiffusionConfiguration = FullDiffusionConfiguration
    { fdcProtocolMagic          :: !ProtocolMagic
    , fdcProtocolConstants      :: !ProtocolConstants
    , fdcRecoveryHeadersMessage :: !Word
    , fdcLastKnownBlockVersion  :: !BlockVersion
    , fdcConvEstablishTimeout   :: !Microsecond
    , fdcTrace                  :: !(Trace IO (Severity, Text))
    }

data RunFullDiffusionInternals = RunFullDiffusionInternals
    { runFullDiffusionInternals :: forall y . (FullDiffusionInternals -> IO y) -> IO y
    }

data FullDiffusionInternals = FullDiffusionInternals
    { fdiNode        :: Node
    , fdiConverse    :: Converse PackingType PeerData
    , fdiSendActions :: SendActions
    }

-- | Make a full diffusion layer, filling in many details using a
-- 'NetworkConfig' and its constituent 'Topology'.
-- An 'OutboundQ' is brought up for you, based on the 'NetworkConfig'.
-- A TCP transport is brought up as well, again using the 'NetworkConfig',
-- which includes information about the address. This is why we use CPS here:
-- the transport is bracketed.
-- The 'NetworkConfig's topology is also used to fill in various options
-- related to subscription, health status reporting, etc.
diffusionLayerFull
    :: FullDiffusionConfiguration
    -> NetworkConfig KademliaParams
    -> Maybe EkgNodeMetrics
    -> (Diffusion IO -> Logic IO)
       -- ^ The logic layer can use the diffusion layer.
    -> (DiffusionLayer IO -> IO x)
    -> IO x
diffusionLayerFull fdconf networkConfig mEkgNodeMetrics mkLogic k = do
    -- Make the outbound queue using network policies.
    oq :: OQ.OutboundQ EnqueuedConversation NodeId Bucket <-
        -- NB: <> it's not Text semigroup append, it's LoggerName append, which
        -- puts a "." in the middle.
        initQueue networkConfig ("diffusion" <> "outboundqueue") (enmStore <$> mEkgNodeMetrics)
    let topology = ncTopology networkConfig
        mSubscriptionWorker = topologySubscriptionWorker topology
        mSubscribers = topologySubscribers topology
        healthStatus = topologyHealthStatus topology oq
        mKademliaParams = topologyRunKademlia topology
        -- Transport needs a Trace IO Text. We re-use the 'Trace' given in
        -- the configuration at severity 'Error' (when transport has an
        -- exception trying to 'accept' a new connection).
        logTrace :: Trace IO Text
        logTrace = contramap ((,) Error) (fdcTrace fdconf)
    bracketTransportTCP logTrace (fdcConvEstablishTimeout fdconf) (ncTcpAddr networkConfig) $ \transport -> do
        rec (fullDiffusion, internals) <-
                diffusionLayerFullExposeInternals fdconf
                                                  transport
                                                  oq
                                                  (ncDefaultPort networkConfig)
                                                  mSubscriptionWorker
                                                  mSubscribers
                                                  mKademliaParams
                                                  healthStatus
                                                  mEkgNodeMetrics
                                                  logic
            let logic = mkLogic fullDiffusion
        k $ DiffusionLayer
            { diffusion = fullDiffusion
            , runDiffusionLayer = \action -> runFullDiffusionInternals internals (const action)
            }

diffusionLayerFullExposeInternals
    :: FullDiffusionConfiguration
    -> Transport
    -> OQ.OutboundQ EnqueuedConversation NodeId Bucket
    -> Word16 -- ^ Port on which peers are assumed to listen.
    -> Maybe SubscriptionWorker
    -> Maybe (NodeType, OQ.MaxBucketSize)
    -> Maybe (KademliaParams, Bool)
       -- ^ KademliaParams and a default port for kademlia.
       -- Bool says whether the node must join before starting normal
       -- operation, as opposed to passively trying to join.
    -> IO HealthStatus
       -- ^ Amazon Route53 health check support (stopgap measure, see note
       --   in Pos.Diffusion.Types, above 'healthStatus' record field).
    -> Maybe EkgNodeMetrics
    -> Logic IO
    -> IO (Diffusion IO, RunFullDiffusionInternals)
diffusionLayerFullExposeInternals fdconf
                                  transport
                                  oq
                                  defaultPort
                                  mSubscriptionWorker
                                  mSubscribers
                                  mKademliaParams
                                  healthStatus -- named to be picked up by record wildcard
                                  mEkgNodeMetrics
                                  logic = do

    let protocolMagic = fdcProtocolMagic fdconf
        protocolConstants = fdcProtocolConstants fdconf
        lastKnownBlockVersion = fdcLastKnownBlockVersion fdconf
        recoveryHeadersMessage = fdcRecoveryHeadersMessage fdconf
        logTrace = fdcTrace fdconf

    -- Subscription status.
    subscriptionStatus <- newTVarIO MS.empty

    keepaliveTimer <- newTimer

    let -- VerInfo is a diffusion-layer-specific thing. It's only used for
        -- negotiating with peers.
        --
        -- Known bug: if the block version changes, the VerInfo will be
        -- out of date, as it's immutable.
        -- Solution: don't put it in the VerInfo. Other clients don't need
        -- to know the peer's latest adopted block version, they need only
        -- know what software version its running.
        ourVerInfo :: VerInfo
        ourVerInfo = VerInfo (getProtocolMagic protocolMagic)
                             lastKnownBlockVersion
                             ins
                             (outs <> workerOuts)

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
            [ -- First: the relay system out specs.
              Diffusion.Txp.txOutSpecs logic
            , Diffusion.Update.updateOutSpecs logic
            , Diffusion.Delegation.delegationOutSpecs logic
            , Diffusion.Ssc.sscOutSpecs logic
              -- Relay system for blocks is ad-hoc.
            , blockWorkerOutSpecs
              -- SSC has non-relay out specs, defined below.
            , sscWorkerOutSpecs
            , securityWorkerOutSpecs
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

        -- announceBlockHeaderOuts from blkCreatorWorker
        -- announceBlockHeaderOuts from blkMetricCheckerWorker
        -- along with the retrieval worker outs which also include
        -- announceBlockHeaderOuts.
        blockWorkerOutSpecs = mconcat
            [ announceBlockHeaderOuts
            , announceBlockHeaderOuts
            , announceBlockHeaderOuts <> toOutSpecs [ convH (Proxy :: Proxy MsgGetBlocks)
                                                            (Proxy :: Proxy MsgBlock)
                                                    ]
            ]

        announceBlockHeaderOuts = toOutSpecs [ convH (Proxy :: Proxy MsgHeaders)
                                                     (Proxy :: Proxy MsgGetHeaders)
                                             ]

        -- Plainly mempty from the definition of allWorkers.
        slottingWorkerOutSpecs = mempty

        subscriptionWorkerOutSpecs = toOutSpecs
            [ convH (Proxy @MsgSubscribe)  (Proxy @Void)
            , convH (Proxy @MsgSubscribe1) (Proxy @Void)
            ]

        -- It's a localOnNewSlotWorker, so mempty.
        dhtWorkerOutSpecs = mempty

        mkL :: MkListeners
        mkL = mconcat $
            [ Diffusion.Block.blockListeners logTrace logic protocolConstants recoveryHeadersMessage oq keepaliveTimer
            , Diffusion.Txp.txListeners logTrace logic oq enqueue
            , Diffusion.Update.updateListeners logTrace logic oq enqueue
            , Diffusion.Delegation.delegationListeners logTrace logic oq enqueue
            , Diffusion.Ssc.sscListeners logTrace logic oq enqueue
            ] ++ [
              subscriptionListeners logTrace oq subscriberNodeType
            | Just (subscriberNodeType, _) <- [mSubscribers]
            ]

        listeners :: VerInfo -> [Listener]
        listeners = mkListeners mkL ourVerInfo

        currentSlotDuration :: IO Millisecond
        currentSlotDuration = bvdSlotDuration <$> getAdoptedBVData logic

        -- Bracket kademlia and network-transport, create a node. This
        -- will be very involved. Should make it top-level I think.
        runDiffusionLayer :: forall y . (FullDiffusionInternals -> IO y) -> IO y
        runDiffusionLayer = runDiffusionLayerFull
            logTrace
            transport
            oq
            (fdcConvEstablishTimeout fdconf)
            ourVerInfo
            defaultPort
            mKademliaParams
            mSubscriptionWorker
            mEkgNodeMetrics
            keepaliveTimer
            currentSlotDuration
            subscriptionStatus
            listeners

        enqueue :: EnqueueMsg
        enqueue = makeEnqueueMsg logTrace ourVerInfo $ \msgType k -> do
            itList <- OQ.enqueue oq msgType (EnqueuedConversation (msgType, k))
            pure (M.fromList itList)

        getBlocks :: NodeId
                  -> HeaderHash
                  -> [HeaderHash]
                  -> IO (OldestFirst [] Block)
        getBlocks = Diffusion.Block.getBlocks logTrace logic recoveryHeadersMessage enqueue

        requestTip :: IO (Map NodeId (IO BlockHeader))
        requestTip = Diffusion.Block.requestTip logTrace logic enqueue recoveryHeadersMessage

        announceBlockHeader :: MainBlockHeader -> IO ()
        announceBlockHeader = void . Diffusion.Block.announceBlockHeader logTrace logic protocolConstants recoveryHeadersMessage enqueue

        sendTx :: TxAux -> IO Bool
        sendTx = Diffusion.Txp.sendTx logTrace enqueue

        sendUpdateProposal :: UpId -> UpdateProposal -> [UpdateVote] -> IO ()
        sendUpdateProposal = Diffusion.Update.sendUpdateProposal logTrace enqueue

        sendVote :: UpdateVote -> IO ()
        sendVote = Diffusion.Update.sendVote logTrace enqueue

        -- TODO put these into a Pos.Diffusion.Full.Ssc module.
        sendSscCert :: VssCertificate -> IO ()
        sendSscCert = void . invReqDataFlowTK logTrace "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCVssCertificate

        sendSscOpening :: Opening -> IO ()
        sendSscOpening = void . invReqDataFlowTK logTrace "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCOpening (ourStakeholderId logic)

        sendSscShares :: InnerSharesMap -> IO ()
        sendSscShares = void . invReqDataFlowTK logTrace "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCShares (ourStakeholderId logic)

        sendSscCommitment :: SignedCommitment -> IO ()
        sendSscCommitment = void . invReqDataFlowTK logTrace "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCCommitment

        sendPskHeavy :: ProxySKHeavy -> IO ()
        sendPskHeavy = Diffusion.Delegation.sendPskHeavy logTrace enqueue

        -- TODO better status text.
        formatStatus :: forall r . (forall a . Format r a -> a) -> IO r
        formatStatus formatter = OQ.dumpState oq formatter

        diffusion :: Diffusion IO
        diffusion = Diffusion {..}

        runInternals = RunFullDiffusionInternals
            { runFullDiffusionInternals = runDiffusionLayer
            }

    return (diffusion, runInternals)

-- | Create kademlia, network-transport, and run the outbound queue's
-- dequeue thread.
runDiffusionLayerFull
    :: Trace IO (Severity, Text)
    -> Transport
    -> OQ.OutboundQ EnqueuedConversation NodeId Bucket
    -> Microsecond -- ^ Conversation establish timeout
    -> VerInfo
    -> Word16 -- ^ Default port to use for resolved hosts (from dns)
    -> Maybe (KademliaParams, Bool)
    -> Maybe SubscriptionWorker
    -> Maybe EkgNodeMetrics
    -> Timer -- ^ Keepalive timer.
    -> IO Millisecond -- ^ Slot duration; may change over time.
    -> TVar (MS.Map NodeId SubscriptionStatus) -- ^ Subscription status.
    -> (VerInfo -> [Listener])
    -> (FullDiffusionInternals -> IO x)
    -> IO x
runDiffusionLayerFull logTrace
                      transport
                      oq
                      convEstablishTimeout
                      ourVerInfo
                      defaultPort
                      mKademliaParams
                      mSubscriptionWorker
                      mEkgNodeMetrics
                      keepaliveTimer
                      slotDuration
                      subscriptionStatus
                      listeners
                      k =
    maybeBracketKademliaInstance logTrace mKademliaParams defaultPort $ \mKademlia ->
        timeWarpNode logTrace transport convEstablishTimeout ourVerInfo listeners $ \nd converse ->
            -- Concurrently run the dequeue thread, subscription thread, and
            -- main action.
            let sendActions :: SendActions
                sendActions = makeSendActions logTrace ourVerInfo oqEnqueue converse
                dequeueDaemon = OQ.dequeueThread oq (sendMsgFromConverse converse)
                subscriptionDaemon = subscriptionThread (fst <$> mKademlia) sendActions
                mainAction = do
                    maybe (pure ()) (flip registerEkgNodeMetrics nd) mEkgNodeMetrics
                    maybe (pure ()) (joinKademlia logTrace) mKademlia
                    let fdi = FullDiffusionInternals
                            { fdiNode = nd
                            , fdiConverse = converse
                            , fdiSendActions = sendActions
                            }
                    t <- k fdi
                    -- If everything went well, stop the outbound queue
                    -- normally. If 'k fdi' threw an exception, the dequeue
                    -- thread ('dequeueDaemon') will be killed.
                    OQ.waitShutdown oq
                    pure t
                
                action = Concurrently dequeueDaemon
                      *> Concurrently subscriptionDaemon
                      *> Concurrently mainAction

            in  runConcurrently action
  where
    oqEnqueue :: Msg
              -> (NodeId -> VerInfo -> Conversation PackingType t)
              -> IO (Map NodeId (STM.TVar (OQ.PacketStatus t)))
    oqEnqueue msgType l = do
        itList <- OQ.enqueue oq msgType (EnqueuedConversation (msgType, l))
        return (M.fromList itList)
    subscriptionThread mKademliaInst sactions = case mSubscriptionWorker of
        Just (SubscriptionWorkerBehindNAT dnsDomains) ->
            dnsSubscriptionWorker logTrace oq defaultPort dnsDomains keepaliveTimer slotDuration subscriptionStatus sactions
        Just (SubscriptionWorkerKademlia nodeType valency fallbacks) -> case mKademliaInst of
            -- Caller wanted a DHT subscription worker, but not a Kademlia
            -- instance. Shouldn't be allowed, but oh well FIXME later.
            Nothing -> pure ()
            Just kInst -> dhtSubscriptionWorker logTrace oq kInst nodeType valency fallbacks sactions
        Nothing -> pure ()

sendMsgFromConverse
    :: Converse PackingType PeerData
    -> OQ.SendMsg EnqueuedConversation NodeId
sendMsgFromConverse converse (EnqueuedConversation (_, k)) nodeId =
    converseWith converse nodeId (k nodeId)

-- | Bring up a time-warp node. It will come down when the continuation ends.
timeWarpNode
    :: Trace IO (Severity, Text)
    -> Transport
    -> Microsecond -- Timeout.
    -> VerInfo
    -> (VerInfo -> [Listener])
    -> (Node -> Converse PackingType PeerData -> IO t)
    -> IO t
timeWarpNode logTrace transport convEstablishTimeout ourVerInfo listeners k = do
    stdGen <- newStdGen
    node logTrace mkTransport mkReceiveDelay mkConnectDelay stdGen bipPacking ourVerInfo nodeEnv $ \theNode ->
        NodeAction listeners $ k theNode
  where
    mkTransport = simpleNodeEndPoint transport
    mkReceiveDelay = const (pure Nothing)
    mkConnectDelay = const (pure Nothing)
    nodeEnv = defaultNodeEnvironment { nodeAckTimeout = convEstablishTimeout }

----------------------------------------------------------------------------
-- Kademlia
----------------------------------------------------------------------------

createKademliaInstance
    :: Trace IO (Severity, Text)
    -> KademliaParams
    -> Word16 -- ^ Default port to bind to.
    -> IO KademliaDHTInstance
createKademliaInstance logTrace kp defaultPort =
    startDHTInstance logTrace instConfig defaultBindAddress
  where
    instConfig = kp {kpPeers = ordNub $ kpPeers kp}
    defaultBindAddress = ("0.0.0.0", defaultPort)

-- | RAII for 'KademliaDHTInstance'.
bracketKademliaInstance
    :: Trace IO (Severity, Text)
    -> (KademliaParams, Bool)
    -> Word16
    -> ((KademliaDHTInstance, Bool) -> IO a)
    -> IO a
bracketKademliaInstance logTrace (kp, mustJoin) defaultPort action =
    bracket (createKademliaInstance logTrace kp defaultPort) stopDHTInstance $ \kinst ->
        action (kinst, mustJoin)

maybeBracketKademliaInstance
    :: Trace IO (Severity, Text)
    -> Maybe (KademliaParams, Bool)
    -> Word16
    -> (Maybe (KademliaDHTInstance, Bool) -> IO a)
    -> IO a
maybeBracketKademliaInstance _ Nothing _ k = k Nothing
maybeBracketKademliaInstance logTrace (Just kp) defaultPort k =
    bracketKademliaInstance logTrace kp defaultPort (k . Just)

-- | Join the Kademlia network.
joinKademlia :: Trace IO (Severity, Text) -> (KademliaDHTInstance, Bool) -> IO ()
joinKademlia logTrace (kInst, mustJoin) = case mustJoin of
    True  -> kademliaJoinNetworkRetry logTrace kInst (kdiInitialPeers kInst) retryInterval
    False -> kademliaJoinNetworkNoThrow logTrace kInst (kdiInitialPeers kInst)
  where
    retryInterval :: Second
    retryInterval = 5
