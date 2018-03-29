{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Pos.Diffusion.Full
    ( FullDiffusionConfiguration (..)
    , diffusionLayerFull
    , diffusionLayerFullExposeInternals
    , FullDiffusionInternals (..)
    , RunFullDiffusionInternals (..)
    ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Monad.Fix (MonadFix)
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import           Data.Time.Units (Microsecond, Millisecond, Second, convertUnit)
import           Formatting (Format)
import           Mockable (withAsync, link)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (MsgType (..), Origin (..))
import           Network.Transport.Abstract (Transport)
import           Node (Node, NodeAction (..), simpleNodeEndPoint, NodeEnvironment (..), defaultNodeEnvironment, node)
import           Node.Conversation (Converse, converseWith, Conversation)
import           System.Random (newStdGen)
import           System.Wlog (WithLogger, CanLog, usingLoggerName)

import           Pos.Block.Network (MsgGetHeaders, MsgHeaders, MsgGetBlocks, MsgBlock)
import           Pos.Communication (NodeId, VerInfo (..), PeerData, PackingType,
                                    EnqueueMsg, makeEnqueueMsg, bipPacking, Listener,
                                    MkListeners (..), HandlerSpecs, InSpecs (..),
                                    OutSpecs (..), createOutSpecs, toOutSpecs, convH,
                                    InvOrDataTK, MsgSubscribe, MsgSubscribe1,
                                    makeSendActions, SendActions, Msg)
import           Pos.Communication.Relay.Logic (invReqDataFlowTK)
import           Pos.Communication.Util (wrapListener)
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
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
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

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
{-# ANN module ("HLint: ignore Use whenJust" :: Text) #-}
{-# ANN module ("HLint: ignore Use record patterns" :: Text) #-}

data FullDiffusionConfiguration = FullDiffusionConfiguration
    { fdcProtocolMagic          :: !ProtocolMagic
    , fdcProtocolConstants      :: !ProtocolConstants
    , fdcRecoveryHeadersMessage :: !Word
    , fdcLastKnownBlockVersion  :: !BlockVersion
    , fdcConvEstablishTimeout   :: !Microsecond
    }

data RunFullDiffusionInternals d = RunFullDiffusionInternals
    { runFullDiffusionInternals :: forall y . (FullDiffusionInternals d -> d y) -> d y
    }

data FullDiffusionInternals d = FullDiffusionInternals
    { fdiNode :: Node d
    , fdiConverse :: Converse PackingType PeerData d
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
    :: forall d m x .
       ( DiffusionWorkMode d
       , MonadFix d
       , MonadIO m
       , MonadMask m
       , WithLogger m
       )
    => (forall y . d y -> IO y)
    -> FullDiffusionConfiguration
    -> NetworkConfig KademliaParams
    -> Maybe (EkgNodeMetrics d)
    -> Logic d
    -> (DiffusionLayer d -> m x)
    -> m x
diffusionLayerFull runIO fdconf networkConfig mEkgNodeMetrics logic k = do
    -- Make the outbound queue using network policies.
    oq :: OQ.OutboundQ (EnqueuedConversation d) NodeId Bucket <-
        -- NB: <> it's not Text semigroup append, it's LoggerName append, which
        -- puts a "." in the middle.
        initQueue networkConfig ("diffusion" <> "outboundqueue") (enmStore <$> mEkgNodeMetrics)
    let topology = ncTopology networkConfig
        mSubscriptionWorker = topologySubscriptionWorker topology
        mSubscribers = topologySubscribers topology
        healthStatus = topologyHealthStatus topology oq
        mKademliaParams = topologyRunKademlia topology
    bracketTransportTCP (fdcConvEstablishTimeout fdconf) (ncTcpAddr networkConfig) $ \transport -> do
        (fullDiffusion, internals) <-
            diffusionLayerFullExposeInternals runIO
                                              fdconf
                                              transport
                                              oq
                                              (ncDefaultPort networkConfig)
                                              mSubscriptionWorker
                                              mSubscribers
                                              mKademliaParams
                                              healthStatus
                                              mEkgNodeMetrics
                                              logic
        k $ DiffusionLayer
            { diffusion = fullDiffusion
            , runDiffusionLayer = \action -> runFullDiffusionInternals internals (const action)
            }

diffusionLayerFullExposeInternals
    :: forall d m .
       ( DiffusionWorkMode d
       , MonadFix d
       , MonadIO m
       , MonadMask m
       )
    => (forall y . d y -> IO y)
    -> FullDiffusionConfiguration
    -> Transport d
    -> OQ.OutboundQ (EnqueuedConversation d) NodeId Bucket
    -> Word16 -- ^ Port on which peers are assumed to listen.
    -> Maybe SubscriptionWorker
    -> Maybe (NodeType, OQ.MaxBucketSize)
    -> Maybe (KademliaParams, Bool)
       -- ^ KademliaParams and a default port for kademlia.
       -- Bool says whether the node must join before starting normal
       -- operation, as opposed to passively trying to join.
    -> d HealthStatus
       -- ^ Amazon Route53 health check support (stopgap measure, see note
       --   in Pos.Diffusion.Types, above 'healthStatus' record field).
    -> Maybe (EkgNodeMetrics d)
    -> Logic d
    -> m (Diffusion d, RunFullDiffusionInternals d)
diffusionLayerFullExposeInternals runIO
                                  fdconf
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

    -- Subscription status.
    subscriptionStatus <- newTVarIO MS.empty

    -- Timer is in microseconds.
    keepaliveTimer :: Timer <- newTimer $ convertUnit (20 :: Second)

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

        -- Copied from existing implementation but
        -- FIXME it will be wrong when the patch to include a keepalive
        -- is merged. That shall be the first test of this inspec/outspec
        -- system I suppose.
        subscriptionWorkerOutSpecs = case mSubscriptionWorker of
            Just (SubscriptionWorkerBehindNAT _)     -> specs
            Just (SubscriptionWorkerKademlia  _ _ _) -> specs
            _                                        -> mempty
          where
            specs = toOutSpecs
                [ convH (Proxy @MsgSubscribe)  (Proxy @Void)
                , convH (Proxy @MsgSubscribe1) (Proxy @Void)
                ]

        -- It's a localOnNewSlotWorker, so mempty.
        dhtWorkerOutSpecs = mempty

        mkL :: MkListeners d
        mkL = mconcat $
            [ lmodifier "block"       $ Diffusion.Block.blockListeners logic protocolConstants recoveryHeadersMessage oq keepaliveTimer
            , lmodifier "tx"          $ Diffusion.Txp.txListeners logic oq enqueue
            , lmodifier "update"      $ Diffusion.Update.updateListeners logic oq enqueue
            , lmodifier "delegation"  $ Diffusion.Delegation.delegationListeners logic oq enqueue
            , lmodifier "ssc"         $ Diffusion.Ssc.sscListeners logic oq enqueue
            ] ++ [
              lmodifier "subscription" $ subscriptionListeners oq subscriberNodeType
            | Just (subscriberNodeType, _) <- [mSubscribers]
            ]

        lmodifier lname mkLs = mkLs { mkListeners = mkListeners' }
          where
            mkListeners' v p =
                let ls = mkListeners mkLs v p
                    f = wrapListener ("server" <> lname)
                in  map f ls

        listeners :: VerInfo -> [Listener d]
        listeners = mkListeners mkL ourVerInfo

        currentSlotDuration :: d Millisecond
        currentSlotDuration = bvdSlotDuration <$> getAdoptedBVData logic

        -- Bracket kademlia and network-transport, create a node. This
        -- will be very involved. Should make it top-level I think.
        runDiffusionLayer :: forall y . (FullDiffusionInternals d -> d y) -> d y
        runDiffusionLayer = runDiffusionLayerFull
            runIO
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

        enqueue :: EnqueueMsg d
        enqueue = makeEnqueueMsg ourVerInfo $ \msgType k -> do
            itList <- liftIO $ OQ.enqueue oq msgType (EnqueuedConversation (msgType, k))
            pure (M.fromList itList)

        getBlocks :: NodeId
                  -> HeaderHash
                  -> [HeaderHash]
                  -> d (OldestFirst [] Block)
        getBlocks = Diffusion.Block.getBlocks logic recoveryHeadersMessage enqueue

        requestTip :: d (Map NodeId (d BlockHeader))
        requestTip = Diffusion.Block.requestTip logic enqueue recoveryHeadersMessage

        announceBlockHeader :: MainBlockHeader -> d ()
        announceBlockHeader = void . Diffusion.Block.announceBlockHeader logic protocolConstants recoveryHeadersMessage enqueue

        sendTx :: TxAux -> d Bool
        sendTx = Diffusion.Txp.sendTx enqueue

        sendUpdateProposal :: UpId -> UpdateProposal -> [UpdateVote] -> d ()
        sendUpdateProposal = Diffusion.Update.sendUpdateProposal enqueue

        sendVote :: UpdateVote -> d ()
        sendVote = Diffusion.Update.sendVote enqueue

        -- TODO put these into a Pos.Diffusion.Full.Ssc module.
        sendSscCert :: VssCertificate -> d ()
        sendSscCert = void . invReqDataFlowTK "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCVssCertificate

        sendSscOpening :: Opening -> d ()
        sendSscOpening = void . invReqDataFlowTK "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCOpening (ourStakeholderId logic)

        sendSscShares :: InnerSharesMap -> d ()
        sendSscShares = void . invReqDataFlowTK "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCShares (ourStakeholderId logic)

        sendSscCommitment :: SignedCommitment -> d ()
        sendSscCommitment = void . invReqDataFlowTK "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCCommitment

        sendPskHeavy :: ProxySKHeavy -> d ()
        sendPskHeavy = Diffusion.Delegation.sendPskHeavy enqueue

        formatPeers :: forall r . (forall a . Format r a -> a) -> d (Maybe r)
        formatPeers formatter = liftIO (Just <$> OQ.dumpState oq formatter)

        diffusion :: Diffusion d
        diffusion = Diffusion {..}

        runInternals = RunFullDiffusionInternals
            { runFullDiffusionInternals = runDiffusionLayer
            }

    return (diffusion, runInternals)

-- | Create kademlia, network-transport, and run the outbound queue's
-- dequeue thread.
runDiffusionLayerFull
    :: forall d x .
       ( DiffusionWorkMode d, MonadFix d )
    => (forall y . d y -> IO y)
    -> Transport d
    -> OQ.OutboundQ (EnqueuedConversation d) NodeId Bucket
    -> Microsecond -- ^ Conversation establish timeout
    -> VerInfo
    -> Word16 -- ^ Default port to use for resolved hosts (from dns)
    -> Maybe (KademliaParams, Bool)
    -> Maybe SubscriptionWorker
    -> Maybe (EkgNodeMetrics d)
    -> Timer -- ^ Keepalive timer.
    -> d Millisecond -- ^ Slot duration; may change over time.
    -> TVar (MS.Map NodeId SubscriptionStatus) -- ^ Subscription status.
    -> (VerInfo -> [Listener d])
    -> (FullDiffusionInternals d -> d x)
    -> d x
runDiffusionLayerFull runIO
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
    maybeBracketKademliaInstance mKademliaParams defaultPort $ \mKademlia ->
        timeWarpNode transport convEstablishTimeout ourVerInfo listeners $ \nd converse ->
            withAsync (liftIO $ OQ.dequeueThread oq (sendMsgFromConverse runIO converse)) $ \dthread -> do
                link dthread
                case mEkgNodeMetrics of
                    Just ekgNodeMetrics -> registerEkgNodeMetrics ekgNodeMetrics nd
                    Nothing -> pure ()
                -- Subscription worker bypasses the outbound queue and uses
                -- send actions directly.
                let sendActions :: SendActions d
                    sendActions = makeSendActions ourVerInfo oqEnqueue converse
                withAsync (subscriptionThread (fst <$> mKademlia) sendActions) $ \sthread -> do
                    link sthread
                    maybe (pure ()) joinKademlia mKademlia
                    k $ FullDiffusionInternals
                        { fdiNode = nd
                        , fdiConverse = converse
                        }
  where
    oqEnqueue :: Msg -> (NodeId -> VerInfo -> Conversation PackingType d t) -> d (Map NodeId (STM.TVar (OQ.PacketStatus t)))
    oqEnqueue msgType l = do
        itList <- liftIO $ OQ.enqueue oq msgType (EnqueuedConversation (msgType, l))
        return (M.fromList itList)
    subscriptionThread mKademliaInst sactions = case mSubscriptionWorker of
        Just (SubscriptionWorkerBehindNAT dnsDomains) ->
            dnsSubscriptionWorker oq defaultPort dnsDomains keepaliveTimer slotDuration subscriptionStatus sactions
        Just (SubscriptionWorkerKademlia nodeType valency fallbacks) -> case mKademliaInst of
            -- Caller wanted a DHT subscription worker, but not a Kademlia
            -- instance. Shouldn't be allowed, but oh well FIXME later.
            Nothing -> pure ()
            Just kInst -> dhtSubscriptionWorker oq kInst nodeType valency fallbacks sactions
        Nothing -> pure ()

sendMsgFromConverse
    :: (forall x . d x -> IO x)
    -> Converse PackingType PeerData d
    -> OQ.SendMsg (EnqueuedConversation d) NodeId
sendMsgFromConverse runIO converse (EnqueuedConversation (_, k)) nodeId =
    runIO $ converseWith converse nodeId (k nodeId)

-- | Bring up a time-warp node. It will come down when the continuation ends.
timeWarpNode
    :: forall d t .
       ( DiffusionWorkMode d, MonadFix d )
    => Transport d
    -> Microsecond -- Timeout.
    -> VerInfo
    -> (VerInfo -> [Listener d])
    -> (Node d -> Converse PackingType PeerData d -> d t)
    -> d t
timeWarpNode transport convEstablishTimeout ourVerInfo listeners k = do
    stdGen <- liftIO newStdGen
    node mkTransport mkReceiveDelay mkConnectDelay stdGen bipPacking ourVerInfo nodeEnv $ \theNode ->
        NodeAction listeners $ k theNode
  where
    mkTransport = simpleNodeEndPoint transport
    mkReceiveDelay = const (pure Nothing)
    mkConnectDelay = const (pure Nothing)
    nodeEnv = defaultNodeEnvironment { nodeAckTimeout = convEstablishTimeout }

----------------------------------------------------------------------------
-- Kademlia
----------------------------------------------------------------------------

createKademliaInstance ::
       (MonadIO m, MonadCatch m, CanLog m)
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
    :: (MonadIO m, MonadMask m, CanLog m)
    => (KademliaParams, Bool)
    -> Word16
    -> ((KademliaDHTInstance, Bool) -> m a)
    -> m a
bracketKademliaInstance (kp, mustJoin) defaultPort action =
    bracket (createKademliaInstance kp defaultPort) stopDHTInstance $ \kinst ->
        action (kinst, mustJoin)

maybeBracketKademliaInstance
    :: (MonadIO m, MonadMask m, CanLog m)
    => Maybe (KademliaParams, Bool)
    -> Word16
    -> (Maybe (KademliaDHTInstance, Bool) -> m a)
    -> m a
maybeBracketKademliaInstance Nothing _ k = k Nothing
maybeBracketKademliaInstance (Just kp) defaultPort k =
    bracketKademliaInstance kp defaultPort (k . Just)

-- | Join the Kademlia network.
joinKademlia
    :: ( DiffusionWorkMode m )
    => (KademliaDHTInstance, Bool)
    -> m ()
joinKademlia (kInst, mustJoin) = case mustJoin of
    True  -> kademliaJoinNetworkRetry kInst (kdiInitialPeers kInst) retryInterval
    False -> kademliaJoinNetworkNoThrow kInst (kdiInitialPeers kInst)
  where
    retryInterval :: Second
    retryInterval = 5
