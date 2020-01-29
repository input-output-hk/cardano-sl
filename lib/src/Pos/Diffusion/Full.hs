{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Diffusion.Full
    ( FullDiffusionConfiguration (..)
    , diffusionLayerFull
    , diffusionLayerFullExposeInternals
    , FullDiffusionInternals (..)
    , RunFullDiffusionInternals (..)
    ) where

import           Universum

import           Control.Concurrent.Async (Concurrently (..), race)
import           Control.Concurrent.MVar (modifyMVar_)
import qualified Control.Concurrent.STM as STM
import           Data.Functor.Contravariant (contramap)
import qualified Data.Map as M
import           Data.Time.Units (Microsecond, Millisecond)
import           Formatting (Format)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (MsgType (..),
                     Origin (..))
import           Network.Transport (Transport)
import           Node (Node, NodeAction (..), NodeEnvironment (..),
                     defaultNodeEnvironment, node, simpleNodeEndPoint)
import           Node.Conversation (Conversation, Converse, converseWith)
import qualified System.Metrics as Monitoring

import           System.Random (newStdGen)

import           Pos.Chain.Block (Block, BlockHeader, HeaderHash,
                     MainBlockHeader)
import           Pos.Chain.Delegation (ProxySKHeavy)
import           Pos.Chain.Ssc (InnerSharesMap, MCCommitment (..),
                     MCOpening (..), MCShares (..), MCVssCertificate (..),
                     Opening, SignedCommitment, VssCertificate)
import           Pos.Chain.Txp (TxAux)
import           Pos.Chain.Update (BlockVersion, BlockVersionData (..), UpId,
                     UpdateProposal, UpdateVote)
import           Pos.Communication (EnqueueMsg, HandlerSpecs, InSpecs (..),
                     InvOrDataTK, Listener, MkListeners (..), Msg,
                     MsgSubscribe, MsgSubscribe1, NodeId, OutSpecs (..),
                     PackingType, PeerData, SendActions, VerInfo (..),
                     bipPacking, convH, createOutSpecs, makeEnqueueMsg,
                     makeSendActions, toOutSpecs)
import           Pos.Core (ProtocolConstants (..), StakeholderId)
import           Pos.Core.Chrono (OldestFirst)
import           Pos.Core.Metrics.Constants (withCardanoNamespace)
import           Pos.Crypto.Configuration (ProtocolMagic (..), getProtocolMagic)
import qualified Pos.Diffusion.Full.Block as Diffusion.Block
import qualified Pos.Diffusion.Full.Delegation as Diffusion.Delegation
import qualified Pos.Diffusion.Full.Ssc as Diffusion.Ssc
import qualified Pos.Diffusion.Full.Txp as Diffusion.Txp
import qualified Pos.Diffusion.Full.Update as Diffusion.Update
import           Pos.Infra.Communication.Relay.Logic (invReqDataFlowTK)
import           Pos.Infra.Diffusion.Subscription.Common (subscriptionListeners)
import           Pos.Infra.Diffusion.Subscription.Dns (dnsSubscriptionWorker)
import           Pos.Infra.Diffusion.Subscription.Status (SubscriptionStates,
                     emptySubscriptionStates)
import           Pos.Infra.Diffusion.Transport.TCP (bracketTransportTCP)
import           Pos.Infra.Diffusion.Types (Diffusion (..),
                     DiffusionHealth (..), DiffusionLayer (..),
                     StreamBlocks (..))
import           Pos.Infra.Network.Types (Bucket (..), NetworkConfig (..),
                     NodeType, SubscriptionWorker (..), initQueue,
                     topologyHealthStatus, topologySubscribers,
                     topologySubscriptionWorker)
import           Pos.Infra.Reporting.Ekg (EkgNodeMetrics (..),
                     registerEkgNodeMetrics)
import           Pos.Infra.Reporting.Health.Types (HealthStatus (..))
import           Pos.Logic.Types (Logic (..))
import           Pos.Network.Block.Types (MsgBlock, MsgGetBlocks, MsgGetHeaders,
                     MsgHeaders, MsgStream, MsgStreamBlock)
import           Pos.Util.OutboundQueue (EnqueuedConversation (..))
import           Pos.Util.Timer (Timer, startTimer)
import           Pos.Util.Trace (Severity (Error), Trace)
import           Pos.Util.Trace.Named (LogNamed, appendName, named)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
{-# ANN module ("HLint: ignore Use whenJust" :: Text) #-}
{-# ANN module ("HLint: ignore Use record patterns" :: Text) #-}

data FullDiffusionConfiguration = FullDiffusionConfiguration
    { fdcProtocolMagic          :: !ProtocolMagic
    , fdcProtocolConstants      :: !ProtocolConstants
    , fdcRecoveryHeadersMessage :: !Word
    , fdcLastKnownBlockVersion  :: !BlockVersion
    , fdcConvEstablishTimeout   :: !Microsecond
    , fdcBatchSize              :: !Word32
      -- ^ Size of batches of blocks to process when streaming.
    , fdcStreamWindow           :: !Word32
      -- ^ Size of window for block streaming.
    , fdcTrace                  :: !(Trace IO (LogNamed (Severity, Text)))
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
    -> NetworkConfig kademlia
    -> Maybe EkgNodeMetrics
    -> (Diffusion IO -> Logic IO)
       -- ^ The logic layer can use the diffusion layer.
    -> (DiffusionLayer IO -> IO x)
    -> IO x
diffusionLayerFull fdconf networkConfig mEkgNodeMetrics mkLogic k = do
    let -- A trace for the Outbound Queue. We use the one from the
        -- configuration, and put an outboundqueue suffix on it.
        oqTrace =appendName "outboundqueue" (fdcTrace fdconf)
    -- Make the outbound queue using network policies.
    oq :: OQ.OutboundQ EnqueuedConversation NodeId Bucket <-
        -- NB: <> it's not Text semigroup append, it's LoggerName append, which
        -- puts a "." in the middle.
        initQueue networkConfig oqTrace (enmStore <$> mEkgNodeMetrics)
    let topology = ncTopology networkConfig
        mSubscriptionWorker = topologySubscriptionWorker topology
        mSubscribers = topologySubscribers topology
        healthStatus = topologyHealthStatus topology oq
        -- Transport needs a Trace IO Text. We re-use the 'Trace' given in
        -- the configuration at severity 'Error' (when transport has an
        -- exception trying to 'accept' a new connection).
        logTrace :: Trace IO Text
        logTrace = contramap ((,) Error) $ named $
            appendName "transport" (fdcTrace fdconf)
    bracketTransportTCP logTrace (fdcConvEstablishTimeout fdconf) (ncTcpAddr networkConfig) (ncCheckPeerHost networkConfig) $ \transport -> do
        rec (fullDiffusion, internals) <-
                diffusionLayerFullExposeInternals fdconf
                                                  transport
                                                  oq
                                                  (ncDefaultPort networkConfig)
                                                  mSubscriptionWorker
                                                  mSubscribers
                                                  healthStatus
                                                  mEkgNodeMetrics
                                                  logic
            let logic = mkLogic fullDiffusion
        k $ DiffusionLayer
            { diffusion = fullDiffusion
            , runDiffusionLayer = \action -> runFullDiffusionInternals internals (const action)
            }

resetKeepAlive ::  IO Millisecond -> MVar (Map NodeId Timer) -> NodeId ->  IO ()
resetKeepAlive slotDuration timersVar nodeId =
    modifyMVar_ timersVar $ \timers ->
        let timer_m = M.lookup nodeId timers in
        case timer_m of
             Just timer -> do
                 currentDuration <- slotDuration
                 startTimer (3 * currentDuration) timer
                 pure timers
             Nothing -> pure timers

diffusionLayerFullExposeInternals
    :: FullDiffusionConfiguration
    -> Transport
    -> OQ.OutboundQ EnqueuedConversation NodeId Bucket
    -> Word16 -- ^ Port on which peers are assumed to listen.
    -> Maybe SubscriptionWorker
    -> Maybe (NodeType, OQ.MaxBucketSize)
    -> IO HealthStatus
       -- ^ Amazon Route53 health check support (stopgap measure, see note
       --   in Pos.Infra.Diffusion.Types, above 'healthStatus' record field).
    -> Maybe EkgNodeMetrics
    -> Logic IO
    -> IO (Diffusion IO, RunFullDiffusionInternals)
diffusionLayerFullExposeInternals fdconf
                                  transport
                                  oq
                                  defaultPort
                                  mSubscriptionWorker
                                  mSubscribers
                                  healthStatus -- named to be picked up by record wildcard
                                  mEkgNodeMetrics
                                  logic = do

    let protocolMagic = fdcProtocolMagic fdconf
        protocolConstants = fdcProtocolConstants fdconf
        lastKnownBlockVersion = fdcLastKnownBlockVersion fdconf
        recoveryHeadersMessage = fdcRecoveryHeadersMessage fdconf
        batchSize    = fdcBatchSize    fdconf
        streamWindow = fdcStreamWindow fdconf
        logTrace = named (fdcTrace fdconf)

    -- Subscription states.
    subscriptionStates <- emptySubscriptionStates

    keepaliveTimerVar <- newMVar M.empty

    diffusionHealth <- case mEkgNodeMetrics of
                            Nothing -> return Nothing
                            Just m  -> liftIO $ do
                                wqgM <- Monitoring.createGauge (withCardanoNamespace "diffusion.WriteQueue") $ enmStore m
                                wM   <- Monitoring.createGauge (withCardanoNamespace "diffusion.Window")     $ enmStore m
                                return $ Just $ DiffusionHealth wqgM wM

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
        -- requestTipOuts from Pos.Network.Block.Types
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
            , streamBlockHeaderOuts
            ]

        announceBlockHeaderOuts = toOutSpecs [ convH (Proxy :: Proxy MsgHeaders)
                                                     (Proxy :: Proxy MsgGetHeaders)
                                             ]

        streamBlockHeaderOuts = toOutSpecs [ convH (Proxy :: Proxy MsgStream)
                                                   (Proxy :: Proxy MsgStreamBlock)
                                           ]

        -- Plainly mempty from the definition of allWorkers.
        slottingWorkerOutSpecs = mempty

        subscriptionWorkerOutSpecs = toOutSpecs
            [ convH (Proxy @MsgSubscribe)  (Proxy @Void)
            , convH (Proxy @MsgSubscribe1) (Proxy @Void)
            ]

        mkL :: MkListeners
        mkL = mconcat $
            [ Diffusion.Block.blockListeners logTrace logic protocolConstants recoveryHeadersMessage oq
                  (Diffusion.Block.ResetNodeTimer $ resetKeepAlive currentSlotDuration keepaliveTimerVar)
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

        -- Bracket network-transport, create a node. This will be very involved.
        -- Should make it top-level I think.
        runDiffusionLayer :: forall y . (FullDiffusionInternals -> IO y) -> IO y
        runDiffusionLayer = runDiffusionLayerFull
            logTrace
            transport
            oq
            (fdcConvEstablishTimeout fdconf)
            ourVerInfo
            defaultPort
            mSubscriptionWorker
            mEkgNodeMetrics
            keepaliveTimerVar
            currentSlotDuration
            subscriptionStates
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

        streamBlocks :: forall t .
                        NodeId
                     -> HeaderHash
                     -> [HeaderHash]
                     -> StreamBlocks Block IO t
                     -> IO (Maybe t)
        streamBlocks = Diffusion.Block.streamBlocks logTrace diffusionHealth logic batchSize streamWindow enqueue

        announceBlockHeader :: MainBlockHeader -> IO ()
        announceBlockHeader = void . Diffusion.Block.announceBlockHeader logTrace logic protocolConstants recoveryHeadersMessage enqueue

        sendTx :: TxAux -> IO Bool
        sendTx = Diffusion.Txp.sendTx logTrace enqueue

        sendUpdateProposal :: UpId -> UpdateProposal -> [UpdateVote] -> IO ()
        sendUpdateProposal = Diffusion.Update.sendUpdateProposal logTrace enqueue

        sendVote :: UpdateVote -> IO ()
        sendVote = Diffusion.Update.sendVote logTrace enqueue

        -- TODO put these into a Pos.Diffusion.Full.Ssc module.
        sendSscCert :: StakeholderId -> VssCertificate -> IO ()
        sendSscCert sid = void . invReqDataFlowTK logTrace "ssc" enqueue (MsgMPC OriginSender) sid . MCVssCertificate

        sendSscOpening :: StakeholderId -> Opening -> IO ()
        sendSscOpening sid = void . invReqDataFlowTK logTrace "ssc" enqueue (MsgMPC OriginSender) sid . MCOpening sid

        sendSscShares :: StakeholderId -> InnerSharesMap -> IO ()
        sendSscShares sid = void . invReqDataFlowTK logTrace "ssc" enqueue (MsgMPC OriginSender) sid . MCShares sid

        sendSscCommitment :: StakeholderId -> SignedCommitment -> IO ()
        sendSscCommitment sid = void . invReqDataFlowTK logTrace "ssc" enqueue (MsgMPC OriginSender) sid . MCCommitment

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

-- | Create network-transport, and run the outbound queue's dequeue thread.
runDiffusionLayerFull
    :: Trace IO (Severity, Text)
    -> Transport
    -> OQ.OutboundQ EnqueuedConversation NodeId Bucket
    -> Microsecond -- ^ Conversation establish timeout
    -> VerInfo
    -> Word16 -- ^ Default port to use for resolved hosts (from dns)
    -> Maybe SubscriptionWorker
    -> Maybe EkgNodeMetrics
    -> MVar (Map NodeId Timer) -- ^ Keepalive timer.
    -> IO Millisecond -- ^ Slot duration; may change over time.
    -> SubscriptionStates NodeId
    -> (VerInfo -> [Listener])
    -> (FullDiffusionInternals -> IO x)
    -> IO x
runDiffusionLayerFull logTrace
                      transport
                      oq
                      convEstablishTimeout
                      ourVerInfo
                      defaultPort
                      mSubscriptionWorker
                      mEkgNodeMetrics
                      keepaliveTimerVar
                      slotDuration
                      subscriptionStates
                      listeners
                      k =
    timeWarpNode logTrace transport convEstablishTimeout ourVerInfo listeners $ \nd converse -> do
        -- Concurrently run the dequeue thread, subscription thread, and
        -- main action.
        let sendActions :: SendActions
            sendActions = makeSendActions logTrace ourVerInfo oqEnqueue converse
            dequeueDaemon = OQ.dequeueThread oq (sendMsgFromConverse converse)
            -- If there's no subscription thread, the main action with
            -- outbound queue is all we need, but if there is a subscription
            -- thread, we run it forever and race it with the others. This
            -- ensures that
            -- 1) The subscription system never stops trying.
            -- 2) The subscription system is incapable of stopping shutdown
            --    (unless it uninterruptible masks exceptions indefinitely).
            -- FIXME perhaps it's better to let the subscription thread
            -- decide if it should go forever or not. Or, demand it does,
            -- by choosing `forall x . IO x` as the result.
            withSubscriptionDaemon :: IO a -> IO (Either x a)
            withSubscriptionDaemon =
                case mSubscriptionThread sendActions of
                    Nothing -> fmap Right
                    Just subscriptionThread -> \other ->
                      -- A subscription worker can finish normally (without
                      -- exception). But we don't want that, so we'll run it
                      -- forever.
                      let subForever = subscriptionThread >> subForever
                      in  race subForever other
            mainAction = do
                maybe (pure ()) (flip registerEkgNodeMetrics nd) mEkgNodeMetrics
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

            action = Concurrently dequeueDaemon *> Concurrently mainAction

        outcome <- withSubscriptionDaemon (runConcurrently action)
        case outcome of
          Left  impossible -> pure impossible
          Right t          -> pure t
  where
    oqEnqueue :: Msg
              -> (NodeId -> VerInfo -> Conversation PackingType t)
              -> IO (Map NodeId (STM.TVar (OQ.PacketStatus t)))
    oqEnqueue msgType l = do
        itList <- OQ.enqueue oq msgType (EnqueuedConversation (msgType, l))
        return (M.fromList itList)
    mSubscriptionThread :: SendActions
                        -> Maybe (IO ())
    mSubscriptionThread sactions = case mSubscriptionWorker of
        Just (SubscriptionWorkerBehindNAT dnsDomains) -> Just $
            dnsSubscriptionWorker logTrace oq defaultPort dnsDomains keepaliveTimerVar slotDuration subscriptionStates sactions
        Just (SubscriptionWorkerKademlia _ _ _) -> Nothing
        Nothing -> Nothing

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
