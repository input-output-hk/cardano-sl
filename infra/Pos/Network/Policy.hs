module Pos.Network.Policy
    ( defaultEnqueuePolicyCore
    , defaultEnqueuePolicyRelay
    , defaultEnqueuePolicyEdgeBehindNat
    , defaultEnqueuePolicyEdgeP2P
    , defaultEnqueuePolicyEdgeExchange
    , defaultDequeuePolicyCore
    , defaultDequeuePolicyRelay
    , defaultDequeuePolicyEdgeBehindNat
    , defaultDequeuePolicyEdgeP2P
    , defaultDequeuePolicyEdgeExchange
    , defaultFailurePolicy

    , fromStaticPolicies
    ) where

import           Universum
import           Network.Broadcast.OutboundQueue.Types
import           Network.Broadcast.OutboundQueue
import qualified Pos.Network.Yaml as Y

fromStaticPolicies
    :: Y.StaticPolicies
    -> (EnqueuePolicy nid, DequeuePolicy, FailurePolicy nid)
fromStaticPolicies staticPolicies =
    ( enqueuePolicy
    , dequeuePolicy
    , failurePolicy
    )
  where
    enqueuePolicy msgType =
        let staticEnqueues = Y.getStaticEnqueuePolicy (Y.staticEnqueuePolicy staticPolicies) msgType
            mkEnqueue it@(Y.StaticEnqueueAll {..}) = EnqueueAll
                { enqNodeType   = Y.senqNodeType it
                , enqMaxAhead   = MaxAhead (fromIntegral (Y.senqMaxAhead it))
                , enqPrecedence = Y.senqPrecedence it
                }
            mkEnqueue it@(Y.StaticEnqueueOne {..}) = EnqueueOne
                { enqNodeTypes  = Y.senqNodeTypes it
                , enqMaxAhead   = MaxAhead (fromIntegral (Y.senqMaxAhead it))
                , enqPrecedence = Y.senqPrecedence it
                }
        in  fmap mkEnqueue staticEnqueues
    dequeuePolicy nodeType =
        let staticDequeue = Y.getStaticDequeuePolicy (Y.staticDequeuePolicy staticPolicies) nodeType
        in  Dequeue {
                  deqRateLimit = maybe NoRateLimiting (MaxMsgPerSec . fromIntegral) (Y.sdeqRateLimit staticDequeue)
                , deqMaxInFlight = MaxInFlight (fromIntegral (Y.sdeqMaxInFlight staticDequeue))
                }
    failurePolicy nodeType msgType _ =
        let seconds = Y.getStaticFailure (Y.getStaticFailurePolicy (Y.staticFailurePolicy staticPolicies) nodeType) msgType
        in  ReconsiderAfter (fromIntegral seconds)

-- | Default enqueue policy for core nodes
defaultEnqueuePolicyCore :: EnqueuePolicy nid
defaultEnqueuePolicyCore = go
  where
    go :: EnqueuePolicy nid
    go (MsgAnnounceBlockHeader _) = [
        EnqueueAll NodeCore  (MaxAhead 0) PHighest
      , EnqueueAll NodeRelay (MaxAhead 0) PHigh
      ]
    go MsgRequestBlockHeaders = [
        EnqueueAll NodeCore  (MaxAhead 1) PHigh
      , EnqueueAll NodeRelay (MaxAhead 1) PHigh
      ]
    go (MsgRequestBlocks _) = [
        -- We never ask for data from edge nodes
        EnqueueOne [NodeRelay, NodeCore] (MaxAhead 1) PHigh
      ]
    go (MsgMPC _) = [
        EnqueueAll NodeCore (MaxAhead 1) PMedium
        -- not sent to relay nodes
      ]
    go (MsgTransaction _) = [
        EnqueueAll NodeCore (MaxAhead 20) PLow
        -- not sent to relay nodes
      ]

-- | Default enqueue policy for relay nodes
defaultEnqueuePolicyRelay :: EnqueuePolicy nid
defaultEnqueuePolicyRelay = go
  where
    -- Enqueue policy for relay nodes
    go :: EnqueuePolicy nid
    go (MsgAnnounceBlockHeader _) = [
        EnqueueAll NodeRelay (MaxAhead 0) PHighest
      , EnqueueAll NodeCore  (MaxAhead 0) PHigh
      , EnqueueAll NodeEdge  (MaxAhead 0) PMedium
      ]
    go MsgRequestBlockHeaders = [
        EnqueueAll NodeCore  (MaxAhead 1) PHigh
      , EnqueueAll NodeRelay (MaxAhead 1) PHigh
      ]
    go (MsgRequestBlocks _) = [
        -- We never ask for blocks from edge nodes
        EnqueueOne [NodeRelay, NodeCore] (MaxAhead 1) PHigh
      ]
    go (MsgTransaction _) = [
        EnqueueAll NodeCore  (MaxAhead 20) PLow
      , EnqueueAll NodeRelay (MaxAhead 20) PLow
        -- transactions not forwarded to edge nodes
      ]
    go (MsgMPC _) = [
        -- Relay nodes never sent any MPC messages to anyone
      ]

-- | Default enqueue policy for standard behind-NAT edge nodes
defaultEnqueuePolicyEdgeBehindNat :: EnqueuePolicy nid
defaultEnqueuePolicyEdgeBehindNat = go
  where
    -- Enqueue policy for edge nodes
    go :: EnqueuePolicy nid
    go (MsgTransaction OriginSender) = [
        EnqueueAll NodeRelay (MaxAhead 1) PLow
      ]
    go (MsgTransaction (OriginForward _)) = [
        -- don't forward transactions that weren't created at this node
      ]
    go (MsgAnnounceBlockHeader _) = [
        -- not forwarded
      ]
    go MsgRequestBlockHeaders = [
        EnqueueAll NodeRelay (MaxAhead 0) PHigh
      ]
    go (MsgRequestBlocks _) = [
        -- Edge nodes can only talk to relay nodes
        EnqueueOne [NodeRelay] (MaxAhead 0) PHigh
      ]
    go (MsgMPC _) = [
        -- not relevant
      ]

-- | Default enqueue policy for exchange nodes
defaultEnqueuePolicyEdgeExchange :: EnqueuePolicy nid
defaultEnqueuePolicyEdgeExchange = go
  where
    -- Enqueue policy for edge nodes
    go :: EnqueuePolicy nid
    go (MsgTransaction OriginSender) = [
        EnqueueAll NodeRelay (MaxAhead 6) PLow
      ]
    go (MsgTransaction (OriginForward _)) = [
        -- don't forward transactions that weren't created at this node
      ]
    go (MsgAnnounceBlockHeader _) = [
        -- not forwarded
      ]
    go MsgRequestBlockHeaders = [
        EnqueueAll NodeRelay (MaxAhead 0) PHigh
      ]
    go (MsgRequestBlocks _) = [
        -- Edge nodes can only talk to relay nodes
        EnqueueOne [NodeRelay] (MaxAhead 0) PHigh
      ]
    go (MsgMPC _) = [
        -- not relevant
      ]

-- | Default enqueue policy for edge nodes using P2P
defaultEnqueuePolicyEdgeP2P :: EnqueuePolicy nid
defaultEnqueuePolicyEdgeP2P = go
  where
    -- Enqueue policy for edge nodes
    go :: EnqueuePolicy nid
    go (MsgTransaction _) = [
        EnqueueAll NodeRelay (MaxAhead 3) PLow
      ]
    go (MsgAnnounceBlockHeader _) = [
        EnqueueAll NodeRelay (MaxAhead 0) PHighest
      ]
    go MsgRequestBlockHeaders = [
        EnqueueAll NodeRelay (MaxAhead 1) PHigh
      ]
    go (MsgRequestBlocks _) = [
        -- Edge nodes can only talk to relay nodes
        EnqueueOne [NodeRelay] (MaxAhead 1) PHigh
      ]
    go (MsgMPC _) = [
        -- not relevant
      ]

-- | Default dequeue policy for core nodes
defaultDequeuePolicyCore :: DequeuePolicy
defaultDequeuePolicyCore = go
  where
    go :: DequeuePolicy
    go NodeCore  = Dequeue NoRateLimiting (MaxInFlight 3)
    go NodeRelay = Dequeue NoRateLimiting (MaxInFlight 2)
    go NodeEdge  = error "defaultDequeuePolicy: core to edge not applicable"

-- | Dequeueing policy for relay nodes
defaultDequeuePolicyRelay :: DequeuePolicy
defaultDequeuePolicyRelay = go
  where
    go :: DequeuePolicy
    go NodeCore  = Dequeue (MaxMsgPerSec 1) (MaxInFlight 2)
    go NodeRelay = Dequeue (MaxMsgPerSec 3) (MaxInFlight 2)
    go NodeEdge  = Dequeue (MaxMsgPerSec 1) (MaxInFlight 2)

-- | Dequeueing policy for standard behind-NAT edge nodes
defaultDequeuePolicyEdgeBehindNat :: DequeuePolicy
defaultDequeuePolicyEdgeBehindNat = go
  where
    go :: DequeuePolicy
    go NodeCore  = error "defaultDequeuePolicy: edge to core not applicable"
    go NodeRelay = Dequeue (MaxMsgPerSec 1) (MaxInFlight 2)
    go NodeEdge  = error "defaultDequeuePolicy: edge to edge not applicable"

-- | Dequeueing policy for exchange edge nodes
defaultDequeuePolicyEdgeExchange :: DequeuePolicy
defaultDequeuePolicyEdgeExchange = go
  where
    go :: DequeuePolicy
    go NodeCore  = error "defaultDequeuePolicy: edge to core not applicable"
    go NodeRelay = Dequeue (MaxMsgPerSec 5) (MaxInFlight 3)
    go NodeEdge  = error "defaultDequeuePolicy: edge to edge not applicable"

-- | Dequeueing policy for P2P edge nodes
defaultDequeuePolicyEdgeP2P :: DequeuePolicy
defaultDequeuePolicyEdgeP2P = go
  where
    go :: DequeuePolicy
    go NodeCore  = error "defaultDequeuePolicy: edge to core not applicable"
    go NodeRelay = Dequeue (MaxMsgPerSec 1) (MaxInFlight 2)
    go NodeEdge  = error "defaultDequeuePolicy: edge to edge not applicable"

-- | Default failure policy
--
-- TODO: Implement proper policy
defaultFailurePolicy :: NodeType -- ^ Our node type
                     -> FailurePolicy nid
defaultFailurePolicy _ourType _theirType _msgType _err = ReconsiderAfter 200
