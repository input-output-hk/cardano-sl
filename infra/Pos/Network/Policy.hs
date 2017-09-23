module Pos.Network.Policy
       ( defaultEnqueuePolicyCore
       , defaultEnqueuePolicyRelay
       , defaultEnqueuePolicyEdgeBehindNat
       , defaultEnqueuePolicyEdgeP2P
       , defaultDequeuePolicyCore
       , defaultDequeuePolicyRelay
       , defaultDequeuePolicyEdgeBehindNat
       , defaultDequeuePolicyEdgeP2P
       , defaultFailurePolicy
       ) where


import           Universum

import           Network.Broadcast.OutboundQueue
import           Network.Broadcast.OutboundQueue.Types

-- | Default enqueue policy for core nodes
defaultEnqueuePolicyCore :: EnqueuePolicy nid
defaultEnqueuePolicyCore = go
  where
    go :: EnqueuePolicy nid
    go (MsgAnnounceBlockHeader _) = [
        EnqueueAll NodeCore  (MaxAhead 0) PHighest
      , EnqueueAll NodeRelay (MaxAhead 0) PHighest
      ]
    go (MsgRequestBlockHeaders Nothing) = [
        EnqueueAll NodeCore  (MaxAhead 2) PHigh
      , EnqueueAll NodeRelay (MaxAhead 2) PHigh
      ]
    go (MsgRequestBlockHeaders (Just _)) = [
        -- We never ask for data from edge nodes
        EnqueueOne [NodeRelay, NodeCore] (MaxAhead 3) PHigh
      ]
    go (MsgRequestBlocks _) = [
        -- We never ask for data from edge nodes
        EnqueueOne [NodeRelay, NodeCore] (MaxAhead 3) PHigh
      ]
    go (MsgMPC _) = [
        EnqueueAll NodeCore (MaxAhead 2) PMedium
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
      , EnqueueAll NodeCore  (MaxAhead 0) PHighest
      , EnqueueAll NodeEdge  (MaxAhead 0) PMedium
      ]
    go (MsgRequestBlockHeaders Nothing) = [
        EnqueueAll NodeCore  (MaxAhead 2) PHigh
      , EnqueueAll NodeRelay (MaxAhead 2) PHigh
      ]
    go (MsgRequestBlockHeaders (Just _)) = [
        -- We never ask for data from edge nodes
        EnqueueOne [NodeRelay, NodeCore] (MaxAhead 3) PHigh
      ]
    go (MsgRequestBlocks _) = [
        -- We never ask for blocks from edge nodes
        EnqueueOne [NodeRelay, NodeCore] (MaxAhead 3) PHigh
      ]
    go (MsgTransaction _) = [
        EnqueueAll NodeCore  (MaxAhead 200) PLow
      , EnqueueAll NodeRelay (MaxAhead 200) PLow
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
        EnqueueAll NodeRelay (MaxAhead 2) PLow
      ]
    go (MsgTransaction (OriginForward _)) = [
        -- don't forward transactions that weren't created at this node
      ]
    go (MsgAnnounceBlockHeader _) = [
        -- not forwarded
      ]
    go (MsgRequestBlockHeaders Nothing) = [
        EnqueueAll NodeRelay (MaxAhead 1) PHigh
      ]
    go (MsgRequestBlockHeaders (Just _)) = [
        EnqueueOne [NodeRelay] (MaxAhead 2) PHigh
      ]
    go (MsgRequestBlocks _) = [
        -- Edge nodes can only talk to relay nodes
        EnqueueOne [NodeRelay] (MaxAhead 2) PHigh
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
    go (MsgRequestBlockHeaders _) = [
        EnqueueAll NodeRelay (MaxAhead 2) PHigh
      ]
    go (MsgRequestBlocks _) = [
        -- Edge nodes can only talk to relay nodes
        EnqueueOne [NodeRelay] (MaxAhead 3) PHigh
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
    go NodeCore  = Dequeue (MaxMsgPerSec 3) (MaxInFlight 2)
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
