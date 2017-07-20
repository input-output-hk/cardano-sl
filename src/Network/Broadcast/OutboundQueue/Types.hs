{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Broadcast.OutboundQueue.Types (
    EnqueueTo (..)
  , Peers (..)
  , AllOf
  , Alts
  , peersOfType
  , simplePeers
  , peersFromList
  , removePeer
  , restrictPeers
  , MsgType(..)
  , Origin(..)
  , msgOrigin
  , msgEnqueueTo
  , NodeType(..)
  , FormatMsg(..)
  ) where

import Data.Bifunctor (second)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Control.Lens
import Formatting

{-------------------------------------------------------------------------------
  Known peers
-------------------------------------------------------------------------------}

data EnqueueTo nid =
    -- | Enqueue to all known peers
    EnqueueToAll

    -- | Enqueue to a specific set of peers. This must be a subset of the
    -- known peers.
  | EnqueueToParticular (Set nid)

-- Requirement: efficient subset on Peers.

-- | All known peers, split per node type, in order of preference
data Peers nid = Peers {
      _peersCore  :: AllOf (Alts nid)
    , _peersRelay :: AllOf (Alts nid)
    , _peersEdge  :: AllOf (Alts nid)
    }
  deriving (Show)

-- | List of forwarding sets
--
-- Each of these need to be contacted (in arbitrary order)
type AllOf a = [a]

-- | Single forwarding set
--
-- Non-empty list of alternatives (in order of preference)
type Alts a = [a]

makeLenses ''Peers

instance Functor Peers where
  fmap f Peers{..} = Peers{
        _peersCore  = map (map f) _peersCore
      , _peersRelay = map (map f) _peersRelay
      , _peersEdge  = map (map f) _peersEdge
      }

peersOfType :: NodeType -> Lens' (Peers nid) (AllOf (Alts nid))
peersOfType NodeCore  = peersCore
peersOfType NodeRelay = peersRelay
peersOfType NodeEdge  = peersEdge

-- | Restrict a 'Peers nid' to those members of some 'Set nid'.
restrictPeers :: forall nid . Ord nid => Set nid -> Peers nid -> Peers nid
restrictPeers restriction  Peers {..} = Peers
    { _peersCore  = restrict _peersCore
    , _peersRelay = restrict _peersRelay
    , _peersEdge  = restrict _peersEdge
    }
  where
    predicate :: nid -> Bool
    predicate = flip Set.member restriction
    restrict :: AllOf (Alts nid) -> AllOf (Alts nid)
    restrict = mapMaybe (pickNonEmpty . filter predicate)
    pickNonEmpty :: forall a . Alts a -> Maybe (Alts a)
    pickNonEmpty [] = Nothing
    pickNonEmpty xs = Just xs

-- | Construct 'Peers' from a list of node IDs
--
-- This effectively means that all of these peers will be sent all (relevant)
-- messages.
simplePeers :: forall nid. [(NodeType, nid)] -> Peers nid
simplePeers = peersFromList . map (second (:[]))

-- | Construct 'Peers' from a list of alternatives and their types
peersFromList :: forall nid. [(NodeType, Alts nid)] -> Peers nid
peersFromList = go mempty
  where
    go :: Peers nid -> [(NodeType, Alts nid)] -> Peers nid
    go acc []                  = acc
    go acc ((typ, alts):altss) = go (acc & peersOfType typ %~ (alts :)) altss

instance Monoid (Peers nid) where
  mempty      = Peers [] [] []
  mappend a b = Peers {
                    _peersCore  = comb _peersCore
                  , _peersRelay = comb _peersRelay
                  , _peersEdge  = comb _peersEdge
                  }
    where
      comb :: Monoid a => (Peers nid -> a) -> a
      comb f = f a `mappend` f b

removePeer :: forall nid. Ord nid => nid -> Peers nid -> Peers nid
removePeer toRemove peers =
    peers & peersCore  %~ remove
          & peersRelay %~ remove
          & peersEdge  %~ remove
  where
    remove :: AllOf (Alts nid) -> AllOf (Alts nid)
    remove = map $ filter (/= toRemove)

{-------------------------------------------------------------------------------
  Classification of messages and destinations
-------------------------------------------------------------------------------}

-- | Where did the message we're sending originate?
--
-- We need this because, for example, edge nodes will want to send /their/
-- transactions to relay nodes, but not any transactions that they /received/
-- from relay nodes.
data Origin nid =
    -- | It originated at the node who's sending it
    --
    -- For instance, for a transaction this means it was created on this (edge)
    -- node; for a block it would mean it was constructed on this (core) node.
    OriginSender

    -- | It originated elsewhere; we're just forwarding it
    --
    -- We record on behalf of whom we're forwarding so that we can avoid
    -- sending it straight back to them.
  | OriginForward nid
  deriving (Eq, Ord, Show)

-- | Message types
data MsgType nid =
    -- | Announcement of a new block
    MsgAnnounceBlockHeader (Origin nid)

    -- | Request block headers (either specific range or the tip)
  | MsgRequestBlockHeaders

    -- | Request for a specific block from these peers.
  | MsgRequestBlock (Set nid)

    -- | New transaction
  | MsgTransaction (Origin nid)

    -- | MPC messages
  | MsgMPC (Origin nid)
  deriving (Show)

msgOrigin :: MsgType nid -> Origin nid
msgOrigin msg = case msg of
  MsgAnnounceBlockHeader origin -> origin
  MsgRequestBlockHeaders -> OriginSender
  MsgRequestBlock _ -> OriginSender
  MsgTransaction origin -> origin
  MsgMPC origin -> origin

msgEnqueueTo :: MsgType nid -> EnqueueTo nid
msgEnqueueTo msg = case msg of
  MsgRequestBlock peers -> EnqueueToParticular peers
  _ -> EnqueueToAll

-- | Node types
data NodeType =
    -- | Core node
    --
    -- Core nodes:
    --
    -- * can become slot leader
    -- * never create currency transactions
    NodeCore

    -- | Edge node
    --
    -- Edge nodes:
    --
    -- * cannot become slot leader
    -- * creates currency transactions,
    -- * cannot communicate with core nodes
    -- * may or may not be behind NAT/firewalls
  | NodeEdge

    -- | Relay node
    --
    -- Relay nodes:
    --
    -- * cannot become slot leader
    -- * never create currency transactions
    -- * can communicate with core nodes
  | NodeRelay
  deriving (Show, Eq, Ord)

class FormatMsg msg where
  formatMsg :: forall r a. Format r (msg a -> r)
