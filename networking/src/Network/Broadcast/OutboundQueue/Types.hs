{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Network.Broadcast.OutboundQueue.Types
    ( EnqueueTo(..)
    , Peers(..)
    , classifyNode
    , classifyNodeDefault
    , Routes(..)
    , AllOf
    , Alts
    , routesOfType
    , simplePeers
    , peersFromList
    , peersRouteSet
    , peersSet
    , removePeer
    , restrictPeers
    , MsgType(..)
    , Origin(..)
    , msgOrigin
    , msgEnqueueTo
    , NodeType(..)
    , FormatMsg(..)
    ) where

import           Universum

import           Control.Lens (makeLenses)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Formatting

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
  deriving (Show, Eq, Ord, Bounded, Enum)

{-------------------------------------------------------------------------------
  Known peers
-------------------------------------------------------------------------------}

data EnqueueTo nid =
    -- | Enqueue to all known peers
    EnqueueToAll

    -- | Enqueue to a specific set of peers. This must be a subset of the
    -- known peers.
  | EnqueueToSubset (Set nid)
  deriving (Show)

-- | Classification of a set of 'nid's¸ along with routes on a subset of that
--   classification: every 'nid' in the 'Routes nid' is a key in the
--   'Map nid NodeType'.
--   Keep that in mind when, for example, defining an 'Arbitrary' instance on
--   'Peers nid'.
data Peers nid = Peers
    { peersRoutes         :: Routes nid
    , peersClassification :: Map nid NodeType
    } deriving (Show, Eq)

classifyNode :: Ord nid => Peers nid -> nid -> Maybe NodeType
classifyNode Peers {..} nid = Map.lookup nid peersClassification

classifyNodeDefault :: Ord nid => Peers nid -> NodeType -> nid -> NodeType
classifyNodeDefault peers deflt = maybe deflt identity . classifyNode peers

-- | All known peers, split per node type, in order of preference
data Routes nid = Routes {
      _routesCore  :: AllOf (Alts nid)
    , _routesRelay :: AllOf (Alts nid)
    , _routesEdge  :: AllOf (Alts nid)
    }
  deriving (Show, Eq)

-- | List of forwarding sets
--
-- Each of these need to be contacted (in arbitrary order)
type AllOf a = [a]

-- | Single forwarding set
--
-- Non-empty list of alternatives (in order of preference)
type Alts a = [a]

makeLenses ''Routes

instance Functor Routes where
  fmap f Routes{..} = Routes{
        _routesCore  = map (map f) _routesCore
      , _routesRelay = map (map f) _routesRelay
      , _routesEdge  = map (map f) _routesEdge
      }

routesOfType :: NodeType -> Lens' (Routes nid) (AllOf (Alts nid))
routesOfType NodeCore  = routesCore
routesOfType NodeRelay = routesRelay
routesOfType NodeEdge  = routesEdge

-- | Restrict a 'Peers nid' to those members of some 'Set nid'.
restrictPeers :: forall nid . Ord nid => Set nid -> Peers nid -> Peers nid
restrictPeers restriction peers = Peers
    { peersRoutes         = Routes
        { _routesCore  = restrict _routesCore
        , _routesRelay = restrict _routesRelay
        , _routesEdge  = restrict _routesEdge
        }
    , peersClassification = Map.filterWithKey (\k _ -> k `Set.member` restriction) classification
    }
  where
    Routes {..} = peersRoutes peers
    classification = peersClassification peers
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
simplePeers :: forall nid. Ord nid => [(NodeType, nid)] -> Peers nid
simplePeers = peersFromList mempty . map (second (:[]))

-- | Construct 'Peers' from a list of alternatives and their types (routes) and
--   some extra classified nodes (not in the routes).
peersFromList :: forall nid. Ord nid => Map nid NodeType -> [(NodeType, Alts nid)] -> Peers nid
peersFromList notRouted = go start
  where
    start :: Peers nid
    start = mempty { peersClassification = notRouted }
    go :: Peers nid -> [(NodeType, Alts nid)] -> Peers nid
    go acc []                  = acc
    go acc ((typ, alts):altss) = -- go (acc & routesOfType typ %~ (alts :)) altss
      let routes = peersRoutes acc
          routes' = routes & routesOfType typ %~ (alts :)
          newClassifications = Map.fromList (fmap (flip (,) typ) alts)
          classification = peersClassification acc
          classification' = Map.union newClassifications classification
          acc' = acc
              { peersRoutes = routes'
              , peersClassification = classification'
              }
      in  go acc' altss

-- | The set of all peers which appear in a route.
--   Always a subset of 'peersSet'.
peersRouteSet :: Ord nid => Peers nid -> Set nid
peersRouteSet Peers{..} = Set.unions . concat $ [
      map Set.fromList (_routesCore peersRoutes)
    , map Set.fromList (_routesRelay peersRoutes)
    , map Set.fromList (_routesEdge peersRoutes)
    ]

-- | The set of all peers which are classified.
--   Always a superset of 'peersRouteSet'
peersSet :: Peers nid -> Set nid
peersSet Peers{..} = Map.keysSet peersClassification

instance Semigroup (Routes nid) where
    a <> b = Routes {
                      _routesCore  = comb _routesCore
                    , _routesRelay = comb _routesRelay
                    , _routesEdge  = comb _routesEdge
                    }
      where
        comb :: Semigroup a => (Routes nid -> a) -> a
        comb f = f a <> f b

instance Monoid (Routes nid) where
    mempty  = Routes [] [] []
    mappend = (<>)

-- These instances are somewhat irresponsible: if both sides contain
-- the same 'nid' but have a different classification for it, the one
-- on the left will be taken.
instance Ord nid => Semigroup (Peers nid) where
    (Peers a b) <> (Peers c d) = Peers (a <> c) (b <>d)

instance Ord nid => Monoid (Peers nid) where
    mempty = Peers mempty mempty
    mappend = (<>)

removePeer :: forall nid. Ord nid => nid -> Peers nid -> Peers nid
removePeer toRemove peers =
    let routes' =
              (peersRoutes peers)
            & routesCore  %~ remove
            & routesRelay %~ remove
            & routesEdge  %~ remove
        classification' = Map.delete toRemove (peersClassification peers)
    in  Peers
            { peersRoutes = routes'
            , peersClassification = classification'
            }
  where
    -- Removes the peer `toRemove` from the list of alternatives, skipping
    -- any empty list encountered, which is discarded as meaningless information.
    remove :: AllOf (Alts nid) -> AllOf (Alts nid)
    remove = filter (not . null) . map (filter (/= toRemove))

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
    -- Opitionally give a set of targets.
  | MsgRequestBlockHeaders (Maybe (Set nid))

    -- | Request for a specific block from these peers.
  | MsgRequestBlocks (Set nid)

    -- | New transaction
  | MsgTransaction (Origin nid)

    -- | MPC messages
  | MsgMPC (Origin nid)
  deriving (Show)

msgOrigin :: MsgType nid -> Origin nid
msgOrigin msg = case msg of
  MsgAnnounceBlockHeader origin -> origin
  MsgRequestBlockHeaders _      -> OriginSender
  MsgRequestBlocks _            -> OriginSender
  MsgTransaction origin         -> origin
  MsgMPC origin                 -> origin

msgEnqueueTo :: MsgType nid -> EnqueueTo nid
msgEnqueueTo msg = case msg of
  MsgRequestBlockHeaders (Just peers) -> EnqueueToSubset peers
  MsgRequestBlocks peers              -> EnqueueToSubset peers
  _                                   -> EnqueueToAll

class FormatMsg msg where
  formatMsg :: forall r a. Format r (msg a -> r)
