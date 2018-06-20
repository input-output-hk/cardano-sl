{-# LANGUAGE GADTSyntax #-}

module ChainSelection where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>), mempty)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Text.Lazy.Builder as Text
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.Word

import ChainExperiment2

-- |
-- = Overview
--
-- 'ChainExperiment2' gives 'STM' backed producer and consumer interfaces.
-- It looks like something that a program might use as an abstraction over
-- the blockchain broadcast and exchange protocol. In this file, I'm
-- interested only in that underlying protocol. The goal is to describe pure
-- state transitions based on a message type describing the wire protocol.
--
--   transition :: (Peer, Message)
--              -> ChainSelection
--              -> (ChainSelection, Set (Peer, Message))
--
-- For every message, the state is updated, and new messages are generated to
-- be sent out to particular peers.

-- |
-- = Node-to-node protocol
--
-- Duncan's stateful protocol has the producer track a read pointer for the
-- consumer. This seems to be appropriate for something like a wallet following
-- a full node, but for a general-purpose node-to-node protocol, neither peer
-- is uniformly a producer or consumer.
--
-- It's fair to say that they are both producers *and* consumers: producers of
-- their own chain, consumers of the peer's chain. However, if one node has a
-- strictly longer (fully verified, adopted) chain than its peer, then there's
-- no reason for it to consume the shorter one. On the other hand, the node
-- with the shorter one should consume the longer one. If their best chains are
-- of equal length, then they each should consume the other's chain, because
-- both chains are (as far as each node can tell) equally likely to be extended.
--
-- That's all to say that for any pair of nodes, either
--
--   - they have equally-long chains (possibly the same chain)
--   - one has a shorter chain and the other has a longer chain
--
-- and furthermore, they can come to agree on this by exchanging the headers of
-- the tip of their (fully verified, adopted) longest chain.
--
-- Exchanging tips drives the protocol. A peer with a tip that is of greater or
-- equal length to the other (that's possibly both peers) must respond with an
-- indication that its chain either
--
--   - Immediately continues (is a child of) the peer's tip-of-chain.
--     This of course implies the peer's chain is shorter.
--   - Non-immediately continues (is a descendent of) the peer's tip-of-chain.
--     Again, this implies the peer's chain is shorter.
--   - Is a fork (not a descendent) of the peer's tip-of-chain.
--     This is the only possibility in case they have equal-length chains.
--
-- After this, it's the consumer's turn. As soon as the intersection between
-- producer and consumer chains is known, the consumer can begin requesting
-- blocks. For the first 2 cases (descendant) the intersection is already known.
-- For the third case (fork) the producer will send checkpoints (header hashes),
-- and the consumer may respond with more checkpoints. This can continue
-- indefinitely until either end finds a checkpoint in their own best chain.
-- The number of checkpoints per message must be bounded.
-- NB: it's also possible that its a fork and the producer already knows the
-- intersection, for instance when the producer and consumer had the same
-- chain, but the producer just forked from it. In that case the producer sends
-- the intersection point and no checkpoint negotiation is needed. This is
-- the "read pointer" scheme.
--
-- To download blocks, the most straightforward solution is to have the
-- producer stream them all from the intersection point as soon as it knows
-- what that is. But this may not be appropriate, because the consumer may be
-- downloading these blocks from some other peer. It will probably be a common
-- case for a node to have multiple peers showing the same tip-of-chain.
-- It's not clear whether a node should be capable of downloading pieces of
-- the same chain from multiple peers. Doing so would make it hard to keep
-- track of the "read pointer" for a consumer: a producer can know what it has
-- delivered, but not what other nodes have delivered.
--
-- If at any point during this interaction, either peer's best chain changes,
-- whether to an extension of it or to a fork, they must send another tip
-- message. Each subsequent message to determine the intersection includes the
-- identifier (header hash) of the other peer's tip-of-chain, so it can
-- determine whether the message is still relevant in case the tip update was
-- sent concurrently with intersection negotiation messages.
--
-- If a node sends a tip message with a header which is equal length to the
-- header in its previous tip message (it can never send a shorter one) then
-- it's assumed that this node has both of those chains fully downloaded and
-- verified. This is how a node expresses that it has multiple equal length
-- chains: by announcing, and carrying out the subsequent intersection
-- negotiation and download, for each tip in the equivalence class in sequence.
--
-- Throughout this protocol, the set of best chains (equal length) at a
-- node can only improve (become longer).
--
-- In the common case when a node receives a header which immediately
-- continues its best chain, it can announce this to its peers before receiving
-- the body, which the producer will send right away.
--
-- Here is the message type.

-- | Messages related to blockchain broadcast.
data Message where

  -- | Claims this header to be the tip of the sender's best chain.
  -- The sender always has the complete valid chain terminating at this header.
  -- Whenever a node's best chain (fully verified and known) changes, it must
  -- send this to all of its peers, even those which have claimed better chains
  -- than it (but are still tentative).
  --
  -- If a 'Tip' is received and it has the same length as the previously
  -- received 'Tip', it means the peer has the complete verified chains for
  -- both of those 'Tip's. A peer never sends a 'Tip' that is shorter than its
  -- previsouly-sent 'Tip'.
  Tip        :: Header -> Message

  -- Whenever a 'Tip' is sent, the party with the longer chain must send one of
  -- the following 3 messages.
  --
  -- In each of the following 4 constructors, the first field (a 'BlockId') is
  -- that of the other peer's claimed tip. This is important in case both
  -- parties send a 'Tip' concurrently. They can use the 'BlockId' to determine
  -- that it's not relevant to their most-recently-sent 'Tip'.

  -- | Eagerly push the body of a block. Must be immediately preceded by a
  -- 'Tip' containing the header for this body.
  -- Used in cases where a producer sends a header to a peer which it knows to
  -- have the chain which it extends. The peer can relay the header before
  -- receiving the body. This is the common case of relaying a newly minted
  -- block, but happens whenever a node discovers a better chain and relays
  -- it to its peers which have 'Equal' or 'Equiv' best chains.
  Continues  :: BlockId -> Body -> Message
  -- | Producer indicates that their tip (most recently sent) extends this
  -- 'BlockId', but does not immediately continue it (is not its child).
  Extends    :: BlockId -> Message
  -- | Producer indicates that their tip (most recently sent) does not extend
  -- the consumer's tip (which is shorter).
  --
  -- There are cases in which the producer's 'Tip' is a fork of the consumer's
  -- 'Tip', but the producer already knows the intersection. For example: the
  -- producer has just moved to a fork supplied by a different peer, and now
  -- sends the new 'Tip' to the peers with whom it was formerly 'Equal'. For
  -- these peers, it can say precisely where the intersection is (it has
  -- already negotiated that with the other peer).
  --
  -- If the intersection point is known, it can be sent in 'Right'. Otherwise,
  -- some bounded number of checkpoints ('BlockId's) is sent. These are
  -- ancestors of the sending party's 'Tip'. The hope is that more often than
  -- not, the receiver's chain will contain one of these.
  Forks      :: BlockId -> Either Checkpoints BlockId -> Message

  -- In case the producer sends 'Extends' or 'Forks', the consumer is
  -- responsible for attempting to download and verify the longer chain.
  --
  -- For 'Extends', it's just a matter of downloading the headers and bodies,
  -- since the intersection is already known.
  -- The consumer may request particular ranges of blocks, making it possible
  -- to download pieces of a chain from multiple peers.

  -- | Add a ? to the end of this constructor for a more suggestive name.
  -- The consumer asks whether any of these 'BlockId's are in the producer's
  -- best chain, and also indicates that they are in the consumer's best chain.
  -- The producer replies with a 'Forks' indicating either more checkpoints
  -- (still not found) or a 'BlockId' giving the newest one that's in its
  -- chain.
  --
  -- The first 'BlockId' is that of the 'Tip' of the other party. Important in
  -- case that peer concurrently sent a new 'Tip', in which case it will
  -- discard this message.
  Intersects :: BlockId -> Checkpoints -> Message

  -- | Request blocks by giving a reference point ('BlockId'), an offset
  -- (where to start, with the child of the reference point being 0), and range
  -- (how many contiguous blocks to give).
  Blocks     :: BlockId -> Offset -> Range -> Message

  -- | Deliver a block in response to a 'Blocks' request.
  Block      :: Header -> Body -> Message

type Offset = Word16
type Range = Word16
type Peer = String

data Header = Header
data Body = Body

type Checkpoints = [BlockId]

data PeerState where
  -- | Haven't heard this peer's tip-of-chain.
  Unknown :: PeerState
  -- | Every 'Header' in the 'NonEmpty' has the same block count (length),
  -- and the 'Ordering' is 'GT' if _our_ chain is longer than these, 'LT' of
  -- our chain is shorter than these, 'EQ' if the same length.
  Known :: NonEmpty Header -> Ordering -> PeerState

data ChainSelection = ChainSelection
  { csPeers :: !(Map Peer PeerState)
    -- | Values include a parent (second component) and children.
    -- Children can be other headers, as well as the peer which claims this as
    -- tip-of-chain.
  , csTree  :: !(Map BlockId (Header, OutEdge, [InEdge]))
  , csBest  :: !BlockId
  , csRoots :: !(NonEmpty BlockId)
  }

newtype OutEdge = OutEdge { getOutEdge :: BlockId }

data InEdge where
  InBlock :: BlockId -> InEdge
  InPeer  :: Peer -> InEdge
  InBest  :: InEdge

transition
  :: (Peer, Message)
  -> ChainSelection
  -> (ChainSelection, Set (Peer, Message))
transition (peer, message) = undefined
