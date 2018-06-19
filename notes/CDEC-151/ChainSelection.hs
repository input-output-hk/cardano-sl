{-# LANGUAGE GADTSyntax #-}

module ChainSelection where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Seq (Seq)
import qualified Data.Seq as Seq
import Data.Word

type Epoch = Word

type Slot = Word

type Hash = String

data HeaderHash = HeaderHash Hash

data BlockHeader = BlockHeader Epoch Slot Hash

data BlockBody = BlockBody String

data Block = Block BlockHeader BlockBody

headerHash :: BlockHeader -> HeaderHash
headerHash (BlockHeader epoch slot hash) = HeaderHash (show epoch <> show slot <> show hash)

bodyHash :: BlockBody -> Hash
bodyHash (BlockBody str) = str

-- Chain selection.

-- | A candidate for the best chain.
data ChainCandidate = ChainCandidate HeaderHash

-- | Facts about the best chain.
data ChainSelectionClaim where

  -- | Claim that this is the tip of the best chain.
  -- A peer can claim only one chain to be the best. If a new claim comes in
  -- it replaces the old claim.
  ClaimBestChain :: BlockHeader -> ChainSelectionClaim

  -- | This header hash is not a member of this candidate chain.
  -- In response to a 'Membership' message.
  IsNotMember :: ChainCandidate -> HeaderHash -> ChainSelectionClaim
  -- | This header hash is a member of this candidate chain.
  -- In response to a 'Membership' message.
  IsMember :: ChainCandidate -> HeaderHash -> ChainSelectionClaim

  -- | A block, in response to a request for a chain segment.
  ChainMember :: Block -> ChainSelectionClaim

-- | Queries about a chain candidate.
data ChainSelectionQuery where
  -- | Express interest in whether this header hash is in this candidate chain.
  Membership :: ChainCandidate -> HeaderHash -> ChainSelectionQuery
  -- | Express interest in the blocks in this candidate chain beginning at the
  -- child of the block with this header hash. The 'Word16' indicates how many
  -- blocks are requested.
  Members :: ChainCandidate -> HeaderHash -> Word16 -> ChainSelectionQuery

-- 'ChainSelectionClaim' and 'ChainSelectionQuery' correspond to messages on
-- the wire. If 'BlockHeader' is of bounded size then all of the messages are
-- as well.

-- | For every peer with which we have a channel (bounded number) there is
-- at most one 'ChainSelectionState' (it's created as soon as we hear a claim
-- from them).
type ChainSelectionStates = Map NodeId ChainSelectionState

-- | State of chain selection for one peer. In practice we'll deal with
-- 'Maybe ChainSelectionState' so 'Nothing' serves as the extra "we don't know
-- anything" state.
data ChainSelectionState = ChainSelectionState
  { -- | Claimed by the peer to be the tip of the best chain.
    cssClaimed      :: BlockHeader
    -- | What we know about its intersection with our best chain.
  , cssIntersection :: Intersection
  }

data Intersection where
  -- | If it's known, then we have a chain segment beginning at that block
  -- header.
  -- This is also used to express the fact that the claimed chain coincides with
  -- the local node's best chain: the header hash is that of the tip of chain,
  -- and the segment is [].
  Known :: HeaderHash -> ChainSegment -> Intersection
  -- | We've made this guess and are awaiting an answer from the peer
  -- (is it a member? yes or no).
  -- The list is the next guess(es) we'll make. Laziness is useful here.
  -- In practice this will probably need to be monadic, as we may have to do
  -- disk I/O to pull them up.
  Guess :: HeaderHash -> [HeaderHash] -> Intersection
  -- | Rare, will probably never happen in practice if the guesses are chosen
  -- wisely.
  NotFound :: Intersection

-- | In reverse order, as we only ever append older blocks.
-- It's a _valid_ chain, given some known valid chain which comes before it.
-- We'll need to be able to do a rolling verify sans-side-effects. A tall order
-- given the state of the core at the moment.
-- Also contained in here
data ChainSegment = ChainSegment
  { csReverseChain :: [Block]
  }

-- In practice a 'ChainSegment' may actually be useless, because the core
-- blockchain definitions do not allow us to verify or apply without mutating
-- the one-true-blockchain database.

-- | The single source of truth for:
--     1. The local best chain.
--     2. The claimed best chains of peers, as far as the local node can tell.
--   In practice this is influenced by:
--     1. Local minting of blocks. When it's time to create a block, the
--        'ChainSelection' is used to determine the parent of the new block (use
--        the best chain) and then the 'ChainSelection' is updated, including
--        the 'ChainSelectionStates', which may now be stale.
--     2. Claims from peers.
data ChainSelection = ChainSelection
  { -- | Head and tail of chain, tail in newest-to-oldest order.
    csBest   :: (Block, Seq Block)
  , csStates :: ChainSelectionStates
  }
