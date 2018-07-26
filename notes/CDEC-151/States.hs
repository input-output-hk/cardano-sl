{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE FlexibleInstances #-}

module States where

import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Semigroup (..))
import Data.Word (Word16)

data Header = Header
  { headerHash       :: HeaderHash
  , headerParent     :: HeaderHash
  , headerSlot       :: Slot
  , headerBlockCount :: BlockCount
  }
newtype Body = Body
  { getBody :: ByteString
  }
type Block = (Header, Body)
type Chain = NonEmpty Block
type HeaderHash = Int
type BlockId = HeaderHash
type Slot = Word
type BlockCount = Word

tipHeader :: Chain -> Header
tipHeader = fst . NE.head

tipBody :: Chain -> Body
tipBody = snd . NE.head

blockCount :: Header -> Word
blockCount = error "blockCount"

-- | Comparison based on block count. As usual, the result refers to the
-- left-hand-side: LT means it is less than the right-hand-side.
compareBlockCount :: Header -> Header -> Ordering
compareBlockCount left right = blockCount left `compare` blockCount right

-- | Comparison based on slot count. As usual, the result refers to the
-- left-hand-side: LT means it is less than the right-hand-side.
compareSlot :: Header -> Header -> Ordering
compareSlot left right = headerSlot left `compare` headerSlot right

-- | Comparison based on block count, breaking ties using slot count (higher
-- is better).
compareHeaders :: Header -> Header -> Ordering
compareHeaders left right = case left `compareBlockCount` right of
  LT -> LT
  GT -> GT
  EQ -> left `compareSlot` right

-- | Does the header immediately continue the chain?
continues :: Header -> Chain -> Bool
continues header chain = headerParent header == headerHash (tipHeader chain)

-- | A complete chain, or a chain with a header as the newest, awaiting the
-- body to come through. This is to support relaying a header while the body
-- is inbound (fast relay).
data ChainCandidate where
  Complete   :: Chain -> ChainCandidate
  -- | The 'Header' extends the 'Chain'.
  -- @header `continues` chain = True@
  Incomplete :: Header -> Chain -> ChainCandidate

-- | The ordering uses 'compareHeaders' on the tips of the candidates.
-- In the 'EQ' case, it's possible that one of the candidates is complete
-- and the other is incomplete.
compareChainCandidates :: ChainCandidate -> ChainCandidate -> Ordering
compareChainCandidates left right = headerLeft `compareHeaders` headerRight
  where
  headerLeft = chainCandidateHeader left
  headerRight = chainCandidateHeader right

-- | Give the 'Body' which is the difference between the left and right
-- candidates: it's in the left (complete), but not the right (incomplete),
-- and completes the right.
completionOf :: ChainCandidate -> ChainCandidate -> Maybe Body
completionOf left right = case left of
  Complete chain -> case right of
    Incomplete header _ ->
      if headerHash (tipHeader chain) == headerHash header
      then Just (tipBody chain)
      else Nothing
    _ -> Nothing
  _ -> Nothing

data ChainCandidateRelation where
  -- For an improvement, we get the header of the left one, and its body if
  -- we have it.
  Improvement :: Header -> Maybe Body -> ChainCandidateRelation
  Same        :: ChainCandidateRelation
  Degradation :: ChainCandidateRelation

chainCandidateRelation :: ChainCandidate -> ChainCandidate -> ChainCandidateRelation
chainCandidateRelation left right = case left `compareChainCandidates` right of
  LT -> Degradation
  -- If they're equal, it's still possible that the left one has a body and
  -- the right one does not.
  EQ -> case left `completionOf` right of
    Nothing   -> Same
    Just body -> Improvement header (Just body)
  GT -> case right of
    Complete chain -> Improvement header (Just body)
      where
      (header, body) = NE.head chain
    Incomplete header _ -> Improvement header Nothing
  where
  header = chainCandidateHeader left

-- | Comparison using length (block count) as well as slot number to break
-- ties. 
compareToChainCandidate :: Header -> ChainCandidate -> Ordering
compareToChainCandidate h cc = h `compareHeaders` candidateHeader
  where
  candidateHeader = chainCandidateHeader cc

-- | The tip header of the candidate.
chainCandidateHeader :: ChainCandidate -> Header
chainCandidateHeader cc = case cc of
  Complete c -> tipHeader c
  Incomplete h _ -> h

-- | 'True' if it's complete.
chainCandidateIsComplete :: ChainCandidate -> Bool
chainCandidateIsComplete cc = case cc of
  Complete _ -> True
  Incomplete _ _ -> False

-- | Pick the better candidate using 'compareHeaders'. Right-biased in case of
-- equality.
betterChainCandidate :: ChainCandidate -> ChainCandidate -> ChainCandidate
betterChainCandidate left right = case leftHeader `compareHeaders` rightHeader of
  LT -> right
  GT -> left
  EQ -> right
  where
  leftHeader = chainCandidateHeader left
  rightHeader = chainCandidateHeader right

gatherCheckpoints :: ChainCandidate -> Checkpoints
gatherCheckpoints cc = case cc of
  Complete chain -> fmap (headerHash . fst) (take 8 (NE.tail chain))
  Incomplete _ chain -> fmap (headerHash . fst) (NE.take 8 chain)

-- | Identifier of a peer. In practice will probably be a 'ByteString'
-- delivered by the network layer.
data Peer = Peer String
  deriving (Eq, Ord)

-- | For resolving an intersection / read pointer.
-- In practice we'll want to limit the number of these which can be sent in
-- one message.
type Checkpoints = [BlockId]

-- | Description of the read pointer, for producer and consumer alike.
data Cursor where
  -- | Sent these checkpoints to the peer, but no intersection agreed upon yet.
  -- 'Nothing' is used by the peer with the shorter chain, while waiting for
  -- the peer with the longer chain to either give the intersection if it's
  -- known, or kick off the refinement process via checkpointing.
  Refining :: Maybe Checkpoints -> Cursor
  -- | This is the read pointer: for producer, the last header sent; for
  -- consumer, the last header received.
  Found :: Header -> Cursor

-- | Network message description
data Msg where
  -- | Announcement of the best local 'ChainCandidate', by way of its tip
  -- 'Header'.
  MsgAnnounce :: Announce -> Msg
  -- | Refinement of the intersection / read pointer.
  MsgRefine   :: Refine -> Msg
  -- | Request for blocks and headers.
  MsgRequest  :: Request -> Msg
  -- | Delivery of blocks and headers.
  MsgDeliver  :: Deliver -> Msg

type IsComplete = Bool

-- | An announcement indicates whether it's of a chain that the sender knows
-- fully ('True') or whether they have it all but the body of the tip ('False').
-- The latter case is needed to support fast relay: in this way a node
-- announces that it intends to send the body as soon as it's known, but also
-- that it would like to be sent that body.
data Announce where
  Announce :: Header -> IsComplete -> Announce

-- | In these constructors, The 'BlockId' of the other party's tip, as known by
-- the sending party, is included so that in case the other party sent a new
-- announce concurrently, it can be determined that this refine message is now
-- irrelevant (new refine phase begins).
data Refine where
  -- | Either end can send more checkpoints which are in their chain. There
  -- needs to be some reasonable limit, probably 8 or 16.
  Refine :: BlockId -> Checkpoints -> Refine
  -- | Either end can terminate the refine phase by settling on a particular
  -- intersection point (must be in both of their chains, i.e. present in a
  -- 'Refine' message sent by the other, or the checkpoints given by the
  -- producer in 'Forks'.
  Settle :: BlockId -> BlockId -> Refine

type Range = Word16

data Request where
  -- | Relative to the read pointer, established by the refine phase.
  -- The 'BlockId' identifies the refine phase (identifier of peer's tip).
  RequestHeaders :: BlockId -> Range -> Request
  -- | Random access using a 'BlockId'.
  -- The 'BlockId' identifying the refine phase (identifier of peer's tip) is
  -- _not_ present because it's not needed, as body requests are not relative
  -- to a read pointer.
  --
  -- This can also come in unsolicited, for fast block relay.
  RequestBody    :: BlockId -> Request

data Deliver where
  -- | The 'BlockId' identifies the refine phase (identifier of peer's tip).
  DeliverHeader :: BlockId -> Header -> Deliver
  -- | The 'BlockId' identifying the refine phase (identifier of peer's tip) is
  -- _not_ present because it's not needed, as body requests are not relative
  -- to a read pointer.
  DeliverBody   :: BlockId -> Body -> Deliver

-- |
-- = Description of state machine for a particular peer.

type Machine s i o = (i, s) -> (o, s)

-- | Includes the chain candidate we announced to this peer, and what we know
-- about theirs.
data PeerState = PeerState
  { -- | We always have a complete chain that we present to the peer.
    -- This also gives us enough information to do fast relay of header while
    -- downloading the body: if this 'ChainCandidate' is 'Incomplete' when we
    -- receive the body for its header, we know to relay it to this peer.
    psOurChain   :: ChainCandidate
    -- | It reads relative to the _peer's_ chain: shorter means theirs is
    -- shorter than ours.
    -- It's Nothing if we haven't yet received their tip.
  , psComparison :: Maybe ChainComparison
  }

-- | Comparison between the peer's chain and the local chain. It can be
-- derived from the announcement message which includes a header and an
-- indication of whether it's complete or missing the body of the tip (fast
-- relay).
data ChainComparison where
  -- | Theirs is shorter; we produce and they consume.
  Shorter    :: Header -> IsComplete -> Cursor -> ChainComparison
  -- | Theirs is longer; we consume and they produce.
  Longer     :: Header -> IsComplete -> Cursor -> ChainComparison
  -- | Equal length, but not necessarily the same.
  Equivalent :: Header -> IsComplete -> ChainComparison

comparisonHeader :: ChainComparison -> Header
comparisonHeader cc = case cc of
  Shorter    h _ _ -> h
  Longer     h _ _ -> h
  Equivalent h _   -> h

comparisonIsComplete :: ChainComparison -> IsComplete
comparisonIsComplete cc = case cc of
  Shorter    _ b _ -> b
  Longer     _ b _ -> b
  Equivalent _ b   -> b

data PeerInput where
  MsgFromPeer   :: Msg -> PeerInput
  ImproveLocalChain :: ChainCandidate -> PeerInput

-- | Messages (possibly none) to send to this peer, and their chain in case
-- we've just finished downloading it.
data PeerOutput = PeerOutput
  { poMsgs  :: [Msg]
  , poChain :: Maybe ChainCandidate
  }

peerStateMachine :: Machine PeerState PeerInput PeerOutput
peerStateMachine (input, peerState) = case input of
  -- Local event: we've got a new best chain.
  -- - If new is worse than old, error.
  -- - If new is equal to old, do nothing.
  -- - If new is better than old:
  --   - If remote is unknown, announce new.
  --   - If new is shorter than remote, announce new.
  --   - If new is equivalent to remote, announce new.
  --   - If new is longer than remote, announce new, and begin refinement.
  --     - If new does not immediately continue remote, send refinement info.
  --     - If new immediately continues remote:
  --       - ...
  -- In any case, the 'poNewChain' is always 'Nothing', because we do not get
  -- a new 'ChainCandidate' from a peer on this input.
  ImproveLocalChain newChain -> case newChain `chainCandidateRelation` oldChain of
    Degradation -> error "new local chain does not improve old chain"
    Same -> (PeerOutput [] Nothing, peerState)
    Improvement header mBody -> case psComparison peerState of
      -- We haven't heard from them yet. Just update the state with what we
      -- announced, and announce it.
      Nothing -> (PeerOutput [msgAnnounce] Nothing, PeerState newChain Nothing)
      Just cc -> case comparisonHeader cc `compareToChainCandidate` newChain of
        -- Remote header is better.
        GT -> case cc of
          -- Their chain was shorter, ours got longer, and now theirs is longer.
          -- Impossible.
          Shorter _ _ _  -> error "remote chain was worse but is now better"
          -- Their chain was equivalent, ours got longer, and now theirs is
          -- longer. Impossible.
          Equivalent _ _ -> error "remote chain was equivalent but is now better"
          -- Their chain was longer, ours got longer, but is still shorter.
          -- Announce it.
          Longer _ _ _ ->
            (PeerOutput [msgAnnounce] Nothing, PeerState newChain (Just cc))
        -- Remote header is equal.
        EQ -> case cc of
          -- Their chain was shorter, ours got longer, and now theirs is
          -- equialent. Impossible.
          Shorter _ _ _ -> error "remote chain was worse but is now equivalent"
          -- Their chain was equivalent, ours got longer, and now theirs
          -- remains equivalent. Impossible, I think...
          -- No, this is the point where we must ship the body for fast
          -- relay!
          Equivalent _ _ -> error "remote chain was equivalent and remains equivalent"
          -- Their chain was longer, ours got longer, and is now equivalent.
          -- We may want to send the body, in case theirs is incomplete and
          -- the same as ours, and we have the body now.
          Longer h isComplete cursor ->
            let comparison = Just (Equivalent h isComplete)
                peerState' = PeerState newChain comparison
                extraMsgs = case mBody of
                  Just body ->
                    if headerHash header == headerHash h &&  not isComplete
                    then [MsgDeliver (DeliverBody (headerHash header) body)]
                    else []
                  Nothing -> []
            in  (PeerOutput (msgAnnounce : extraMsgs) Nothing, peerState')
        -- When the remote chain is worse than the new local chain, all
        -- previous comparisons are compatible with the growth of the local
        -- chain.
        LT -> case cc of
          -- Their chain was shorter, ours got longer, and theirs is still
          -- shorter. No chance we need to send the body here, because it
          -- can't be the case that the new chain immediately continues their
          -- chain: the old chain is a midpoint.
          Shorter _ _ _ ->
            (PeerOutput [msgAnnounce] Nothing, PeerState newChain (Just cc))
          -- Their chain was equivalent, ours got longer, and theirs is now
          -- shorter. It's possible that ours immediately continues theirs.
          Equivalent h isComplete ->
            let comparison = Just (Longer h isComplete cursor)
                cursor = Found h
                peerState' = PeerState newChain comparison
                extraMsgs = case mBody of
                  Just body ->
                    if headerHash header == headerHash h && not isComplete
                    then [MsgDeliver (DeliverBody (headerHash header) body)]
                    else []
                  Nothing -> []
            in  (PeerOutput (msgAnnounce : extraMsgs) Nothing, peerState')
          -- Their chain was longer, ours got longer, and theirs is now
          -- shorter.
          Longer h isComplete cursor ->
            let comparison = Just (Longer h isComplete cursor)
                (extraMsgs, cursor) =
                  -- Special case in which our new header immediately continues
                  -- theirs.
                  if headerParent header == headerHash h
                  then case mBody of
                    Just body ->
                      if isComplete
                      then ([MsgDeliver (DeliverBody (headerHash header) body)], Found h)
                      else ([MsgRefine (Settle (headerHash header) (headerHash h))], Found h)
                    Nothing -> ([MsgRefine (Settle (headerHash header) (headerHash h))], Found h)
                  else ([MsgRefine (Refine (headerHash header) checkpoints)], Refining (Just checkpoints))
                checkpoints = gatherCheckpoints newChain
                peerState' = PeerState newChain comparison
            in  (PeerOutput (msgAnnounce : extraMsgs) Nothing, peerState')

      where

      msgAnnounce = MsgAnnounce $ Announce
        (chainCandidateHeader newChain)
        (chainCandidateIsComplete newChain)

    where

    oldChain = psOurChain peerState

  MsgFromPeer (MsgAnnounce (Announce header isComplete)) ->

    case header `compareToChainCandidate` psOurChain peerState of

      -- Remote header is better. They are the producer.
      -- Nothing to do but wait for them to start the refinement.
      GT ->
        let comparison = Just (Longer header isComplete (Refining Nothing))
            peerState' = peerState { psComparison = comparison }
        in  (PeerOutput [] Nothing, peerState')

      -- Remote header is the same.
      -- It may be that they are incomplete but we have the body they need.
      -- In that case, send it.
      EQ ->
        let comparison = Just (Equivalent header isComplete)
            peerState' = peerState { psComparison = comparison }
            msgs = case psOurChain peerState of
              Complete chain ->
                if headerHash (tipHeader chain) == headerHash header && not isComplete
                then [MsgDeliver (DeliverBody (headerHash (tipHeader chain)) (snd (NE.head chain)))]
                else []
              _ -> []
        in  (PeerOutput msgs Nothing, peerState')

      -- Remote header is worse. We are the producer.
      -- If ours extends it by one and we have the body, we send it.
      LT ->
        let comparison = Just (Shorter header isComplete cursor)
            ourTip = chainCandidateHeader (psOurChain peerState)
            (msgs, cursor) =
              if headerParent ourTip == headerHash header
              then
                if isComplete
                then case psOurChain peerState of
                  Complete chain ->
                    ([MsgDeliver (DeliverBody (headerHash header) (snd (NE.head chain)))], Found header)
                  Incomplete _ _ ->
                    ([MsgRefine (Settle (headerHash header) (headerHash ourTip))], Found header)
                else ([MsgRefine (Settle (headerHash header) (headerHash ourTip))], Found header)
              else ([MsgRefine (Refine (headerHash header) checkpoints)], Refining (Just checkpoints))
            checkpoints = gatherCheckpoints (psOurChain peerState)
            peerState' = peerState { psComparison = comparison }
        in  (PeerOutput msgs Nothing, peerState')
