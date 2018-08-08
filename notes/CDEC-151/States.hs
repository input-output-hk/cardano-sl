{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module States where

import Control.Monad (ap, forM_, unless)
import Data.ByteString (ByteString)
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Semigroup (..))
import Data.Word (Word16)
import qualified Debug.Trace as Debug

data Header = Header
  { headerHash       :: HeaderHash
  , headerParent     :: HeaderHash
  , headerSlot       :: Slot
  , headerBlockCount :: BlockCount
  }
  deriving (Show)
newtype Body = Body
  { getBody :: ByteString
  }
  deriving (Show)
type Block = (Header, Body)
type Chain = NonEmpty Block
type HeaderHash = ByteString
type BlockId = HeaderHash
type Slot = Word
type BlockCount = Word

tipHeader :: Chain -> Header
tipHeader = fst . NE.head

tipBody :: Chain -> Body
tipBody = snd . NE.head

blockCount :: Header -> Word
blockCount = headerBlockCount

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
  deriving (Show)

type IsComplete = Bool

-- | An announcement indicates whether it's of a chain that the sender knows
-- fully ('True') or whether they have it all but the body of the tip ('False').
-- The latter case is needed to support fast relay: in this way a node
-- announces that it intends to send the body as soon as it's known, but also
-- that it would like to be sent that body.
data Announce where
  Announce :: Header -> IsComplete -> Announce
  deriving (Show)

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
  deriving (Show)

type Limit = Word16

data Request where
  -- | Relative to the read pointer, established by the refine phase.
  -- The 'BlockId' identifies the refine phase (identifier of peer's tip).
  RequestHeaders :: BlockId -> Limit -> Request
  -- | Random access using a 'BlockId'.
  -- The 'BlockId' identifying the refine phase (identifier of peer's tip) is
  -- _not_ present because it's not needed, as body requests are not relative
  -- to a read pointer.
  --
  -- This can also come in unsolicited, for fast block relay.
  RequestBody    :: BlockId -> Request
  deriving (Show)

data Deliver where
  -- | The 'BlockId' identifies the refine phase (identifier of peer's tip).
  DeliverHeader :: BlockId -> Header -> Deliver
  -- | The 'BlockId' identifying the refine phase (identifier of peer's tip) is
  -- _not_ present because it's not needed, as body requests are not relative
  -- to a read pointer. Here the 'BlockId' is that of the 'Body's header.
  -- It also encodes "not found" as 'Nothing' for the 'Body'.
  DeliverBody   :: BlockId -> Maybe Body -> Deliver
  deriving (Show)

-- |
-- = Description of state machine for a particular peer.

data Step i o e t where
  Done :: t -> Step i o e t
  Yield :: o -> Step i o e t -> Step i o e t
  -- | A "hole" in the state machine. The GADT 'e' encodes the output type of
  -- the hole.
  Hole :: e x -> (x -> Step i o e t) -> Step i o e t
  -- | Await the next input signal, with an explanation of the current state.
  Await :: Explanation -> K i o e t -> Step i o e t

-- | Includes the possibility of an unexpected input.
type K i o e t = i -> Either Unexpected (Step i o e t)

instance Functor (Step i o e) where
  fmap f term = case term of
    Done t -> Done (f t)
    Yield o next -> Yield o (fmap f next)
    Await ex next -> Await ex ((fmap . fmap) f . next)
    Hole e k -> Hole e (fmap f . k)

instance Applicative (Step i o e) where
  pure = Done
  (<*>) = ap

instance Monad (Step i o e) where
  return = pure
  term >>= k = case term of
    Done t -> k t
    Yield o next -> Yield o (next >>= k)
    Hole e l -> Hole e ((>>= k) . l)
    Await ex next -> Await ex (fmap (>>= k) . next)

type Explanation = String

type Unexpected = String

done :: t -> Step i o e t
done = pure

yield :: o -> Step i o e ()
yield o = Yield o (pure ())

await :: Explanation -> K i o e t -> Step i o e t
await = Await

continue :: Step i o e t -> Either Unexpected (Step i o e t)
continue = Right

unexpected :: Unexpected -> Either Unexpected (Step i o e t)
unexpected = Left

hole :: e t -> Step i o e t
hole e = Hole e pure

-- TBD: how can we make expression of the 'peerStateMachine' slicker with
-- less redundancy in the 'await' cases?
-- We need to be able to use a sub-input-type: eliminate the announce messages
-- for instance, and then be required to handle on the remaining ones.

{-
branch 
  :: (i1 -> Step (Either i1 i2) o e t)
  -> Step i2 o e t
  -> Step (Either i1 i2) o e t
branch l term = case term of
  Done t -> Done t
  Yield o next -> Yield o (branch l next)
  Hole e next -> Hole e (branch l . next)
  Await exp k -> Await exp $ \input -> case input of
    Left i  -> l i
    Right i -> branch l (k i)

imap :: (i1 -> i2) -> Step i2 o e t -> Step i1 o e t
imap f term = case term of
  Done t -> Done t
  Yield o next -> Yield o (imap f next)
  Hole e next -> Hole e (imap f . next)
  Await exp k -> Await exp (imap f . k . f)

omap :: (o1 -> o2) -> Step i o1 e t -> Step i o2 e t
omap f term = case term of
  Done t -> Done t
  Yield o next -> Yield (f o) (omap f next)
  Hole e next -> Hole e (omap f . next)
  Await exp k -> Await exp (omap f . k)
-}

-- Should start with this:

peerToPeer :: ([Either Header Body], [Either Header Body])
peerToPeer = loop (peerA, []) peerB
  where

  peerA = peerStateMachine startA
  peerB = peerStateMachine startB
  startA = (headerC, True)
  startB = (headerB2, True)

  -- Run A until it's blocked, collecting yields. Then run B with the messages
  -- yielded until it's blocked.
  loop (peerA, inboxA) peerB = case fullStep "A" inboxA [] [] peerA of
    (_, outboxA, accA, peerA') -> case fullStep "B" outboxA [] [] peerB of
      (_, outboxB, accB, peerB') -> case outboxB of
        [] -> (accA, accB)
        outboxB ->
          let (accA', accB') = loop (peerA', outboxB) peerB'
          in  (accA ++ accA', accB ++ accB')

  fullStep str inbox outbox acc step = case step of
    Done t -> error $ "done " ++ show t
    Yield (SendToPeer o) next -> Debug.trace (str ++ " send : " ++ show o) (fullStep str inbox (o : outbox) acc next)
    Yield (ReceivedHeader header) next -> fullStep str inbox outbox (Left header : acc) next
    Yield (ReceivedBody headerHash body) next -> fullStep str inbox outbox (Right body : acc) next
    Hole (GetBody headerHash) k -> fullStep str inbox outbox acc (k (getBody headerHash))
    Hole (GetIntersection h c) k -> fullStep str inbox outbox acc (k (getIntersection h c))
    Hole (GetHeadersRange to from range) k -> fullStep str inbox outbox acc (k (getDescendants to from range))
    Await ex k -> case inbox of
      [] -> ([], reverse outbox, reverse acc, step)
      (m : ms) -> Debug.trace (str ++ " : " ++ ex ++ " " ++ show m) $ case k (RemoteInput m) of
        Left unexpected -> Debug.trace (str ++ " : " ++ unexpected) (inbox, reverse outbox, reverse acc, step)
        Right it -> fullStep str ms outbox acc it

  -- Find the earliest element in 'checkpoints' which is an ancestor of
  -- 'header'. Stupid implementation, but fine for small data sets.
  getIntersection header checkpoints = loop (header : ancestry header)
    where
    loop [] = Left (fmap headerHash (take 8 (ancestry header)))
    loop (header : headers) = case find (== (headerHash header)) checkpoints of
      Just _ -> Right header
      _ -> loop headers

  ancestry :: Header -> [Header]
  ancestry header = case mParent of
    Nothing -> []
    Just parent -> parent : ancestry parent
    where
    mParent = Map.lookup (headerParent header) headers

  getBody headerHash = Map.lookup headerHash bodies

  getDescendants :: HeaderHash -> HeaderHash -> Limit -> Maybe [Header]
  getDescendants toH fromH limit = do
    to <- Map.lookup toH headers
    -- Look it up even though we don't use it. If it's not known, we need to
    -- give 'Nothing'.
    _ <- Map.lookup fromH headers
    let ancestors = to : ancestry to
        descendants = takeWhile ((/= fromH) . headerHash) ancestors
    pure . reverse . drop (length descendants - fromIntegral limit) $ descendants

  chainA = [headerC, headerB1, headerA]
  chainB = [headerB2, headerA]

  headers :: Map HeaderHash Header
  headers = Map.fromList
    [ ("A", headerA)
    , ("B1", headerB1)
    , ("B2", headerB2)
    , ("C", headerC)
    ]

  bodies :: Map HeaderHash Body
  bodies = Map.fromList
    [ ("A", Body "A")
    , ("B1", Body "B1")
    , ("B2", Body "B2")
    , ("C", Body "C")
    ]

  headerA = Header
    { headerHash = "A"
    , headerParent = "Genesis"
    , headerSlot = 0
    , headerBlockCount = 1
    }
  headerB1 = Header
    { headerHash = "B1"
    , headerParent = "A"
    , headerSlot = 1
    , headerBlockCount = 2
    }
  headerB2 = Header
    { headerHash = "B2"
    , headerParent = "A"
    , headerSlot = 2
    , headerBlockCount = 2
    }
  headerC = Header
    { headerHash = "C"
    , headerParent = "B1"
    , headerSlot = 3
    , headerBlockCount = 3
    }
  

data PeerInput where
  RemoteInput :: Msg -> PeerInput
  LocalInput  :: (Header, IsComplete) -> PeerInput
  deriving (Show)

data PeerOutput where
  SendToPeer     :: Msg -> PeerOutput
  ReceivedHeader :: Header -> PeerOutput
  ReceivedBody   :: HeaderHash -> Body -> PeerOutput

-- | Abstract definition of supporting functions needed by the peer protocol.
data PeerHole t where
  -- | Resolve a block body from a 'HeaderHash'
  GetBody         :: HeaderHash -> PeerHole (Maybe Body)
  -- | Get headers between two points, excluding the older endpoint but
  -- including the newer one.
  -- The first hash is the newer one, the second is the older one.
  -- At most the 'Limit' are returned.
  -- 'Nothing' means we don't have the first header.
  -- 'Just' gives the headers oldest-to-newest, beginning with the child of
  -- the second header.
  -- TODO should use an 'Either Problem' rather than 'Maybe'. It could be that
  -- one or both of the input hashes are not known.
  GetHeadersRange :: HeaderHash -> HeaderHash -> Limit -> PeerHole (Maybe [Header])
  -- | Locally-known header, remotely-provided checkpoints. Either give the
  -- newest header with hash in the checkpoints which is in the chain ending
  -- at the header, or give a set of checkpoints older than the header.
  GetIntersection :: Header -> Checkpoints -> PeerHole (Either Checkpoints Header)
  GetVerifier     :: Header -> Body -> PeerHole (Maybe Verifier)

-- | Gives a cursor on headers and bodies. The 'Verifier' given in 'Right' of
-- 'verifyHeader' will accept a next valid header, but its 'verifyBody' still
-- expects the body of the header just verified.
-- Used when downloading headers and bodies. The consumer is free to download
-- headers first, and then bodies.
data Verifier = Verifier
  { verifyHeader :: Header -> Either String Verifier
  , verifyBody   :: Body -> Either String Verifier
  }

-- | Reason for an inconsistency in the implementation. Example: getting the
-- body for a header hash that was announced returns 'Nothing'. It means the
-- implementation of a 'PeerHole' is not sound.
type Inconsistent = String

-- | Given a local tip-of-chain 'Header', and whether it's complete or not
-- (whether we have the body of the tip, which we may not in case it was
-- fast-relayed to us), we can start the protocol fresh.
peerStateMachine
  :: (Header, IsComplete)
  -> Step PeerInput PeerOutput PeerHole (Either Inconsistent ())
peerStateMachine (ourHeader, oursIsComplete) = do
  -- Announce it and wait for announcement.
  yield (SendToPeer (MsgAnnounce (Announce ourHeader oursIsComplete)))
  await "initial" $ \input -> case input of
    -- New local chain: start again.
    LocalInput localChain -> continue $ peerStateMachine localChain
    RemoteInput (MsgAnnounce (Announce theirHeader theirsIsComplete)) -> continue $
      announcePhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete)
    _ -> unexpected "new local input or announcement"

-- | Given local and remote 'Header's, carry out the protocol. The same
-- machine can be used on both sides, by flipping the 'Header' arguments.
announcePhase
  :: (Header, IsComplete) -- ^ Ours
  -> (Header, IsComplete) -- ^ Theirs
  -> Step PeerInput PeerOutput PeerHole (Either Inconsistent ())
announcePhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) =
  case ourHeader `compareHeaders` theirHeader of
    -- EQ means local and remote are equivalent, not necessarily the same:
    -- they have the same block count and slot number, but they may be
    -- different. In case they really are the same, and the local one is
    -- incomplete (tip is missing the body) then the body may come in from
    -- this peer. This is the fast relay case, in which we yield ours peer's
    -- header before receiving and verifying the body.
    EQ ->
      if headerHash ourHeader == headerHash theirHeader && not oursIsComplete
      -- In this case, the body may come in (fast relay). That's true even if
      -- theirs is not complete (they will not re-announce when they get the
      -- body).
      -- NB: it's possible that both sides are in this case, but there's no
      -- deadlock. Local input on either one will result in a new announcement
      -- or a delivery of the body.
      then await "equal headers, incomplete" $ \input -> case input of
        LocalInput localChain -> continue $ reannounce localChain
        RemoteInput (MsgAnnounce (Announce theirHeader theirsIsCompletE)) -> continue $
          announcePhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete)
        RemoteInput (MsgDeliver (DeliverBody headerHash (Just body))) -> continue $ do
          yield (ReceivedBody headerHash body)
          -- Now we know they're both complete. No need to re-announce, the
          -- peer can figure out that we're now complete.
          announcePhase (ourHeader, True) (theirHeader, True)
        RemoteInput (MsgDeliver (DeliverBody _ Nothing)) -> continue $
          announcePhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete)
        _ -> unexpected "new local input, announcement, or body"
      else if headerHash ourHeader == headerHash theirHeader && oursIsComplete && not theirsIsComplete
      -- We have the completion of their chain. Send the body right away.
      then do
        -- TBD: what to do in case we can't find the body?
        -- That's a bug. If we announce the header and say it's complete, we
        -- need to have the body.
        mBody <- hole (GetBody (headerHash ourHeader))
        case mBody of
          Nothing -> done $ Left ("body not found for " ++ show (headerHash ourHeader))
          Just body -> do
            yield (SendToPeer (MsgDeliver (DeliverBody (headerHash ourHeader) (Just body))))
            -- Can go back to the main phase knowing their chain is complete. They
            -- will not announce it back to us.
            announcePhase (ourHeader, True) (theirHeader, True)
      else await "equivalent headers" $ \input -> case input of
        -- If it's a new local chain, we must re-announce.
        LocalInput localChain -> continue $ reannounce localChain
        -- If it's a new remote chain, we don't announce again.
        RemoteInput (MsgAnnounce (Announce theirHeader theirsIsComplete)) -> continue $
          announcePhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete)
        _ -> unexpected "new local input or announcement"

    -- Our header is shorter. Become the consumer.
    LT -> consumer (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete)

    -- Our header is longer. Become the producer.
    GT -> producer (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete)

  where

  -- Re-announce and continue the main phase.
  reannounce (ourHeader, oursIsComplete) = do
    yield (SendToPeer (MsgAnnounce (Announce ourHeader oursIsComplete)))
    announcePhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete)

  restart (theirHeader, theirsIsComplete) =
    announcePhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete)

  producer (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) = do
    -- If ours immediately continues theirs, and theirs is complete, we
    -- should send the body as soon as we have it.
    if headerParent ourHeader == headerHash theirHeader && theirsIsComplete
    then
      if not oursIsComplete
      -- Nothing we can do until we get a new chain.
      then await "producer, incomplete, waiting for body" $ \input -> case input of
        -- Special case if it's the completion: we don't need to re-announce.
        -- Going back to 'announcePhase' is enough. It will send the body.
        LocalInput (newHeader, isComplete) -> continue $
          if headerHash newHeader == headerHash ourHeader && isComplete
          then announcePhase (ourHeader, True) (theirHeader, theirsIsComplete)
          else reannounce (newHeader, isComplete)
        RemoteInput (MsgAnnounce (Announce theirHeader theirsIsComplete)) -> continue $
          announcePhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete)
        _ -> unexpected "producer, local input or announcement"
      else do
        -- We have the body. Grab it and send it.
        mBody <- hole (GetBody (headerHash ourHeader))
        case mBody of
          Nothing -> done $ Left ("body not found for " ++ show (headerHash ourHeader))
          Just body -> do
            yield (SendToPeer (MsgDeliver (DeliverBody (headerHash ourHeader) (Just body))))
            -- We go back to the main phase knowing their chain is complete.
            -- They will not announce it back to us.
            announcePhase (ourHeader, True) (ourHeader, True)
    else
      if headerParent ourHeader == headerHash theirHeader && not theirsIsComplete
      -- In this case we don't immediately relay the body, but we do know the
      -- intersection point. We can settle on it and then wait for them to
      -- request headers.
      then settle (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) (headerHash theirHeader) uploadPhase
      -- TODO clean up the if/else. In this case, though it may not be clear,
      -- our header is greater than theirs (we're producer) but does not
      -- immediately continue it (the if/else excluded that).
      -- Try to figure out the intersection point (refine phase).
      else refinePhaseActive (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) [headerHash ourHeader] uploadPhase

  -- The consumer awaits instruction from the producer.
  -- If theirs immediately extends ours, we yield an incomplete new chain and
  -- expect the body.
  -- Otherwise, we try to get the whole chain before yielding it.
  consumer (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) = do
    if headerParent theirHeader == headerHash ourHeader
    then earlyYield theirHeader
    -- FIXME reorganize so that we don't have to give the empty checkpoint
    -- list here.
    else refinePhasePassive (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) [] downloadPhase

  -- Yield a header before getting its body.
  earlyYield theirHeader = do
    yield (ReceivedHeader theirHeader)
    announcePhase (theirHeader, False) (theirHeader, theirsIsComplete)

  -- Refine phase
  --
  -- The peer with the longer chain starts in the active phase, by sending
  -- either the intersection point (settle) or some checkpoints (refine).
  -- In the latter case, the roles reverse and the peer with the shorter
  -- chain is now active. It must either settle on one of the checkpoints
  -- in its own chain (possibly older than the true intersection) or respond
  -- with more checkpoints.
  --
  -- Checkpoints are an ordered list of header hashes by slot number. They
  -- must be chosen in such a way that, if the peers' chains intersect at all,
  -- then one of the checkpoints will always be in the peer's chain. The first
  -- set, then, must have the genesis hash as the oldest. If the peer finds a
  -- newer hash in the checkpoint set, it can send back a set of checkpoints
  -- with that hash as the oldest, so that the peer may find a newer one (it
  -- would be in-between the two oldest checkpoints).

  -- Active role in intersection refinement: we send the checkpoints.
  -- Other side is in 'refinePhasePassive'.
  refinePhaseActive (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) checkpoints k = do
    checkpointsOrIntersection <- hole (GetIntersection theirHeader checkpoints)
    case checkpointsOrIntersection of
      Left checkpoints' -> do
        yield (SendToPeer (MsgRefine (Refine (headerHash theirHeader) checkpoints')))
        refinePhasePassive (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) checkpoints' k
      Right intersection ->
        settle (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) (headerHash intersection) k

  -- Passive role in intersection refinement: wait for the other end and
  -- become active if necessary.
  -- Other side is in 'refinePhaseActive'.
  refinePhasePassive (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) checkpoints k =
    await "passive refinement" $ \input -> case input of
      LocalInput (newHeader, isComplete) -> continue $
        reannounce (newHeader, isComplete)
      RemoteInput (MsgAnnounce (Announce theirHeader theirsIsComplete)) -> continue $
        announcePhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete)
      -- Peer settled on an intersection point.
      RemoteInput (MsgRefine (Settle hash intersection)) -> continue $
        if hash == headerHash ourHeader
        -- All refine messages are tagged with the header that the peer thinks
        -- it's relevant for. In case we had announced a new header and then
        -- entered this phase, we want to ignore any in-flight messages for
        -- the previous header announcement.
        then k (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) intersection
        else refinePhasePassive (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) checkpoints k
      -- Peer couldn't find the intersection, gave checkpoints.
      RemoteInput (MsgRefine (Refine hash checkpoints)) -> continue $
        if hash == headerHash ourHeader
        then refinePhaseActive (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) checkpoints k
        -- Old message for a different remote tip.
        else refinePhasePassive (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) checkpoints k
      _ -> unexpected "passive refinement"

  -- Finish the refine phase and continue according to 'k'.
  settle
    :: (Header, IsComplete)
    -> (Header, IsComplete)
    -> HeaderHash
    -> ((Header, IsComplete) -> (Header, IsComplete) -> HeaderHash -> Step PeerInput PeerOutput PeerHole t)
    -> Step PeerInput PeerOutput PeerHole t
  settle (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) point k = do
    yield (SendToPeer (MsgRefine (Settle (headerHash theirHeader) point)))
    k (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) point

  -- Upload / download phase
  --
  -- After refinement, both ends have agreed on a point from which to start
  -- downloading, and the consumer is able to verify them. The peer with the
  -- longer chain is the uploader, the other is
  -- the downloader. Downloader can request headers in order from the
  -- point agreed upon in refinement, and may request bodies randomly by
  -- header hash.

  -- Producer delivers headers and bodies.
  uploadPhase
    :: (Header, IsComplete)
    -> (Header, IsComplete)
    -> HeaderHash
    -> Step PeerInput PeerOutput PeerHole (Either Inconsistent ())
  uploadPhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) cursor =
    await "upload phase" $ \input -> case input of
      RemoteInput (MsgRequest (RequestHeaders hash limit)) -> continue $
        if hash == headerHash ourHeader
        then
          if limit == 0 || headerHash ourHeader == cursor
          -- Client knows it did this. No need to tell it.
          then uploadPhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) cursor
          else do
            mHeaders <- hole (GetHeadersRange (headerHash ourHeader) cursor limit)
            case mHeaders of
              -- One of the hashes could not be resolved.
              -- That's an internal inconsistency, since we agreed upon the
              -- cursor and chose our header.
              Nothing -> done $ Left "failed to get header range"
              -- Empty range, even though the limit is nonzero and our
              -- header is not the same as the cursor. It's an inconsitency
              -- in the implementation of 'GetHeadersRange'.
              Just [] -> done . Left $ "no headers in range " ++ " " ++ show (headerHash ourHeader) ++ " " ++ show cursor
              Just (h : hs) -> do
                forM_ (h : hs) $ \header -> yield $
                  SendToPeer (MsgDeliver (DeliverHeader (headerHash theirHeader) header))
                let newCursor = NE.last (h NE.:| hs)
                uploadPhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) (headerHash newCursor)
        else uploadPhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) cursor
      RemoteInput (MsgRequest (RequestBody hash)) -> continue $ do
        mBody <- hole (GetBody hash)
        yield (SendToPeer (MsgDeliver (DeliverBody hash mBody)))
        -- The cursor does not change.
        uploadPhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) cursor
      -- TODO we don't actually need to restart in all cases, and in fact we
      -- shouldn't because it will disrupt the download.
      RemoteInput (MsgAnnounce (Announce theirHeader theirsIsComplete)) -> continue $
        restart (theirHeader, theirsIsComplete)
      LocalInput (ourHeader, oursIsComplete) -> continue $
        reannounce (ourHeader, oursIsComplete)
      _ -> unexpected "upload phase"

  -- Consumer downloads headers and then bodies.
  -- TBD: should we interleave downloading headers and their bodies?
  -- TBD: does the downloader even need to know the cursor?
  -- TBD: tracking of requested data?
  downloadPhase
    :: (Header, IsComplete)
    -> (Header, IsComplete)
    -> HeaderHash
    -> Step PeerInput PeerOutput PeerHole (Either Inconsistent ())
  downloadPhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) cursor = do
    let limit = 8
    yield (SendToPeer (MsgRequest (RequestHeaders (headerHash theirHeader) limit)))
    let downloadLoop n = await "download phase" $ \input -> case input of
          RemoteInput (MsgDeliver (DeliverHeader hash header)) -> continue $
            if hash /= headerHash ourHeader
            then downloadLoop n
            else do
              yield (ReceivedHeader header)
              let shouldStop = headerHash header == headerHash theirHeader || (n - 1) == 0
              if shouldStop
              then pure (headerHash header)
              else downloadLoop (n-1)
          _ -> unexpected "download phase"
    cursor' <- downloadLoop limit
    if cursor' == headerHash theirHeader
    -- TODO must download the bodies next, then announce.
    then reannounce (theirHeader, False)
    else downloadPhase (ourHeader, oursIsComplete) (theirHeader, theirsIsComplete) cursor'

-- TODO in refine, upload, or download phase, if we get a local chain
-- announcement and it completes or extends our chain, we don't need to go
-- back to announce.
--
-- This can be a milestone after the initial version.
