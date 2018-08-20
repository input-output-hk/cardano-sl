{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Protocol where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (throwIO)
import Control.Monad (ap, forM_, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (find)
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Semigroup (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word16)
import qualified Debug.Trace as Debug
import Data.Void
import Data.Kind (Type)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.IO.Error (userError)

-- A basic blockchain model for simulation.

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

type Transition local remote = Type -> local -> remote -> local -> remote -> Type

-- | Chain consumer states (used as a kind).
data StChainConsumer where
  StChainConsumerInit  :: StChainConsumer
  StChainConsumerAsked :: StChainConsumer
  StChainConsumerGotTip :: StChainConsumer
  StChainConsumerGotRead :: StChainConsumer
  StChainConsumerImprove :: StChainConsumer
  StChainConsumerDownload :: StChainConsumer

-- | Chain producer states (used as a kind).
data StChainProducer where
  StChainProducerInit  :: StChainProducer
  StChainProducerGaveTip :: StChainProducer
  StChainProducerGaveRead :: StChainProducer
  StChainProducerUpload :: StChainProducer

-- TODO: need transitions to enable
--
--   - Producer to extend or fork after read pointer is established (always
--     in response to an ask or a download).
--   - Consumer to try to improve the read pointer.
--   - Consumer to ask for a tip update without downloading.

data TrChainConsumer t local remote localNext remoteNext where

  -- First, the consumer asks for the producer's tip.
  TrChainConsumerInit
    :: TrChainConsumer ()
                       -- Consumer and producer must be in 'Init'.
                       'StChainConsumerInit 'StChainProducerInit
                       -- Consumer transition to 'Asked', producer remains in
                       -- 'Init'.
                       'StChainConsumerAsked 'StChainProducerInit

  -- With the producer's tip, the two must establish a proper "read pointer":
  -- a header known to the consumer which is in the producer's chain.
  -- This is done by repeatedly asking whether a given header is in the
  -- producer's chain (the tip of which was given earlier).
  TrChainConsumerAsk
    :: TrChainConsumer Header
                       -- Initial state: consumer got tip, producer gave tip.
                       'StChainConsumerGotTip 'StChainProducerGaveTip
                       -- Terminal state: consumer asked, producer gave tip.
                       'StChainConsumerAsked 'StChainProducerGaveTip

  -- Try to improve the read pointer to this header. The producer decides
  -- whether it moves.
  TrChainConsumerImprove
    :: TrChainConsumer Header
                       'StChainConsumerGotRead 'StChainProducerGaveRead
                       'StChainConsumerImprove 'StChainProducerGaveRead

  -- Consumer can request to download a certain number of blocks from the
  -- read pointer (it's in StChainConsumerGotRead).
  TrChainConsumerDownload
    :: TrChainConsumer Word16
                       -- Initial state: consumer got read pointer, producer
                       -- gave it.
                       'StChainConsumerGotRead 'StChainProducerGaveRead
                       -- Terminal state: consumer downloading, producer
                       -- uploading.
                       'StChainConsumerDownload 'StChainProducerUpload

data TrChainProducer t local from localNext remoteNext where

  -- The producer gives the consumer the header of its best chain.
  TrChainProducerInit
    :: TrChainProducer Header
                       'StChainProducerInit 'StChainConsumerAsked
                       'StChainProducerGaveTip 'StChainConsumerGotTip

  -- Consumer asks producer whether a header is in the chain at its best tip.
  -- It can say no or yes, or reset by giving a different tip.
  TrChainProducerInitNo
    :: TrChainProducer ()
                       'StChainProducerGaveTip 'StChainConsumerAsked
                       'StChainProducerGaveTip 'StChainConsumerGotTip
  TrChainProducerInitYes
    :: TrChainProducer ()
                       'StChainProducerGaveTip 'StChainConsumerAsked
                       'StChainProducerGaveRead 'StChainConsumerGotRead
  TrChainProducerImproveNo
    :: TrChainProducer ()
                       'StChainProducerGaveRead 'StChainConsumerImprove
                       'StChainProducerGaveRead 'StChainConsumerGotRead
  TrChainProducerImproveYes
    :: TrChainProducer ()
                       'StChainProducerGaveRead 'StChainConsumerImprove
                       'StChainProducerGaveRead 'StChainConsumerGotRead
  -- The producer can send a new header in the midst of intersection point
  -- negotiation, but only in response to the consumer asking.
  TrChainProducerReset
    :: TrChainProducer Header
                       'StChainProducerGaveTip 'StChainConsumerAsked
                       'StChainProducerGaveTip 'StChainConsumerGotTip

  TrChainProducerUpload
    :: TrChainProducer (Header, Body)
                       'StChainProducerUpload 'StChainConsumerDownload
                       'StChainProducerUpload 'StChainConsumerDownload
  TrChainProducerUploadDone
    :: TrChainProducer ()
                       'StChainProducerUpload 'StChainConsumerDownload
                       'StChainProducerGaveRead 'StChainConsumerGotRead

-- | TBD rename to Linear? Sequential?
-- This means it's impossible for both sides to be waiting on a transition from
-- the other. NB this does not mean no deadlock, for deadlock can arise if
-- there are no transitions from some transition.
--
-- TBD can we quickcheck that 2 folds never deadlock?
-- Deadlock is when they're both in 'B' (awaiting). This can't be ruled out
-- statically for non-Exclusive transitions, since the choice of 'B' can depend
-- on values.
-- If a fold is constructed applicatively, then we can tell.
-- Maybe we could give a way to construct a pair of folds in such a way that
-- they never deadlock?
type Exclusive (trA :: Transition stA stB) (trB :: Transition stB stA) =
  forall s t atA atB toA toB toA' toB' . trA s atA atB toA toB -> trB t atB atA toB' toA' -> Void

exclusiveCommutes :: Exclusive trA trB -> Exclusive trB trA
exclusiveCommutes = flip

exclusiveChainExchange :: Exclusive TrChainConsumer TrChainProducer
exclusiveChainExchange trC trP = case trC of
  TrChainConsumerInit -> case trP of
    { }
  TrChainConsumerAsk -> case trP of
    { }
  TrChainConsumerImprove -> case trP of
    { }
  TrChainConsumerDownload -> case trP of
    { }

-- | For any transition of either kind, there's always a transition of either
-- kind from its terminus (we never get stuck). Even if 'Progress' holds, there
-- can still be deadlock for a particular pair of 'Fold's of the protocol.
data Progress (trA :: Transition stA stB) (trB :: Transition stB stA) = Progress
  { progressA :: forall t atA atB toA toB . trA t atA atB toA toB -> TransitionExistsFrom trA trB toA toB
  , progressB :: forall t atA atB toA toB . trB t atB atA toB toA -> TransitionExistsFrom trA trB toA toB
  }

data TransitionExistsFrom1 (tr :: Transition stA stB) (fromA :: stA) (fromB :: stB) where
  TransitionExists :: tr t fromA fromB toA toB -> TransitionExistsFrom1 tr fromA fromB

data TransitionExistsFrom (trA :: Transition stA stB) (trB :: Transition stB stA) (atA :: stA) (atB :: stB) where
  TransitionExistsA :: trA t atA atB toA toB -> TransitionExistsFrom trA trB atA atB
  TransitionExistsB :: trB t atB atA toB toA -> TransitionExistsFrom trA trB atA atB

-- | Deadlock := not Progress
type Deadlock trA trB = Progress trA trB -> Void

-- Progress tells us that from any transition there is always _some_ way to
-- progress, if the transitions are applied linearly. It doesn't deal with
-- the case in which there are concurrent transitions (the transition kinds are
-- not exclusive).

-- | Concurrent transitions are those which have the same local and remote types,
-- in swapped places.
data Concurrent (trA :: Transition stA stB) (trB :: Transition stB stA) where
  Concurrent :: trA s atA atB toA toB -> trB t atB atA toB' toA' -> Concurrent trA trB

data TransitionSequence (trA :: Transition stA stB) (fromA :: stA) (fromB :: stB) (toA :: stA) (toB :: stB) where
  TransitionSequence1
    :: trA t fromA fromB toA toB
    -> TransitionSequence trA fromA fromB toA toB
  TransitionSequenceN
    :: trA t fromA fromB interA interB
    -> TransitionSequence trA interA interB toA toB
    -> TransitionSequence trA fromA fromB toA toB

-- | Two transition sequences which could be done concurrently.
data TransitionConcurrence (trA :: Transition stA stB)
                           (trB :: Transition stB stA)
                           (fromA :: stA)
                           (fromB :: stB)
                           (toA :: stA)
                           (toB :: stB)
                           (toA' :: stA)
                           (toB' :: stB)
                           where
  TransitionConcurrence
    :: TransitionSequence trA fromA fromB toA toB
    -> TransitionSequence trB fromB fromA toB' toA'
    -> TransitionConcurrence trA trB fromA fromB toA toB toA' toB'

exclusiveNoConcurrence
  :: Exclusive trA trB
  -> TransitionConcurrence trA trB fromA fromB toA toB toA' toB'
  -> Void
exclusiveNoConcurrence excl (TransitionConcurrence seqA seqB) = case seqA of
  TransitionSequence1 trA -> case seqB of
    TransitionSequence1 trB -> excl trA trB
    TransitionSequenceN trB _ -> excl trA trB
  TransitionSequenceN trA _ -> case seqB of
    TransitionSequence1 trB -> excl trA trB
    TransitionSequenceN trB _ -> excl trA trB

-- | For all concurrent transitions, there's a next transition.
-- Not sure if this will be useful.
type StrongProgress (trA :: Transition stA stB) (trB :: Transition stB stA) =
  forall fromA fromB toA toB toA' toB' .
       TransitionConcurrence trA trB fromA fromB toA toB toA' toB'
    -> Either (TransitionExistsFrom1 trA toA toB) (TransitionExistsFrom1 trB toB' toA')

-- | Weak (linear) progress is strong progress on an exclusive system, since
-- there is no possibility of concurrence here.
exclusiveProgress
  :: Exclusive trA trB
  -> Progress trA trB
  -> StrongProgress trA trB
exclusiveProgress excl weakP = absurd . exclusiveNoConcurrence excl

-- Assuming some source of A and B transitions, a 'Fold' allows us to carry out
-- an application of the protocol in 'm'.
--
-- You can either
-- 1. Give a local transition and a continuation
-- 2. Produce a continuation from a remote transition
-- 3. Do effects
-- 4. Finish, if the type is right.
--
-- TODO: it would be ideal to define both sides simultaneously, in such a way
-- that one side is guaranteed to expect what the other sends.
-- It's not so simple, though, because we need each side to be separable from
-- the other; the structure of sends and receives needs to be statically
-- known, rather than computed in some monad.

data Fold (trA :: Transition stA stB)
          (trB :: Transition stB stA)
          (fromA :: stA)
          (fromB :: stB)
          (toA :: stA)
          (toB :: stB)
          (m :: Type -> Type)
          (r :: Type)
          where

  A :: trA t fromA fromB nextA nextB
    -> t
    -> Fold trA trB nextA nextB toA toB m r
    -> Fold trA trB fromA fromB toA toB m r

  B :: (forall t nextB nextA . trB t fromB fromA nextB nextA -> t -> Fold trA trB nextA nextB toA toB m r)
    -> Fold trA trB fromA fromB toA toB m r

  E :: m x
    -> (x -> Fold trA trB fromA fromB toA toB m r)
    -> Fold trA trB fromA fromB toA toB m r

  D :: Finished trA trB fromA fromB toA toB
    -> r
    -> Fold trA trB fromA fromB toA toB m r

data Finished (trA :: Transition stA stB)
              (trB :: Transition stB stA)
              (fromA :: stA)
              (fromB :: stB)
              (toA :: stA)
              (toB :: stB)
              where

  FinishedA :: trA t fromA fromB toA toB
            -> Finished trA trB fromA fromB toA toB

  FinishedB :: trB t fromB fromA toB toA
            -> Finished trA trB fromA fromB toA toB

-- In order to use a 'Fold' in separate threads or on separate machines, the
-- transitions need to transit through some common type.

data FromMsg (tr :: Transition stA stB) (atA :: stA) (atB :: stB) where
  Unexpected :: String -> FromMsg tr atA atB
  Expected   :: tr t atA atB toA toB -> t -> FromMsg tr atA atB

-- | Must obey:
--
--     fromMsg (toMsg tr t) = Expected tr t
--
-- That's also to say that the terminal types, toA and toB, are the same in
-- the parse result.
data Roundtrip (tr :: Transition stA stB) (atA :: stA) (atB :: stB) (msg :: Type) = Roundtrip
  { toMsg   :: forall t toA toB . tr t atA atB toA toB -> t -> msg
  , fromMsg :: msg -> FromMsg tr atA atB
  }

data Roundtrips (trA :: Transition stA stB) (trB :: Transition stB stA) (msg :: Type) = Roundtrips
  { nextRoundtripA
      :: forall t atA atB toA toB .
         trA t atA atB toA toB
      -> (Roundtrip trA toA toB msg, Roundtrip trB toB toA msg)
  , nextRoundtripB
      :: forall t atA atB toA toB .
         trB t atB atA toB toA
      -> (Roundtrip trB toB toA msg, Roundtrip trA toA toB msg)
  }

roundtripsCommute
  :: Roundtrips trA trB msg
  -> Roundtrips trB trA msg
roundtripsCommute rts = Roundtrips
  { nextRoundtripA = nextRoundtripB rts
  , nextRoundtripB = nextRoundtripA rts
  }

-- TODO
-- For non-exclusive/non-linear transition kinds, the receiver of a remote
-- transition may try to parse it against a different local state from the
-- one which the sender expected, because the receiver made a transition
-- concurrently with the sender.
-- In order for 'Roundtrip's to work, they must obey further laws.
-- We can work that out later. The demo example is linear and so won't
-- be affected by this.

-- | For non-IO simulation, this type helps to represent the current
-- continuation of a 'Fold'.
data Next (trA :: Transition stA stB) (trB :: Transition stB stA) (toA :: stA) (toB :: stB) msg m a where
  Complete
    :: [msg] -- ^ Inbox (unconsumed)
    -> [msg] -- ^ Outbox
    -> Finished trA trB fromA fromB toA toB
    -> a
    -> Next trA trB toA toB msg m a
  Incomplete
    :: [msg] -- ^ Outbox
    -> ([msg] -> ExceptT String m (Next trA trB toA toB msg m a)) -- ^ Continuation with new inbox.
    -> Next trA trB toA toB msg m a

runFold
  :: forall trA trB fromA fromB toA toB msg m a .
     ( Monad m )
  => Roundtrips trA trB msg
  -> Roundtrip trA fromA fromB msg
  -> Roundtrip trB fromB fromA msg
  -> [msg]
  -> [msg]
  -> Fold trA trB fromA fromB toA toB m a
  -> ExceptT String m (Next trA trB toA toB msg m a)
runFold rts rtA rtB inbox outbox fold = case fold of
  D fin a -> pure (Complete inbox outbox fin a)
  E act k -> do
    x <- lift act
    runFold rts rtA rtB inbox outbox (k x)
  A (tr :: trA t fromA fromB nextA nextB) t next -> do
    let msg = toMsg rtA tr t
    runFold rts (fst (nextRoundtripA rts tr)) (snd (nextRoundtripA rts tr)) inbox (msg : outbox) next
  B k -> case inbox of
    -- If the inbox is empty, give an 'Incomplete' which runs it again with
    -- the new inbox.
    -- Otherwise carry on, draining the inbox.
    [] -> pure $ Incomplete (reverse outbox) $ \inbox' ->
      runFold rts rtA rtB inbox' [] fold
    (msg : msgs) -> case fromMsg rtB msg of
      Unexpected str -> throwE str
      Expected (tr :: trB t fromB fromA nextB nextA) t ->
        runFold rts (snd (nextRoundtripB rts tr)) (fst (nextRoundtripB rts tr)) msgs outbox (k tr t)

runFolds
  :: ( Monad m )
  => Roundtrips trA trB msg
  -> Roundtrip trA fromA fromB msg
  -> Roundtrip trB fromB fromA msg
  -> Fold trA trB fromA fromB toA toB m a
  -> Fold trB trA fromB fromA toB toA m b
  -> ExceptT String m (a, b)
runFolds rts rtA rtB foldA foldB =
  let nextA = Incomplete [] (\inbox -> runFold rts rtA rtB inbox [] foldA)
      nextB = Incomplete [] (\inbox -> runFold (roundtripsCommute rts) rtB rtA inbox [] foldB)
  in  roundrobin nextA nextB

-- Problem: loops if there is starvation.
-- Won't happen in a transition system which doesn't have concurrent
-- transitions (see 'type Exclusive').
roundrobin
  :: ( Monad m )
  => Next trA trB toA toB msg m a
  -> Next trB trA toB toA msg m b
  -> ExceptT String m (a, b)
roundrobin nextA nextB = case nextA of
  Complete inbox outbox finished a -> case nextB of
    Complete _ _ _ b -> pure (a, b)
    Incomplete _ kB -> do
      nextB <- kB outbox
      roundrobin (Complete inbox [] finished a) nextB
  Incomplete outboxA kA -> case nextB of
    Complete inbox outbox finished b -> do
      nextA <- kA outbox
      roundrobin nextA (Complete inbox [] finished b)
    Incomplete outboxB kB -> do
      nextA <- kA outboxB
      nextB <- kB outboxA
      roundrobin nextA nextB

-- Next step: show how a single 'Fold' can be run given a source and sink of
-- messages. Then use this to tie up 2 threads running opposite ends of the
-- system.

runFoldSourceSink
  :: ( Monad m )
  => Roundtrips trA trB msg
  -> Roundtrip trA fromA fromB msg
  -> Roundtrip trB fromB fromA msg
  -> (msg -> m ())
  -> m msg
  -> Fold trA trB fromA fromB toA toB m a
  -> m (Either String a)
runFoldSourceSink rts rtA rtB send recv fold = case fold of
  D fin a -> pure (Right a)
  E eff k -> do
    x <- eff
    runFoldSourceSink rts rtA rtB send recv (k x)
  A tr t next -> do
    let msg = toMsg rtA tr t
    send msg
    runFoldSourceSink rts (fst (nextRoundtripA rts tr)) (snd (nextRoundtripA rts tr)) send recv next
  B k -> do
    msg <- recv
    case fromMsg rtB msg of
      Unexpected str -> pure (Left str)
      Expected tr t ->
        runFoldSourceSink rts (snd (nextRoundtripB rts tr)) (fst (nextRoundtripB rts tr)) send recv (k tr t)

-- | A chain consumer application in IO which never ends (toA, toB, x are
-- universally quantified, meaning 'Finished' and the 'D' constructor are
-- impossible).
--
-- It uses stdin/stdio IOs to get headers for read pointer negotiation, to
-- determine the number of blocks to download, and to show downloaded block
-- header hashes.
demoConsumer
  :: Fold TrChainConsumer TrChainProducer 'StChainConsumerInit 'StChainProducerInit toA toB IO x
demoConsumer = A TrChainConsumerInit () $
  B $ \tr t -> case tr of
    TrChainProducerInit -> establish t

  where
  establish
    :: Header
    -> Fold TrChainConsumer TrChainProducer 'StChainConsumerGotTip 'StChainProducerGaveTip toA toB IO x
  establish theirHeader = E (askForLocalHeader "consumer") $ \ourHeader ->
    A TrChainConsumerAsk ourHeader $ 
      B $ \tr t -> case tr of
        TrChainProducerReset -> E (putStrLn "> consumer: reset") $ \_ ->
          let theirHeader' = t
          in  establish t
        TrChainProducerInitNo -> E (putStrLn "> consumer: miss") $ \_ -> establish theirHeader
        TrChainProducerInitYes -> E (putStrLn "> consumer: hit") $ \_ -> download theirHeader ourHeader

  download
    :: Header -> Header
    -> Fold TrChainConsumer TrChainProducer 'StChainConsumerGotRead 'StChainProducerGaveRead toA toB IO x
  download theirHeader ourHeader = E (askForDownloadNumber theirHeader ourHeader) $ \w ->
    A TrChainConsumerDownload w (downloadLoop theirHeader ourHeader)

  downloadLoop
    :: Header -> Header
    -> Fold TrChainConsumer TrChainProducer 'StChainConsumerDownload 'StChainProducerUpload toA toB IO x
  downloadLoop theirHeader ourHeader = B $ \tr t -> case tr of
    TrChainProducerUpload -> E (downloadedBlock t) $ \_ -> downloadLoop theirHeader ourHeader
    TrChainProducerUploadDone ->
      E (putStrLn "> consumer: download finished") $ \_ -> download theirHeader ourHeader

  askForDownloadNumber :: Header -> Header -> IO Word16
  askForDownloadNumber theirHeader ourHeader = do
    putStrLn "> consumer: how many to download?"
    n <- getLine
    case reads n of
      [(w, [])] -> pure w
      _ -> do
        putStrLn "> consumer: not a number"
        askForDownloadNumber theirHeader ourHeader

  downloadedBlock :: (Header, Body) -> IO ()
  downloadedBlock (h, b) =
    let msg = "downloaded block with hash " ++ show (headerHash h)
    in  putStrLn ("> consumer: " ++ msg)

-- | Complements the 'demoConsumer'.
demoProducer
  :: Fold TrChainProducer TrChainConsumer 'StChainProducerInit 'StChainConsumerInit toA toB IO x
demoProducer = B $ \tr t -> case tr of
  TrChainConsumerInit -> E (askForLocalHeader "producer") $ \ourHeader ->
    A TrChainProducerInit ourHeader establish

  where
  establish
    :: Fold TrChainProducer TrChainConsumer 'StChainProducerGaveTip 'StChainConsumerGotTip toA toB IO x
  establish = B $ \tr t -> case tr of
    TrChainConsumerAsk ->
      let theirHeader = t
      in  if headerHash t == "ffffff"
          then A TrChainProducerInitYes () (upload t)
          else A TrChainProducerInitNo () establish

  upload
    :: Header
    -> Fold TrChainProducer TrChainConsumer 'StChainProducerGaveRead 'StChainConsumerGotRead toA toB IO x
  upload headerFrom = B $ \tr t -> case tr of
    TrChainConsumerImprove -> improve headerFrom t
    TrChainConsumerDownload -> uploadLoop headerFrom t

  -- Always say yes...
  improve
    :: Header -> Header
    -> Fold TrChainProducer TrChainConsumer 'StChainProducerGaveRead 'StChainConsumerImprove toA toB IO x
  improve from new = A TrChainProducerImproveYes () (upload new)

  uploadLoop
    :: Header
    -> Word16
    -> Fold TrChainProducer TrChainConsumer 'StChainProducerUpload 'StChainConsumerDownload toA toB IO x
  uploadLoop headerFrom n =
    if n <= 0
    then A TrChainProducerUploadDone () (upload headerFrom)
    else E (getNextBlock headerFrom) $ \(nextHeader, body) ->
           A TrChainProducerUpload (nextHeader, body) (uploadLoop nextHeader (n-1))

  getNextBlock :: Header -> IO (Header, Body)
  getNextBlock h = do
    putStr "< producer: please input a hash: "
    hash <- BS.getLine
    putStr "< producer: please input a parent hash: "
    parent <- BS.getLine
    let header = Header { headerHash = hash, headerParent = parent, headerSlot = 0, headerBlockCount = 0 }
        body = Body ""
    pure (header, body)

askForLocalHeader :: String -> IO Header
askForLocalHeader str = do
  putStr $ "Input header hash for " ++ str ++ ": "
  str <- BS.getLine
  pure $ Header { headerHash = str, headerParent = "", headerSlot = 0, headerBlockCount = 0 }

data Msg where
  MsgInit :: Msg
  -- | Consumer gives its header.
  MsgHeader :: Header -> Msg
  -- | Consumer asks the producer whether it can read from this header.
  MsgAsk :: Header -> Msg
  MsgYes :: Msg
  MsgNo  :: Msg
  MsgDownload :: Word16 -> Msg
  MsgUpload :: Header -> Body -> Msg
  MsgUploadDone :: Msg

roundtripConsumerInit :: Roundtrip TrChainConsumer 'StChainConsumerInit 'StChainProducerInit Msg
roundtripConsumerInit = Roundtrip
  { toMsg = \tr t -> case tr of
      TrChainConsumerInit -> MsgInit
  , fromMsg = \msg -> case msg of
      MsgInit -> Expected TrChainConsumerInit ()
      _ -> Unexpected "consumer init"
  }

roundtripProducerInit :: Roundtrip TrChainProducer 'StChainProducerInit 'StChainConsumerInit Msg
roundtripProducerInit = Roundtrip
  { toMsg = \tr t -> case tr of { }
  , fromMsg = \_ -> Unexpected "producer init"
  }

roundtripConsumerEstablish :: Roundtrip TrChainConsumer 'StChainConsumerAsked 'StChainProducerInit Msg
roundtripConsumerEstablish = Roundtrip
  { toMsg = \tr t -> case tr of
      { }
  , fromMsg = \_ -> Unexpected "consumer establish"
  }

roundtripProducerEstablish :: Roundtrip TrChainProducer 'StChainProducerInit 'StChainConsumerAsked Msg
roundtripProducerEstablish = Roundtrip
  { toMsg = \tr t -> case tr of
      TrChainProducerInit -> MsgHeader t
  , fromMsg = \msg -> case msg of
      MsgHeader h -> Expected TrChainProducerInit h
      _ -> Unexpected "producer init"
  }

roundtripConsumerAsk :: Roundtrip TrChainConsumer 'StChainConsumerGotTip 'StChainProducerGaveTip Msg
roundtripConsumerAsk = Roundtrip
  { toMsg = \tr t -> case tr of
      TrChainConsumerAsk -> MsgAsk t
  , fromMsg = \msg -> case msg of
      MsgAsk h -> Expected TrChainConsumerAsk h
      _ -> Unexpected "consumer ask"
  }

roundtripProducerAsk :: Roundtrip TrChainProducer 'StChainProducerGaveTip 'StChainConsumerGotTip Msg
roundtripProducerAsk = Roundtrip
  { toMsg = \tr -> case tr of { }
  , fromMsg = \msg -> undefined
  }

roundtripConsumerAsked :: Roundtrip TrChainConsumer 'StChainConsumerAsked 'StChainProducerGaveTip Msg
roundtripConsumerAsked = Roundtrip
  { toMsg = \tr -> case tr of { }
  , fromMsg = \msg -> Unexpected "consumer asked"
  }

roundtripProducerAsked :: Roundtrip TrChainProducer 'StChainProducerGaveTip 'StChainConsumerAsked Msg
roundtripProducerAsked = Roundtrip
  { toMsg = \tr t -> case tr of
      TrChainProducerInitNo -> MsgNo
      TrChainProducerInitYes -> MsgYes
      TrChainProducerReset -> MsgHeader t
  , fromMsg = \msg -> case msg of
      MsgNo -> Expected TrChainProducerInitNo ()
      MsgYes -> Expected TrChainProducerInitYes ()
      MsgHeader h -> Expected TrChainProducerReset h
      _ -> Unexpected "producer asked"
  }

roundtripConsumerGotRead :: Roundtrip TrChainConsumer 'StChainConsumerGotRead 'StChainProducerGaveRead Msg
roundtripConsumerGotRead = Roundtrip
  { toMsg = \tr t -> case tr of
      TrChainConsumerDownload -> MsgDownload t
      TrChainConsumerImprove -> MsgAsk t
  , fromMsg = \msg -> case msg of
      MsgDownload w -> Expected TrChainConsumerDownload w
      _ -> Unexpected "consumer got read"
  }

roundtripProducerGaveRead :: Roundtrip TrChainProducer 'StChainProducerGaveRead 'StChainConsumerGotRead Msg
roundtripProducerGaveRead = Roundtrip
  { toMsg = \tr -> case tr of { }
  , fromMsg = \_ -> Unexpected "producer gave read"
  }

roundtripConsumerImprove :: Roundtrip TrChainConsumer 'StChainConsumerImprove 'StChainProducerGaveRead Msg
roundtripConsumerImprove = Roundtrip
  { toMsg = \tr -> case tr of { }
  , fromMsg = \_ -> undefined
  }

roundtripProducerImprove :: Roundtrip TrChainProducer 'StChainProducerGaveRead 'StChainConsumerImprove Msg
roundtripProducerImprove = Roundtrip
  { toMsg = \tr _ -> case tr of
      TrChainProducerImproveNo -> MsgNo
      TrChainProducerImproveYes -> MsgYes
  , fromMsg = \msg -> case msg of
      MsgNo -> Expected TrChainProducerImproveNo ()
      MsgYes -> Expected TrChainProducerImproveYes ()
      _ -> Unexpected "producer improve"
  }

roundtripConsumerDownload :: Roundtrip TrChainConsumer 'StChainConsumerDownload 'StChainProducerUpload Msg
roundtripConsumerDownload = Roundtrip
  { toMsg = \tr -> case tr of { }
  , fromMsg = \_ -> Unexpected "consumer download"
  }

roundtripProducerUpload :: Roundtrip TrChainProducer 'StChainProducerUpload 'StChainConsumerDownload Msg
roundtripProducerUpload = Roundtrip
  { toMsg = \tr t -> case tr of
      TrChainProducerUpload -> MsgUpload (fst t) (snd t)
      TrChainProducerUploadDone -> MsgUploadDone
  , fromMsg = \msg -> case msg of
      MsgUpload h b -> Expected TrChainProducerUpload (h, b)
      MsgUploadDone -> Expected TrChainProducerUploadDone ()
      _ -> Unexpected "producer upload"
  }

roundtripsConsumerProducer :: Roundtrips TrChainConsumer TrChainProducer Msg
roundtripsConsumerProducer = Roundtrips
  { nextRoundtripA = \tr -> case tr of
      TrChainConsumerInit -> (roundtripConsumerEstablish, roundtripProducerEstablish)
      TrChainConsumerAsk -> (roundtripConsumerAsked, roundtripProducerAsked)
      TrChainConsumerImprove -> (roundtripConsumerImprove, roundtripProducerImprove)
      TrChainConsumerDownload -> (roundtripConsumerDownload, roundtripProducerUpload)
  , nextRoundtripB = \tr -> case tr of
      TrChainProducerInit -> (roundtripProducerAsk, roundtripConsumerAsk)
      TrChainProducerInitNo -> (roundtripProducerAsk, roundtripConsumerAsk)
      TrChainProducerInitYes -> (roundtripProducerGaveRead, roundtripConsumerGotRead)
      TrChainProducerImproveNo -> (roundtripProducerGaveRead, roundtripConsumerGotRead)
      TrChainProducerImproveYes -> (roundtripProducerGaveRead, roundtripConsumerGotRead)
      TrChainProducerReset -> (roundtripProducerAsk, roundtripConsumerAsk)
      TrChainProducerUploadDone -> (roundtripProducerGaveRead, roundtripConsumerGotRead)
      TrChainProducerUpload -> (roundtripProducerUpload, roundtripConsumerDownload)
  }

-- | Consumer and producer run in different threads, communicating via MVars.
-- They can similarly be run by way of any ordered IPC mechanism,
-- for instance on different machines using a TCP socket.
demo :: IO ((), ())
demo = do
  consumerInbox <- newEmptyMVar
  producerInbox <- newEmptyMVar
  let consumerSend = putMVar producerInbox
      consumerRecv = takeMVar consumerInbox
      producerSend = putMVar consumerInbox
      producerRecv = takeMVar producerInbox
      consumer = runFoldSourceSink
                   roundtripsConsumerProducer
                   roundtripConsumerInit
                   roundtripProducerInit
                   consumerSend
                   consumerRecv
                   demoConsumer
      producer = runFoldSourceSink
                   (roundtripsCommute roundtripsConsumerProducer)
                   roundtripProducerInit
                   roundtripConsumerInit
                   producerSend
                   producerRecv
                   demoProducer
      blowUp :: Either String a -> IO a
      blowUp (Left str) = throwIO (userError str)
      blowUp (Right a) = pure a
  concurrently (consumer >>= blowUp) (producer >>= blowUp)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  demo
  pure ()
