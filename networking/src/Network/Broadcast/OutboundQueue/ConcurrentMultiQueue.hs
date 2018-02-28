{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wall #-}

-- | Concurrent multi-queue
--
-- Intended for qualified import
--
-- > import Network.Broadcast.OutboundQueue.ConcurrentMultiQueue (MultiQueue)
-- > import qualified Network.Broadcast.OutboundQueue.ConcurrentMultiQueue as MQ
--
-- This module provides a data structure which I've termed a
-- /concurrent multi-queue/:
--
-- * Queue because elements are only inserted at the back and removed from
--   the front (though not necessarily the very front).
-- * Multi-queue because it is a collection of queues which can share elements.
-- * Concurrent because multiple threads can read the multi-queue concurrently
--   (though not modify it).
--
-- Every queue itself is basically a fairly standard imperative doubly-linked
-- list, except that since nodes are shared, every node has a bunch of back
-- pointers and a bunch of next pointers. These " pointers " are 'IORef's, not
-- 'MVar's; instead, each queue has a single @MVar ()@ which serves as a
-- write lock for the entire queue.
--
-- >         |       \
-- >         |        -\
-- >         |          -\
-- >     +---|---+    +----\--+    +-------+
-- >     |   |   |    |     -\|    |       |
-- > --------|-----------------\--------------->
-- >     |   |   |    |       | \  |       |
-- >     +---|---+    +-------+  -\+-------+
-- >         |                     -\
-- >         |                       -\
-- >     +---|---+    +-------+    +----\--+
-- >     |   |   |    |       |    |     -\|
-- > --------|--------------------------------->
-- >     |   |   |    |       |    |      /|
-- >     +---|---+    +-------+    +----/--+
-- >         |                        /-
-- >         |                      /-
-- >     +---|---+    +-------+   /--------+
-- >     |   |   |    |       | /- |       |
-- > --------|-----------------/--------------->
-- >     |   |   |    |      /-    |       |
-- >     +---|---+    +----/--+    +-------+
-- >         |           /-
-- >         |         /-
-- >         v        <
module Network.Broadcast.OutboundQueue.ConcurrentMultiQueue
    ( MultiQueue -- opaque
    , new
    , size
    , sizeBy
    , enqueue
    , dequeue
    , remove
    , removeFront
    , removeAllIn
    -- * Tests
    , snapshot
    , tests
    ) where

import           Control.Concurrent
import           Control.Exception (Exception, throwIO)
import           Control.Lens
import           Control.Monad
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)
import           GHC.Stack

{-------------------------------------------------------------------------------
  Concurrent multi-queue

  SKETCH OF CORRECTNESS (LINEARIZABILITY) PROOF

  I'm going to take single-threaded correctness for granted, and focus only
  on potential interference between concurrent operations.

  enqueue/enqueue

    These cannot possibly interfere with each other, since 'enqueue' holds the
    write lock for the entirity of the update. □

  dequeue/dequeue

    Dequeue finds a node in a queue which has not yet been deleted (without
    taking any locks). It then takes the write lock and checks again if the node
    has not yet been deleted. If it has, it releases the lock again and
    continues its search. If it hasn't, it updates the next and back pointers
    of its predecessor and successor and returns the node's value.

    How might two dequeue's interfere with each other? In general, the
    queue will look something like

    > (..) ~ A ~ B ~ C ~ D ~ E ~ F <-> (..)

    We do a case analysis on which two nodes will be dequeued:

    Two nodes far enough apart (at least two nodes in between; e.g., B and E):

      In this case there is no inference; the first dequeue will update
      A and C, and the second dequeue will update D and F. □

    Two identical nodes (e.g., both pick B):

      In this case the first dequeue will succeed and the second dequeue will
      find after it acquires the lock that the node has already been deleted,
      and it will continue searching. □

    Two nodes with a single node in between (e.g., B and D):

      Suppose B gets dequeued first. The predecessor of C becomes A:

      > (..) ~ A ~ C ~ D ~ E ~ F <-> (..)

      Now when D is dequeued, the successor of C becomes E:

      > (..) ~ A ~ C ~ E ~ F <-> (..)

      There is no interference, as one dequeue only modifies the precessor and
      the other only modifies the successor.

      The same argument holds, mutatis mutandis, when D is dequeued first. □

    Two adjacent nodes (e.g., B and C):

      Suppose B gets dequeued first. The predecessor of C becomes A:

      > (..) ~ A ~ C ~ D ~ E ~ F <-> (..)

      Now when we dequeue C, we need to make sure that the predecessor of D
      becomes A, not B; this will be the case if we read these next and back
      pointers after having obtained the lock.

      The same argument holds, mutatis mutandis, when C is dequeued first. □

  enqueue/dequeue

    If an element is not found because it was enqueue during the search,
    then that is okay: we can consider the enqueue to have happened after the
    dequeue.

    We do have to worry about what happens when the enqueue and the dequeue
    both modify the end of the queue. If the queue looks something like

    > (..) ~ X ~ Y ~ Z

    We do a case analysis on which node we dequeue:

    Dequeueing the end of the queue (Z):

      If the enqueue happens first, adding a new node N at the end

      > (..) ~ X ~ Y ~ Z ~ N

      then there is no problem; enqueuing happens with the write lock taken
      and dequeue will wait for that lock to be released before modifying Z.

      If the dequeue happens first, it will modify the queue with the write
      lock taken, so that when the enqueue proceeds it will see the queue

      > (..) ~ X ~ Y

      instead. Again, all good. □

    Dequeueing any other item:

      Suppose we dequeue Y. This will modify the back pointer of Z, and
      enqueue will modify the forward pointer of Z. No interference.

      No interference is possible when dequeuing even earlier elements. □
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Node
-------------------------------------------------------------------------------}

-- | Node in the linked list
data Node k a = Node {
      -- | The value stored in the node
      _value   :: a

      -- | Mark whether or not the node has been removed from the linked list
      --
      -- This is used for concurrency, as discussed above.
    , _deleted :: Bool

      -- | Links to previous and next node for each list the node is part of
      --
      -- Invariants:
      --
      -- * There must be an entry in the map for each list the node is part of
      -- * The kind of link must match the 'Ends' recorded for the list
      --   (i.e., if the node is listed as the front of the list, the 'Links'
      --   must be 'Front' or 'Singleton').
    , _links   :: Map k (Links k a)
    }

-- | Links from a node to its (previous and next) neighbours
data Links k a =
    Middle (PNode k a) (PNode k a)
  | Front              (PNode k a)
  | Back   (PNode k a)
  | Singleton

-- | Pointer to a node
type PNode k a = IORef (Node k a)

makeLenses ''Node

{-------------------------------------------------------------------------------
  Statistics about the queue
-------------------------------------------------------------------------------}

-- | Statistics about the queue
data Stats k = Stats {
      _countPerKey :: Map k Int
    , _totalCount  :: Int
    }

makeLenses ''Stats

emptyStats :: Stats k
emptyStats = Stats {
      _countPerKey = Map.empty
    , _totalCount  = 0
    }

modifyCounts :: Ord k => (Int -> Int) -> [k] -> Stats k -> Stats k
modifyCounts f = go
  where
    go []      !stats' = stats' & totalCount %~ f
    go (k:ks') !stats' = go ks' $ stats' & countPerKey . ix k %~ f

increaseCounts, decreaseCounts :: Ord k => [k] -> Stats k -> Stats k
increaseCounts = modifyCounts (\n -> n + 1)
decreaseCounts = modifyCounts (\n -> n - 1)

{-------------------------------------------------------------------------------
  Multiqueue proper
-------------------------------------------------------------------------------}

-- | Front and back of a queue
data Ends k a = Ends { _front :: PNode k a, _back :: PNode k a }

makeLenses ''Ends

-- | We use a single write lock to protect modifications to the queue
type Lock k = MVar (Stats k)

-- | Concurrent multi-queue
data MultiQueue k a = MultiQueue {
      pEnds     :: IORef (Map k (Ends k a))
    , writeLock :: Lock k
    }

{-------------------------------------------------------------------------------
  Lenses

  Although the lens definitions for 'Links' are pretty straight-forward, it's
  not immediately obvious that they don't break the lens laws, so I've spelled
  out the proof for 'next':

  > view next (set next mn l) == n.
  >
  >   Need to prove: g (s l mn) == mn
  >
  >   Case mn == Nothing. Case analysis on l.
  >
  >     g (s (Middle p _n) Nothing) == g (Back p)    == Nothing
  >     g (s (Front    _n) Nothing) == g (Singleton) == Nothing
  >     g (s (Back   p   ) Nothing) == g (Back p)    == Nothing
  >     g (s Singleton     Nothing) == g (Singleton) == Nothing
  >
  >   Case mn == Just n'. Case analysis on l.
  >
  >     g (s (Middle p _n) (Just n')) == g (Middle p n') == Just n'
  >     g (s (Front    _n) (Just n')) == g (Front    n') == Just n'
  >     g (s (Back   p   ) (Just n')) == g (Middle p n') == Just n'
  >     g (s Singleton     (Just n')) == g (Front    n') == Just n' □
  >
  > set next (view next l) l == l
  >
  >   Need to prove: s l (g l) == l
  >
  >   Case analysis on l.
  >
  >     s (Middle _p n) (g (Middle _p n)) == s (Middle _p n) (Just n) == Middle _p n
  >     s (Front     n) (g (Front     n)) == s (Front     n) (Just n) == Front n
  >     s (Back   _p  ) (g (Back   _p  )) == s (Back   _p  ) Nothing  == Back _p
  >     s Singleton     (g Singleton    ) == s Singleton     Nothing  == Singleton □
  >
  > set next mn' (set next mn l) == set next mn' l
  >
  >   Need to prove: s (s l mn) mn' == s l mn'
  >
  >   Case mn == Nothing, mn' = Nothing. Case analysis on l.
  >
  >     s (s (Middle p _n) Nothing) Nothing == s (Back p)  Nothing == Back p    == s (Middle p _n) Nothing
  >     s (s (Front    _n) Nothing) Nothing == s Singleton Nothing == Singleton == s (Front    _n) Nothing
  >     s (s (Back   p   ) Nothing) Nothing == s (Back p)  Nothing == Back p    == s (Back   p   ) Nothing
  >     s (s Singleton     Nothing) Nothing == s Singleton Nothing == Singleton == s Singleton     Nothing
  >
  >   Case mn == Just n', mn' = Nothing. Case analysis on l.
  >
  >     s (s (Middle p _n) (Just n')) Nothing == s (Middle p n') Nothing == Back p    == s (Middle p _n) Nothing
  >     s (s (Front    _n) (Just n')) Nothing == s (Front    n') Nothing == Singleton == s (Front    _n) Nothing
  >     s (s (Back   p   ) (Just n')) Nothing == s (Middle p n') Nothing == Back p    == s (Back   p   ) Nothing
  >     s (s Singleton     (Just n')) Nothing == s (Front    n') Nothing == Singleton == s Singleton     Nothing
  >
  >   Case mn = Nothing, mn' = n''. Case analysis on l.
  >
  >     s (s (Middle p _n) Nothing) (Just n'') = s (Back p)  (Just n'') == Middle p n'' == s (Middle p _n) (Just n'')
  >     s (s (Front    _n) Nothing) (Just n'') = s Singleton (Just n'') == Front n''    == s (Front    _n) (Just n'')
  >     s (s (Back   p   ) Nothing) (Just n'') = s (Back p)  (Just n'') == Middle p n'' == s (Back   p   ) (Just n'')
  >     s (s Singleton     Nothing) (Just n'') = s Singleton (Just n'') == Front n''    == s Singleton     (Just n'')
  >
  >   Case mn = Just n', mn' = Just n''. Case analysis on l.
  >
  >     s (s (Middle p _n) (Just n')) (Just n'') == s (Middle p n') (Just n'') == Middle p n'' == s (Middle p _n) (Just n'')
  >     s (s (Front    _n) (Just n')) (Just n'') == s (Front    n') (Just n'') == Front n''    == s (Front    _n) (Just n'')
  >     s (s (Back   p   ) (Just n')) (Just n'') == s (Middle p n') (Just n'') == Middle p n'' == s (Back   p   ) (Just n'')
  >     s (s Singleton     (Just n')) (Just n'') == s (Front    n') (Just n'') == Front n''    == s Singleton     (Just n'') □
-------------------------------------------------------------------------------}

-- | Links from this node in a particular queue
--
-- NOTE: This should /ONLY/ be used if (externally) it is know that this node
-- in indeed part of this queue.
linksAt :: Ord k => k -> Lens' (Node k a) (Links k a)
linksAt k = unsafeSingular $ links . ix k

-- | Next pointer, if any.
--
-- The getter returns the next pointer, if one exists. The setter overrides or
-- remove the next pointer, possibly changing the type of link in the process
-- (for instance, removing the next of a middle node turns it into a back node).
next :: Lens' (Links k a) (Maybe (PNode k a))
next = lens g s
  where
    g :: Links k a -> Maybe (PNode k a)

    g (Middle _p n) = Just n
    g (Front     n) = Just n
    g (Back   _p  ) = Nothing
    g Singleton     = Nothing

    s :: Links k a -> Maybe (PNode k a) -> Links k a

    s (Middle p _n) Nothing   = Back p
    s (Front    _n) Nothing   = Singleton
    s (Back   p   ) Nothing   = Back p
    s Singleton     Nothing   = Singleton

    s (Middle p _n) (Just n') = Middle p n'
    s (Front    _n) (Just n') = Front    n'
    s (Back   p   ) (Just n') = Middle p n'
    s Singleton     (Just n') = Front    n'

-- | Previous node, if any.
--
-- See detailed discussion at 'next'.
prev :: Lens' (Links k a) (Maybe (PNode k a))
prev = lens g s
  where
    g :: Links k a -> Maybe (PNode k a)

    g (Middle p _n) = Just p
    g (Front    _n) = Nothing
    g (Back   p  )  = Just p
    g Singleton     = Nothing

    s :: Links k a -> Maybe (PNode k a) -> Links k a

    s (Middle _p n) Nothing   = Front n
    s (Front     n) Nothing   = Front n
    s (Back   _p  ) Nothing   = Singleton
    s Singleton     Nothing   = Singleton

    s (Middle _p n) (Just p') = Middle p' n
    s (Front     n) (Just p') = Middle p' n
    s (Back   _p  ) (Just p') = Back p'
    s Singleton     (Just p') = Back p'

{-------------------------------------------------------------------------------
  Construction and modification
-------------------------------------------------------------------------------}

new :: IO (MultiQueue k a)
new = do
    pEnds     <- newIORef Map.empty
    writeLock <- newMVar emptyStats
    return MultiQueue{..}

enqueue :: forall k a. Ord k => MultiQueue k a -> [k] -> a -> IO ()
enqueue MultiQueue{..} ks a = modifyMVar_ writeLock $ \stats -> do
    ends <- readIORef pEnds

    let -- Find the back of queue @k@
        backOfQueue :: k -> Maybe (PNode k a)
        backOfQueue k = _back <$> Map.lookup k ends

        -- Construct 'Links' for the new node given the previous node
        -- (the new node is always inserted at the end of the queue)
        mkLink :: Maybe (PNode k a) -> Links k a
        mkLink Nothing  = Singleton
        mkLink (Just p) = Back p

        newNode :: Node k a
        newNode = Node {
            _value   = a
          , _deleted = False
          , _links   = Map.fromList $ map (\k -> (k, mkLink (backOfQueue k))) ks
          }

    pNewBack :: PNode k a <- newIORef newNode

    let updateNexts :: [k] -> Map k (Ends k a) -> IO (Map k (Ends k a))
        updateNexts []      ends' = return ends'
        updateNexts (k:ks') ends' =
          case ends' ^. at k of
            Nothing -> do
              let pNewFront = pNewBack -- First node in this particular queue
              updateNexts ks' $ ends' & at k .~ Just (Ends pNewFront pNewBack)
            Just (Ends _pOldFront pOldBack) -> do
              modIORef pOldBack $ linksAt k . next .~ Just pNewBack
              updateNexts ks' $ ends' & ix k . back .~ pNewBack

    writeIORef pEnds =<< updateNexts ks ends
    return $! increaseCounts ks stats

-- | Total size of the queue
size :: MultiQueue k a -> IO Int
size MultiQueue{..} =
    view totalCount <$> readMVar writeLock

-- | Size of the specified queue
sizeBy :: Ord k => k -> MultiQueue k a -> IO Int
sizeBy k MultiQueue{..} =
    fromMaybe 0 . view (countPerKey . at k) <$> readMVar writeLock

-- | Remove a node from the queues
--
-- The node is located starting at queue @k@, but removed from /all queues.
dequeue :: Ord k => k -> (a -> Bool) -> MultiQueue k a -> IO (Maybe a)
dequeue k p qs@MultiQueue{..} =
    maybe (return Nothing) (dequeueFrom k p qs) =<< find k p qs

--  Dequeue starting at a particular node
dequeueFrom :: Ord k
            => k -> (a -> Bool) -> MultiQueue k a -> PNode k a -> IO (Maybe a)
dequeueFrom k p qs@MultiQueue{..} pNode = do
    -- We try to remove the node from the queue, returning 'Nothing' if the node
    -- was already deleted by someone else
    ma <- modifyMVar writeLock $ \stats -> do
      cur <- readIORef pNode
      if cur ^. deleted
        then return (stats, Nothing)
        else do
          modIORef pNode $ deleted .~ True

          forM_ (Map.toList (cur ^. links)) $ \(k', linksK') -> case linksK' of

            Middle pPrev pNext -> do
              modIORef pPrev $ links . ix k' . next .~ Just pNext
              modIORef pNext $ links . ix k' . prev .~ Just pPrev

            Front pNext -> do
              modIORef pNext $ links . ix k' . prev .~ Nothing
              modIORef pEnds $ ix k' . front .~ pNext

            Back pPrev -> do
              modIORef pPrev $ links . ix k' . next .~ Nothing
              modIORef pEnds $ ix k' . back .~ pPrev

            Singleton ->
              modIORef pEnds $ at k' .~ Nothing

          return (
              decreaseCounts (Map.keys (cur ^. links)) stats
            , Just (cur ^. value)
            )

    case ma of
      Just a  -> return (Just a)
      Nothing -> findFrom k p pNode
             >>= maybe (return Nothing) (dequeueFrom k p qs)

-- | Remove a node from the queue (if a matching node can be found)
remove :: Ord k => k -> (a -> Bool) -> MultiQueue k a -> IO ()
remove k p qs = void $ dequeue k p qs

-- | Remove the first element from the queue (if such an element exists)
removeFront :: Ord k => k -> MultiQueue k a -> IO ()
removeFront k = remove k (const True)

-- | Remove all elements in the given queue
--
-- The queue may not be empty when 'removeAllIn' returns if there were
-- concurrent enqueues.
removeAllIn :: Ord k => k -> MultiQueue k a -> IO ()
removeAllIn k qs = go
  where
    go :: IO ()
    go = do
      ma <- dequeue k (const True) qs
      when (isJust ma) go

-- | Locate a node satisfying a predicate
--
-- Returns the node pointer. Internal function.
find :: Ord k => k -> (a -> Bool) -> MultiQueue k a -> IO (Maybe (PNode k a))
find k p MultiQueue{..} = do
    ends <- readIORef pEnds
    maybe (return Nothing) (findFrom k p . _front) (ends ^. at k)

-- | Locate a node satisfying a predicate, starting from a given node
findFrom :: Ord k => k -> (a -> Bool) -> PNode k a -> IO (Maybe (PNode k a))
findFrom k p pNode = do
    node <- readIORef pNode
    if not (node ^. deleted) && p (node ^. value)
      then return $ Just pNode
      else maybe (return Nothing) (findFrom k p) (node ^. linksAt k . next)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

modIORef :: IORef a -> (a -> a) -> IO ()
modIORef ref f = atomicModifyIORef' ref ((, ()) . f)

{-------------------------------------------------------------------------------
  Tests

  TODO: QuickCheck? Mocking? Test framework?
-------------------------------------------------------------------------------}

-- | Take a snapshot of the queue
--
-- We take the lock so that the queue cannot be modified.
snapshot :: forall k a. Ord k => MultiQueue k a -> IO [(k, [a])]
snapshot MultiQueue{..} = withMVar writeLock $ \_stats -> do
    ends <- readIORef pEnds
    mapM (\(k, Ends fr _) -> (k,) <$> snapshotQueue k fr) $ Map.toList ends
  where
    snapshotQueue :: k -> PNode k a -> IO [a]
    snapshotQueue k pNode = do
      node <- readIORef pNode
      as   <- maybe (return []) (snapshotQueue k) (node ^. linksAt k . next)
      return (node ^. value : as)

-- | Very simple test, single queue
test1 :: IO ()
test1 = do
    q :: MultiQueue () Char <- new

    enqueue q [()] 'A'
    assertEq [((), "A")]   =<< snapshot q
    enqueue q [()] 'B'
    assertEq [((), "AB")]  =<< snapshot q
    enqueue q [()] 'C'
    assertEq [((), "ABC")] =<< snapshot q

    assertEq (Just 'A')    =<< dequeue () (const True) q
    assertEq [((), "BC")]  =<< snapshot q
    assertEq (Just 'B')    =<< dequeue () (const True) q
    assertEq [((), "C")]   =<< snapshot q
    assertEq (Just 'C')    =<< dequeue () (const True) q
    assertEq []            =<< snapshot q
    assertEq Nothing       =<< dequeue () (const True) q
    assertEq []            =<< snapshot q

-- | Two intersecting queues
--
-- >      X
-- >      |
-- > A -- O -- C
-- >      |
-- >      Y
test2 :: IO ()
test2 = do
    q :: MultiQueue Bool Char <- new

    enqueue q [True] 'A'
    assertEq [(True,"A")]                 =<< snapshot q
    enqueue q [False] 'X'
    assertEq [(False,"X"),(True,"A")]     =<< snapshot q
    enqueue q [True, False] 'O'
    assertEq [(False,"XO"),(True,"AO")]   =<< snapshot q
    enqueue q [True] 'C'
    assertEq [(False,"XO"),(True,"AOC")]  =<< snapshot q
    enqueue q [False] 'Y'
    assertEq [(False,"XOY"),(True,"AOC")] =<< snapshot q

    assertEq (Just 'O')                   =<< dequeue True (== 'O') q
    assertEq [(False,"XY"),(True,"AC")]   =<< snapshot q
    assertEq Nothing                      =<< dequeue True (== 'X') q
    assertEq [(False,"XY"),(True,"AC")]   =<< snapshot q
    assertEq (Just 'X')                   =<< dequeue False (== 'X') q
    assertEq [(False,"Y"),(True,"AC")]    =<< snapshot q

tests :: IO ()
tests = do
    test1
    test2

assertEq :: HasCallStack => Eq a => a -> a -> IO ()
assertEq expected actual =
    if expected == actual
      then return ()
      else throwIO $ AssertionFailure ?callStack

data AssertionFailure = AssertionFailure CallStack
  deriving (Show)

instance Exception AssertionFailure
