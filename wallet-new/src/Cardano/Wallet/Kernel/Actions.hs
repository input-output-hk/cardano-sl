{-# LANGUAGE LambdaCase #-}
module Cardano.Wallet.Kernel.Actions
    ( WalletAction(..)
    , WalletActionInterp(..)
    , withWalletWorker
    , WalletWorkerExpiredError(..)
    , interp
    , interpList
    , WalletWorkerState
    , isInitialState
    , hasPendingFork
    , isValidState
    ) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception.Safe as Ex
import           Control.Lens (makeLenses, (%=), (+=), (-=), (.=))
import           Formatting (bprint, build, shown, (%))
import qualified Formatting.Buildable
import           Universum

import           Pos.Core.Chrono

{-------------------------------------------------------------------------------
  Workers and helpers for performing wallet state updates
-------------------------------------------------------------------------------}

-- | Actions that can be invoked on a wallet, via a worker.
--   Workers may not respond directly to each action; for example,
--   a `RollbackBlocks` followed by several `ApplyBlocks` may be
--   batched into a single operation on the actual wallet.
data WalletAction b
    = ApplyBlocks    (OldestFirst NE b)
    | RollbackBlocks (NewestFirst NE b)
    | LogMessage Text

-- | Interface abstraction for the wallet worker.
--   The caller provides these primitive wallet operations;
--   the worker uses these to invoke changes to the
--   underlying wallet.
data WalletActionInterp m b = WalletActionInterp
    { applyBlocks  :: OldestFirst NE b -> m ()
    , switchToFork :: Int -> OldestFirst [] b -> m ()
    , emit         :: Text -> m ()
    }

-- | Internal state of the wallet worker.
data WalletWorkerState b = WalletWorkerState
    { _pendingRollbacks    :: !Int
    , _pendingBlocks       :: !(NewestFirst [] b)
    , _lengthPendingBlocks :: !Int
    }
  deriving Eq

makeLenses ''WalletWorkerState

-- A helper function for lifting a `WalletActionInterp` through a monad transformer.
lifted :: (Monad m, MonadTrans t) => WalletActionInterp m b -> WalletActionInterp (t m) b
lifted i = WalletActionInterp
    { applyBlocks  = lift . applyBlocks i
    , switchToFork = \n bs -> lift (switchToFork i n bs)
    , emit         = lift . emit i
    }

-- | `interp` is the main interpreter for converting a wallet action to a concrete
--   transition on the wallet worker's state, perhaps combined with some effects on
--   the concrete wallet.
interp :: Monad m => WalletActionInterp m b -> WalletAction b -> StateT (WalletWorkerState b) m ()
interp walletInterp action = do

    numPendingRollbacks <- use pendingRollbacks
    numPendingBlocks    <- use lengthPendingBlocks

    -- Respond to the incoming action
    case action of

      -- If we are not in the midst of a rollback, just apply the blocks.
      ApplyBlocks bs | numPendingRollbacks == 0 -> do
                         emit "applying some blocks (non-rollback)"
                         applyBlocks bs

      -- Otherwise, add the blocks to the pending list. If the resulting
      -- list of pending blocks is longer than the number of pending rollbacks,
      -- then perform a `switchToFork` operation on the wallet.
      ApplyBlocks bs -> do

        -- Add the blocks
        let bsList = toNewestFirst (OldestFirst (toList (getOldestFirst bs)))
        pendingBlocks %= prependNewestFirst bsList
        lengthPendingBlocks += length bs

        -- If we have seen more blocks than rollbacks, switch to the new fork.
        when (numPendingBlocks + length bs > numPendingRollbacks) $ do

          pb <- toOldestFirst <$> use pendingBlocks
          switchToFork numPendingRollbacks pb

          -- Reset state to "no fork in progress"
          pendingRollbacks    .= 0
          lengthPendingBlocks .= 0
          pendingBlocks       .= NewestFirst []

      -- If we are in the midst of a fork and have seen some new blocks,
      -- roll back some of those blocks. If there are more rollbacks requested
      -- than the number of new blocks, see the next case below.
      RollbackBlocks bs | length bs <= numPendingBlocks -> do
                            lengthPendingBlocks -= length bs
                            pendingBlocks %= NewestFirst . drop (length bs) . getNewestFirst

      -- If we are in the midst of a fork and are asked to rollback more than
      -- the number of new blocks seen so far, clear out the list of new
      -- blocks and add any excess to the number of pending rollback operations.
      RollbackBlocks bs -> do
        pendingRollbacks    += length bs - numPendingBlocks
        lengthPendingBlocks .= 0
        pendingBlocks       .= NewestFirst []

      LogMessage txt -> emit txt

  where
    WalletActionInterp{..} = lifted walletInterp
    prependNewestFirst bs = \nf -> NewestFirst (getNewestFirst bs <> getNewestFirst nf)

-- | Connect a wallet action interpreter to a source of actions. This function
-- returns as soon as the given action returns 'Nothing'.
walletWorker
  :: Ex.MonadMask m
  => WalletActionInterp m b
  -> m (Maybe (WalletAction b))
  -> m ()
walletWorker wai getWA = Ex.bracket_
  (emit wai "Starting wallet worker.")
  (evalStateT
     (fix $ \next -> lift getWA >>= \case
        Nothing -> pure ()
        Just wa -> interp wai wa >> next)
     initialWorkerState)
  (emit wai "Stopping wallet worker.")

-- | Connect a wallet action interpreter to a stream of actions.
interpList :: Monad m => WalletActionInterp m b -> [WalletAction b] -> m (WalletWorkerState b)
interpList ops actions = execStateT (forM_ actions $ interp ops) initialWorkerState

initialWorkerState :: WalletWorkerState b
initialWorkerState = WalletWorkerState
    { _pendingRollbacks    = 0
    , _pendingBlocks       = NewestFirst []
    , _lengthPendingBlocks = 0
    }

-- | Thrown by 'withWalletWorker''s continuation in case it's used outside of
-- its intended scope.
data WalletWorkerExpiredError = WalletWorkerExpiredError deriving (Show)
instance Ex.Exception WalletWorkerExpiredError

-- | Start a wallet worker in backround who will react to input provided via the
-- 'STM' function, in FIFO order.
--
-- After the given continuation returns (successfully or due to some exception),
-- the worker will continue processing any pending input before returning,
-- re-throwing the continuation's exception if any. Async exceptions from any
-- source will always be prioritized.
--
-- Usage of the obtained 'STM' action after the given continuation has returned
-- is not possible. It will throw 'WalletWorkerExpiredError'.
withWalletWorker
  :: (MonadIO m, Ex.MonadMask m)
  => WalletActionInterp IO a
  -> ((WalletAction a -> STM ()) -> m b)
  -> m b
withWalletWorker wai k = do
  -- 'tmq' keeps items to be processed by the worker in FIFO order.
  tmq :: TMQueue (WalletAction a) <- liftIO newTMQueueIO
  -- 'getWA' gets the next action to be processed.
  let getWA :: STM (Maybe (WalletAction a))
      getWA = readTMQueue tmq
  -- 'pushWA' adds an action to queue, unless it's been closed already.
  let pushWA :: WalletAction a -> STM ()
      pushWA = writeTMQueue tmq >=> \case
         True -> pure ()
         False -> Ex.throwM WalletWorkerExpiredError
  -- Run the worker in the background, ensuring that any exceptions from it
  -- get thrown to the current thread.
  Ex.bracket
     (liftIO $ do
        as1 <- Async.async (walletWorker wai (STM.atomically getWA))
        Async.link as1
        pure as1)
     (\as1 -> liftIO $ do
        -- Prevent new input.
        STM.atomically (closeTMQueue tmq)
        -- Wait for the worker to finish, re-throwing any exceptions from it.
        Async.wait as1)
     (\_ -> k pushWA)


-- | Check if this is the initial worker state.
isInitialState :: Eq b => WalletWorkerState b -> Bool
isInitialState = (== initialWorkerState)

-- | Check that the state invariants all hold.
isValidState :: WalletWorkerState b -> Bool
isValidState WalletWorkerState{..} =
    _pendingRollbacks >= 0 &&
    length (_pendingBlocks) == _lengthPendingBlocks &&
    _lengthPendingBlocks <= _pendingRollbacks

-- | Check if this state represents a pending fork.
hasPendingFork :: WalletWorkerState b -> Bool
hasPendingFork WalletWorkerState{..} = _pendingRollbacks /= 0

instance Show b => Buildable (WalletWorkerState b) where
    build WalletWorkerState{..} = bprint
      ( "WalletWorkerState "
      % "{ _pendingRollbacks:    " % shown
      % ", _pendingBlocks:       " % shown
      % ", _lengthPendingBlocks: " % shown
      % " }"
      )
      _pendingRollbacks
      _pendingBlocks
      _lengthPendingBlocks

instance Show b => Buildable (WalletAction b) where
    build wa = case wa of
      ApplyBlocks bs    -> bprint ("ApplyBlocks " % shown) bs
      RollbackBlocks bs -> bprint ("RollbackBlocks " % shown) bs
      LogMessage bs     -> bprint ("LogMessage " % shown) bs

instance Show b => Buildable [WalletAction b] where
    build was = case was of
      []     -> bprint "[]"
      (x:xs) -> bprint (build % ":" % build) x xs

--------------------------------------------------------------------------------
-- STM closeable queue.

-- | A FIFO queue that can be closed, preventing new input from being writen to
-- it.
--
-- This is similar to 'Control.Concurrent.STM.TMQueue', redefined here with some
-- of its API to avoid a dependency on the 'stm-chans' library.
data TMQueue a
  = UnsafeTMQueue !(STM.TVar TMQueueOpen) !(STM.TQueue a)
  -- ^ Don't use this constructor directly. It's internal. It carries the queue
  -- itself, and whether this 'TMQueue' is open or not.

data TMQueueOpen = TMQueueOpen | TMQueueNotOpen

-- | Creates a new empty and open 'TMQueue'.
newTMQueueIO :: IO (TMQueue a)
newTMQueueIO = UnsafeTMQueue <$> STM.newTVarIO TMQueueOpen <*> STM.newTQueueIO

-- | Closes the 'TMQueue'. After this, any elements already in the 'TMQueue'
-- will continue to be successfully returned by 'readTMQueue'. However, any
-- new writes with 'writeTMQueue' will fail as described by its documentation.
closeTMQueue :: TMQueue a -> STM ()
closeTMQueue (UnsafeTMQueue to _) = STM.writeTVar to TMQueueNotOpen

-- | Writes a new input to the 'TMQueue', in FIFO order.
--
-- It returns 'True' if the 'TMQueue' was open and it was possible to write to
-- it. Otherwise, if the 'TMQueue' was closed, it returns 'False', meaning
-- nothing has been writen to the queue.
writeTMQueue :: TMQueue a -> a -> STM Bool
writeTMQueue (UnsafeTMQueue to tq) a = do
  STM.readTVar to >>= \case
     TMQueueOpen -> STM.writeTQueue tq a >> pure True
     TMQueueNotOpen -> pure False

-- | Read a value from the 'TMQueue', in FIFO order.
--
-- If the 'TMQueue' is empty and closed, then this function returns 'Nothing'.
-- Otherwise, if the 'TMQueue' is not closed, this function will block waiting
-- for new input.
readTMQueue :: TMQueue a -> STM (Maybe a)
readTMQueue (UnsafeTMQueue to tq) = do
  STM.tryReadTQueue tq >>= \case
     Just a -> pure (Just a)
     Nothing -> STM.readTVar to >>= \case
        TMQueueOpen -> Just <$> STM.readTQueue tq
        TMQueueNotOpen -> pure Nothing


