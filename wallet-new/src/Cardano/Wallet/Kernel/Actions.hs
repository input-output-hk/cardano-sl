{-# LANGUAGE LambdaCase #-}
module Cardano.Wallet.Kernel.Actions
    ( WalletAction(..)
    , WalletActionInterp(..)
    , withWalletWorker
    , interp
    , interpList
    , WalletWorkerState
    , isInitialState
    , hasPendingFork
    , isValidState
    ) where

import           Control.Monad.Morph (MFunctor(hoist))
import           Control.Exception (throwTo)
import qualified Control.Monad.Catch as Ex
import           Control.Concurrent (ThreadId, myThreadId, killThread, forkFinally)
import qualified Control.Concurrent.STM as STM
import           Control.Lens (makeLenses, (%=), (+=), (-=), (.=))
import qualified Data.Text.Buildable
import           Formatting (bprint, build, shown, (%))
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
data WalletWorkerState b
    = Normal
    | PendingFork (PendingForkState b)
  deriving Eq

-- | State during a rollback
data PendingForkState b = PendingForkState
    { _pendingRollbacks    :: !Int
    , _pendingBlocks       :: !(NewestFirst [] b)
    , _lengthPendingBlocks :: !Int
    }
  deriving Eq

makeLenses ''PendingForkState

instance MFunctor WalletActionInterp where
  hoist nat i = WalletActionInterp
    { applyBlocks  = nat . applyBlocks i
    , switchToFork = \n bs -> nat (switchToFork i n bs)
    , emit         = nat . emit i
    }

-- | Sub-interpreters return a `StateChange` to indicate what state
--   the main interpreter should transition to next.
--   `NoTransition` is used to indicate that the currently active
--   state machine couldn't handle the incoming action.
data StateChange b = Same
                   | ChangeTo (WalletWorkerState b)
                   | NoTransition

-- | `interp` is the main interpreter for converting a wallet action to a concrete
--   transition on the wallet worker's state, perhaps combined with some effects on
--   the concrete wallet.
interp :: forall m b
       .  Monad m
       => WalletActionInterp m b
       -> WalletAction b
       -> StateT (WalletWorkerState b) m ()
interp walletInterp action = do
    curState <- get
    case (action, curState) of
        (LogMessage txt, _) -> lift (emit walletInterp txt)
        (_, Normal)         -> run (const Normal) () interpNormal
        (_, PendingFork s)  -> run PendingFork s interpPendingFork
  where
    run :: (s -> WalletWorkerState b)
        -> s
        -> (WalletActionInterp m b
            -> WalletAction b
            -> StateT s m (StateChange b))
        -> StateT (WalletWorkerState b) m ()
    run ctor s0 f = do
      (next, s1) <- lift $ runStateT (f walletInterp action) s0
      put $ case next of
        Same         -> ctor s1
        ChangeTo x   -> x
        NoTransition -> error "Bad transition in the wallet worker!"

-- | Interpret the incoming action, from the typical state where
--   the wallet is just expecting a new block.
interpNormal :: Monad m
             => WalletActionInterp m b
             -> WalletAction b
             -> StateT () m (StateChange b)
interpNormal walletInterp action = do
    -- Respond to the incoming action
    case action of
        --Just apply the blocks.
        ApplyBlocks bs -> lift $ do
            emit walletInterp "applying some blocks (non-rollback)"
            applyBlocks walletInterp bs
            return Same

        -- Kick off a pending fork operation.
        RollbackBlocks bs -> changeTo $ PendingFork $ PendingForkState
            { _pendingRollbacks    = length bs
            , _lengthPendingBlocks = 0
            , _pendingBlocks       = NewestFirst []
            }

        _ -> return NoTransition

-- | Interpret the incoming action, when in the state where we
--   are waiting for enough rollbacks and new blocks to complete
--   a fork.
interpPendingFork :: Monad m
                  => WalletActionInterp m b
                  -> WalletAction b
                  -> StateT (PendingForkState b) m (StateChange b)
interpPendingFork walletInterp action = do
    numPendingRollbacks <- use pendingRollbacks
    numPendingBlocks    <- use lengthPendingBlocks

    -- Respond to the incoming action
    case action of

      -- Add the blocks to the pending list. If the resulting
      -- list of pending blocks is longer than the number of pending rollbacks,
      -- then perform a `switchToFork` operation on the wallet and switch
      -- back to the "normal" worker state.
      ApplyBlocks bs -> do

        -- Add the blocks
        let bsList = toNewestFirst (OldestFirst (toList (getOldestFirst bs)))
        pendingBlocks %= prependNewestFirst bsList
        lengthPendingBlocks += length bs

        -- If we have seen more blocks than rollbacks, switch to the new fork.
        if (numPendingBlocks + length bs > numPendingRollbacks)
          then do pb <- toOldestFirst <$> use pendingBlocks
                  lift (switchToFork walletInterp numPendingRollbacks pb)

                  -- Reset state to "no fork in progress"
                  changeTo Normal
          else return Same

      -- If we have seen some new blocks, roll back some of those blocks.
      -- If there are more rollbacks requested than the number of new
      -- blocks, see the next case below.
      RollbackBlocks bs | length bs <= numPendingBlocks -> do
                            lengthPendingBlocks -= length bs
                            pendingBlocks %= NewestFirst . drop (length bs) . getNewestFirst
                            return Same

      -- If we are asked to rollback more than the number of new blocks
      -- seen so far, clear out the list of new blocks and add any excess
      -- to the number of pending rollback operations.
      RollbackBlocks bs -> do
        pendingRollbacks    += length bs - numPendingBlocks
        lengthPendingBlocks .= 0
        pendingBlocks       .= NewestFirst []
        return Same

      _ -> return NoTransition

  where
    prependNewestFirst bs = \nf -> NewestFirst (getNewestFirst bs <> getNewestFirst nf)

changeTo :: Monad m => WalletWorkerState b -> m (StateChange b)
changeTo wws = return (ChangeTo wws)

-- | Connect a wallet action interpreter to a source actions. This function
-- returns as soon as the given action returns 'Nothing'.
walletWorker
  :: Ex.MonadMask m
  => WalletActionInterp m b
  -> m (Maybe (WalletAction b))
  -> m ()
walletWorker wai getWA = do
  emit wai "Starting wallet worker."
  evalStateT
     (fix $ \k -> lift getWA >>= \case
        Nothing -> lift (emit wai "Stoping wallet worker.")
        Just wa -> interp wai wa >> k)
     initialWorkerState

-- | Connect a wallet action interpreter to a stream of actions.
interpList :: Monad m => WalletActionInterp m b -> [WalletAction b] -> m (WalletWorkerState b)
interpList ops actions = execStateT (forM_ actions $ interp ops) initialWorkerState

initialWorkerState :: WalletWorkerState b
initialWorkerState = Normal

-- | Start a wallet worker in backround who will react to input provided via the
-- 'STM' function, in FIFO order.
--
-- After the given continuation returns, the worker will continue processing any
-- pending input before stopping immediately afterwards.
--
-- Usage of the obtained 'STM' action after the given continuation has returned
-- will fail with an exception.
withWalletWorker
  :: (MonadIO m, Ex.MonadMask m)
  => WalletActionInterp IO a
  -> ((WalletAction a -> STM ()) -> m b)
  -> m b
withWalletWorker wai k = do
  -- 'mDone' is full if the worker finished.
  mDone :: MVar () <- liftIO newEmptyMVar
  -- 'tq' keeps items to be processed by the worker.
  tqWA :: STM.TQueue (WalletAction a) <- liftIO STM.newTQueueIO
  -- 'tvOpen' is 'True' as long as 'tqWA' can receive new input.
  tvOpen :: STM.TVar Bool <- liftIO (STM.newTVarIO True)
  -- 'getWA' returns the next action to be processed. This function blocks
  -- unless 'tvOpen' is 'False', in which case 'Nothing' is returned.
  let getWA :: STM (Maybe (WalletAction a))
      getWA = STM.tryReadTQueue tqWA >>= \case
         Just wa -> pure (Just wa)
         Nothing -> STM.readTVar tvOpen >>= \case
            False -> pure Nothing
            True  -> STM.retry
  -- 'pushWA' adds an action to be executed by the worker, in FIFO order. It
  -- will throw 'BlockedIndefinitelyOnSTM' if used after `k` returns.
  let pushWA :: WalletAction a -> STM ()
      pushWA = \wa -> do STM.check =<< STM.readTVar tvOpen
                         STM.writeTQueue tqWA wa
  me :: ThreadId <- liftIO myThreadId
  Ex.mask $ \restore -> do
     -- Exceptions in the worker thread are re-thrown from the current thread.
     tId <- liftIO $ forkFinally
        (walletWorker wai (STM.atomically getWA))
        (either (throwTo me) (const (putMVar mDone ())))
     -- 'cleanup' prevents new input, waits for the worker to finish processing
     -- any pending work, and finally kills the worker.
     let cleanup :: forall n. MonadIO n => n ()
         cleanup = liftIO $ Ex.finally
            (STM.atomically (STM.writeTVar tvOpen False) >> takeMVar mDone)
            (killThread tId)
     Ex.try (restore (k pushWA)) >>= \case
        Right b -> cleanup >> pure b
        Left (se :: Ex.SomeException) -> do
           -- 'se' has priority over exceptions from 'cleanup'.
           Ex.onException cleanup (Ex.throwM se)
           Ex.throwM se

-- | Check if this is the initial worker state.
isInitialState :: Eq b => WalletWorkerState b -> Bool
isInitialState = (== initialWorkerState)

-- | Check that the state invariants all hold.
isValidState :: WalletWorkerState b -> Bool
isValidState wws = case wws of
  Normal -> True
  PendingFork PendingForkState{..} ->
    _pendingRollbacks >= 0 &&
    length (_pendingBlocks) == _lengthPendingBlocks &&
    _lengthPendingBlocks <= _pendingRollbacks

-- | Check if this state represents a pending fork.
hasPendingFork :: WalletWorkerState b -> Bool
hasPendingFork (PendingFork _) = True
hasPendingFork _               = False

instance Show b => Buildable (WalletWorkerState b) where
    build wws = case wws of
      Normal -> bprint "Normal"
      PendingFork PendingForkState{..} -> bprint
          ( "PendingForkState "
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
