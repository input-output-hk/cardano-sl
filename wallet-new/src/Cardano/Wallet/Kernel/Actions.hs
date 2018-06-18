module Cardano.Wallet.Kernel.Actions
    ( WalletAction(..)
    , WalletActionInterp(..)
    , forkWalletWorker
    , walletWorker
    , interp
    , interpList
    , WalletWorkerState
    , isInitialState
    , hasPendingFork
    , isValidState
    ) where

import           Control.Monad.Morph (MFunctor(hoist))
import           Control.Concurrent.Async (async, link)
import           Control.Concurrent.Chan
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
interp :: forall m b.  Monad m
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
-- never returns.
walletWorker :: IO (WalletAction b) -> WalletActionInterp IO b -> IO Void
walletWorker getWA ops = do
    emit ops "Starting wallet worker."
    evalStateT (forever (interp ops =<< lift getWA)) initialWorkerState

-- | Connect a wallet action interpreter to a stream of actions.
interpList :: Monad m => WalletActionInterp m b -> [WalletAction b] -> m (WalletWorkerState b)
interpList ops actions = execStateT (forM_ actions $ interp ops) initialWorkerState

initialWorkerState :: WalletWorkerState b
initialWorkerState = Normal

-- | Start a wallet worker, who will react to 'WalletAction's submited to the
-- returned function.
forkWalletWorker
  :: (MonadIO m, MonadIO n)
  => WalletActionInterp IO b
  -> m (WalletAction b -> n ())
forkWalletWorker ops = liftIO $ do
    c <- newChan
    link =<< async (walletWorker (readChan c) ops)
    return (liftIO . writeChan c)

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
