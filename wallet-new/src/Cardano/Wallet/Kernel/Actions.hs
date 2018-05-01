
module Cardano.Wallet.Kernel.Actions
  ( WalletActions
  , WalletAction(..)
  , WalletActionInterp(..)
  , forkWalletWorker
  , walletWorker
  , transducer
  ) where

import           Universum hiding (State, execStateT, runState, evalState)
import           Control.Concurrent.Async (async, link)
import           Control.Concurrent.Chan
import           Control.Lens (makeLenses, (%=), (.=), (+=), (-=), (<>=))
import           Control.Monad.State.Lazy hiding (sequence_)
import           Control.Monad.Writer (runWriter, tell, MonadWriter)

import           Pos.Block.Types
import           Pos.Util.Chrono

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
  | FindMyUtxos
  | SystemReset
  | LogMessage Text

-- | Interface abstraction for the wallet worker.
--   The caller provides these primitive wallet operations;
--   the worker uses these to invoke changes to the
--   underlying wallet.
data WalletActionInterp m b = WalletActionInterp
  { applyBlocks :: OldestFirst NE b -> m
  , findUtxos :: m
  , switchToFork :: Int -> OldestFirst [] b -> m
  , emit :: Text -> m
  }

-- | A channel for communicating with a wallet worker.
type WalletActions = Chan (WalletAction Blund)

-- | Internal state of the wallet worker.
data WalletWorkerState b = WalletWorkerState
  { _pendingRollbacks    :: !Int
  , _pendingBlocks       :: !(NewestFirst [] b)
  , _lengthPendingBlocks :: !Int
  }

makeLenses ''WalletWorkerState

asWriter :: (Monoid m, MonadWriter m n) => WalletActionInterp m b -> WalletActionInterp (n ()) b
asWriter i = WalletActionInterp
  { applyBlocks  = tell . applyBlocks i
  , findUtxos    = tell (findUtxos i)
  , switchToFork = \n bs -> tell (switchToFork i n bs)
  , emit         = tell . emit i
  }

-- | `interp` is the main interpreter for turning a wallet action into a concrete
--   transformer for the wallet worker's state.
--
--   `interp` can be thought of as defining a finite-state transducer.
--   The inputs to the transducer are actions of typ `WalletAction b`,
--   the internal state of the transducer is represented by `WalletWorkerState b`,
--   and the transducer outputs a value of type `m` in response to each input.
--
--   For simplicity, the transitions are described using the `State` monad, augmented
--   with a primitive "emit a value x :: m". The `Monoid` instance is used to combine
--   several `m`s emitted during a single operation.

interp :: Monoid m => WalletActionInterp m b -> WalletAction b -> State (WalletWorkerState b) m
interp walletInterp action = runInterp $ do

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
      pendingBlocks <>= toNewestFirst (toListChrono bs)
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

    SystemReset -> emit "==================================================="

    LogMessage txt -> emit txt

    FindMyUtxos -> findUtxos

 where
   WalletActionInterp{..} = asWriter walletInterp
   runInterp = state . fmap (swap . runWriter) . execStateT

-- | Lazily convert a list of actions to interpreted wallet operations.
transducer :: Monoid m => WalletActionInterp m b -> [WalletAction b] -> [m]
transducer ops actions = evalState (mapM (interp ops) actions) initialState
  where
    initialState = WalletWorkerState
                  { _pendingRollbacks    = 0
                  , _pendingBlocks       = NewestFirst []
                  , _lengthPendingBlocks = 0
                  }

-- | Connect a wallet interpreter to a channel of actions.
walletWorker :: Chan (WalletAction b) -> WalletActionInterp (IO ()) b -> IO ()
walletWorker chan ops = do
  emit ops "Starting wallet worker."
  actions <- getChanContents chan
  sequence_ (transducer ops actions)
  emit ops "Finishing wallet worker."

-- | Start up a wallet worker; the worker will respond to actions issued over the
--   returned channel.
forkWalletWorker :: (MonadIO m, MonadIO m') => WalletActionInterp (IO ()) b -> m (WalletAction b -> m' ())
forkWalletWorker ops = liftIO $ do
  c <- newChan
  link =<< async (walletWorker c ops)
  return (liftIO . writeChan c)
             
