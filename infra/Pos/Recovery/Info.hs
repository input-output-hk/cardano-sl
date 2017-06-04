module Pos.Recovery.Info
       ( MonadRecoveryInfo(..)
       , recoveryCommGuard
       ) where

import           Universum

import           Pos.Communication.Types.Protocol (ActionSpec (..), OutSpecs, WorkerSpec)

class Monad m => MonadRecoveryInfo m where
    -- | Returns if 'RecoveryHeader' is 'Just' (which is equivalent to “we're
    -- doing recovery”).
    recoveryInProgress :: m Bool

-- | This function is a helper for workers. It doesn't run a worker if the
-- node is in recovery mode.
recoveryCommGuard
    :: MonadRecoveryInfo m
    => (WorkerSpec m, OutSpecs) -> (WorkerSpec m, OutSpecs)
recoveryCommGuard (ActionSpec worker, outs) =
    (,outs) . ActionSpec $ \vI sA -> unlessM recoveryInProgress $ worker vI sA
