module Pos.Recovery.Info
       ( MonadRecoveryInfo(..)
       , recoveryCommGuard
       , recoveryCommGuardSimple
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
recoveryCommGuard = first recoveryCommGuardSimple

-- | This function is a helper for @gromak. It is another version of
-- 'recoveryCommGuard'.
recoveryCommGuardSimple
    :: MonadRecoveryInfo m
    => WorkerSpec m -> WorkerSpec m
recoveryCommGuardSimple (ActionSpec worker) =
    ActionSpec $ \vI sA -> unlessM recoveryInProgress $ worker vI sA
