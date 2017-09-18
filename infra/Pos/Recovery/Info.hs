-- | The main goal of this module is to encapsulate recovery mechanism
-- and provide helpers related to it.

module Pos.Recovery.Info
       ( MonadRecoveryInfo(..)
       , recoveryCommGuard
       ) where

import           System.Wlog (WithLogger, logDebug)
import           Universum

class Monad m => MonadRecoveryInfo m where
    -- | Returns if 'RecoveryHeader' is 'Just' (which is equivalent to “we're
    -- doing recovery”).
    recoveryInProgress :: m Bool

-- | This is a helper function which runs given action only if we are
-- not doing recovery at this moment.  It is useful for workers which
-- shouldn't do anything while we are not synchronized.
recoveryCommGuard
    :: (MonadRecoveryInfo m, WithLogger m)
    => m () -> m ()
recoveryCommGuard action = ifM recoveryInProgress onIgnore action
  where
    onIgnore = logDebug "recoveryCommGuard: recovery in process, skipping action"
