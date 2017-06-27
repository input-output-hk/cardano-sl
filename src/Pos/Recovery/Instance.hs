module Pos.Recovery.Instance
       (
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Ether.Internal         (HasLens (..))

import           Pos.Context.Context    (RecoveryHeader, RecoveryHeaderTag)
import           Pos.Recovery.Info      (MonadRecoveryInfo (..))

instance
    ( Monad m, MonadIO m, MonadReader ctx m
    , HasLens RecoveryHeaderTag ctx (RecoveryHeader ssc)
    ) => MonadRecoveryInfo m where
    recoveryInProgress = do
        var <- view (lensOf @RecoveryHeaderTag)
        isJust <$> atomically (STM.tryReadTMVar var)
