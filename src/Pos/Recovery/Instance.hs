module Pos.Recovery.Instance
       (
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           EtherCompat

import           Pos.Context.Context    (RecoveryHeader, RecoveryHeaderTag)
import           Pos.Recovery.Info      (MonadRecoveryInfo (..))

instance
    ( Monad m, MonadIO m
    , MonadCtx ctx RecoveryHeaderTag (RecoveryHeader ssc) m
    ) => MonadRecoveryInfo m where
    recoveryInProgress = do
        var <- askCtx @RecoveryHeaderTag
        isJust <$> atomically (STM.tryReadTMVar var)
