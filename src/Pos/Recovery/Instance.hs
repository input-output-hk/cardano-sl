module Pos.Recovery.Instance
       (
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import qualified Ether

import           Pos.Context.Context    (RecoveryHeader, RecoveryHeaderTag)
import           Pos.Recovery.Info      (MonadRecoveryInfo (..))

instance
    ( Monad m, MonadIO m
    , Ether.MonadReader RecoveryHeaderTag (RecoveryHeader ssc) m
    ) => MonadRecoveryInfo m where
    recoveryInProgress = do
        var <- Ether.ask @RecoveryHeaderTag
        isJust <$> atomically (STM.tryReadTMVar var)
