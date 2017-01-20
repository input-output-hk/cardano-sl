-- | Proxy transformer for tracking updates for rollback

module Pos.Update.Poll.RollTrans
       ( RollT (..)
       , runRollT
       , execRollT
       ) where

import           Control.Monad.Except      (MonadError (..))
import           Data.Default              (def)
import           Universum

import           Control.Monad.Trans.Class (MonadTrans)
import           Pos.Update.Poll.Class     (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Types     (USUndo (..))

newtype RollT m a = RollT
    { getRollT :: StateT USUndo m a
    } deriving (Functor, Applicative, Monad, MonadThrow, MonadTrans, MonadError e)

instance MonadPollRead m => MonadPollRead (RollT m)

-- data USUndo = USUndo
--     { unCreatedNewDeps    :: !(Maybe (ProtocolVersion, ScriptVersion))
--     , unLastAdoptedPV     :: !(Maybe (PrevValue ProtocolVersion))
--     , unChangedProps      :: !(HashMap UpId (PrevValue ProposalState))
--     , unChangedSV         :: !(HashMap ApplicationName (PrevValue SoftwareVersion))
--     }

instance MonadPollRead m => MonadPoll (RollT m) where
    addScriptVersionDep = notImplemented
    delScriptVersionDep  = notImplemented
    setLastAdoptedPV = notImplemented
    setLastConfirmedSV = notImplemented
    delConfirmedSV = notImplemented
    addActiveProposal = notImplemented
    deactivateProposal = notImplemented

runRollT :: RollT m a -> m (a, USUndo)
runRollT = flip runStateT def . getRollT

execRollT :: Monad m => RollT m a -> m USUndo
execRollT = flip execStateT def . getRollT

