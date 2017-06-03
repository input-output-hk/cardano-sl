{-# LANGUAGE DataKinds #-}

module Pos.Ssc.Mode
       ( SscMode
       ) where

import           Universum

import           Control.Monad.Catch         (MonadMask)
import qualified Ether
import           Mockable                    (Mockable, ThreadId)
import qualified Mockable                    as M
import           System.Wlog                 (WithLogger)

import           Pos.Communication.PeerState (WithPeerState)
import           Pos.Core                    (MonadPrimaryKey)
import           Pos.DB.Class                (MonadDBCore, MonadDBPure)
import           Pos.Discovery               (MonadDiscovery)
import           Pos.Lrc.Context             (LrcContext)
import           Pos.Recovery.Info           (MonadRecoveryInfo)
import           Pos.Reporting               (MonadReportingMem)
import           Pos.Security.Params         (SecurityParams)
import           Pos.Shutdown                (MonadShutdownMem)
import           Pos.Slotting                (MonadSlots)
import           Pos.Ssc.Class.Types         (MonadSscContext)
import           Pos.Ssc.Extra               (MonadSscMem)

-- | Mode used for all SSC listeners, workers, and the like.
type SscMode ssc m
    = ( WithLogger m
      , MonadIO m
      , MonadMask m
      , MonadSlots m
      , Mockable M.CurrentTime m      -- TODO: if I use Mockables, GHC fails
      , Mockable M.Delay m
      , Mockable M.SharedAtomic m
      , Mockable M.SharedExclusive m
      , Mockable M.Fork m
      , Mockable M.Concurrently m
      , Mockable M.Async m
      , Mockable M.Bracket m
      , Mockable M.Channel m
      , Mockable M.Throw m
      , Mockable M.Catch m
      , Mockable M.Metrics m
      , Ord (ThreadId m)              -- TODO: Why are these needed?
      , Show (ThreadId m)
      , MonadDBCore m
      , MonadDBPure m
      , MonadSscMem ssc m
      , MonadPrimaryKey m
      , MonadRecoveryInfo m
      , MonadReportingMem m
      , MonadShutdownMem m
      , MonadDiscovery m
      , WithPeerState m
      , MonadSscContext ssc m
      , Ether.MonadReader' SecurityParams m
      , Ether.MonadReader' LrcContext m
      )
