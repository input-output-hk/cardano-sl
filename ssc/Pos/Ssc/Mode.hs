{-# LANGUAGE DataKinds #-}

module Pos.Ssc.Mode
       ( SscMode
       ) where

import           Universum

import           Control.Monad.Catch         (MonadMask)
import qualified Ether
import           Mockable                    (MonadMockable)
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
      , MonadMockable m
      , MonadSlots m
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
