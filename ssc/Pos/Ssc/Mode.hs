{-# LANGUAGE DataKinds #-}

module Pos.Ssc.Mode
       ( SscMode
       ) where

import           Universum

import           Control.Monad.Catch         (MonadMask)
import           EtherCompat
import           Mockable                    (MonadMockable)
import           System.Wlog                 (WithLogger)

import           Pos.Communication.PeerState (WithPeerState)
import           Pos.Core                    (MonadPrimaryKey)
import           Pos.DB.Class                (MonadDB, MonadGState)
import           Pos.Discovery               (MonadDiscovery)
import           Pos.Lrc.Context             (LrcContext)
import           Pos.Recovery.Info           (MonadRecoveryInfo)
import           Pos.Reporting               (MonadReportingMem)
import           Pos.Security.Params         (SecurityParams)
import           Pos.Shutdown                (MonadShutdownMem)
import           Pos.Slotting                (MonadSlots)
import           Pos.Ssc.Class.Types         (HasSscContext)
import           Pos.Ssc.Extra               (MonadSscMem)
import           Pos.Util.TimeWarp           (CanJsonLog)

-- | Mode used for all SSC listeners, workers, and the like.
type SscMode ssc ctx m
    = ( WithLogger m
      , CanJsonLog m
      , MonadIO m
      , MonadMask m
      , MonadMockable m
      , MonadSlots m
      , MonadGState m
      , MonadDB m
      , MonadSscMem ssc ctx m
      , MonadPrimaryKey ctx m
      , MonadRecoveryInfo m
      , MonadReportingMem ctx m
      , MonadShutdownMem ctx m
      , MonadDiscovery m
      , WithPeerState m
      , MonadReader ctx m
      , HasSscContext ssc ctx
      , HasLens SecurityParams ctx SecurityParams
      , HasLens LrcContext ctx LrcContext
      )
