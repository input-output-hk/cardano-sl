{-# LANGUAGE DataKinds #-}

module Pos.Ssc.Mode
       ( SscMode
       ) where

import           Universum

import           Control.Monad.Catch (MonadMask)
import qualified Crypto.Random       as Rand
import           Ether.Internal      (HasLens (..))
import           Mockable            (MonadMockable)
import           System.Wlog         (WithLogger)

import           Pos.Core            (HasConfiguration, HasPrimaryKey)
import           Pos.DB.Class        (MonadDB, MonadGState)
import           Pos.Lrc.Context     (LrcContext)
import           Pos.Recovery.Info   (MonadRecoveryInfo)
import           Pos.Reporting       (MonadReporting)
import           Pos.Security.Params (SecurityParams)
import           Pos.Shutdown        (HasShutdownContext)
import           Pos.Slotting        (MonadSlots)
import           Pos.Ssc.Class.Types (HasSscContext)
import           Pos.Ssc.Extra       (MonadSscMem)
import           Pos.Util.TimeWarp   (CanJsonLog)

-- | Mode used for all SSC listeners, workers, and the like.
type SscMode ssc ctx m
    = ( WithLogger m
      , CanJsonLog m
      , MonadIO m
      , Rand.MonadRandom m
      , MonadMask m
      , MonadMockable m
      , MonadSlots ctx m
      , MonadGState m
      , MonadDB m
      , MonadSscMem ssc ctx m
      , MonadRecoveryInfo m
      , HasShutdownContext ctx
      , MonadReader ctx m
      , HasSscContext ssc ctx
      , MonadReporting ctx m
      , HasPrimaryKey ctx
      , HasLens SecurityParams ctx SecurityParams
      , HasLens LrcContext ctx LrcContext
      , HasConfiguration
      )
