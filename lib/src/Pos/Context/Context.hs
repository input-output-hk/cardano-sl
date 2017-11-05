{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS -fno-warn-unused-top-binds #-} -- for lenses

-- | Runtime context of node.

module Pos.Context.Context
       ( HasNodeContext(..)
       , HasSscContext(..)
       , NodeContext (..)
       , NodeParams(..)
       , BaseParams(..)
       , TxpGlobalSettings
       , StartTime(..)
       , LastKnownHeader
       , LastKnownHeaderTag
       , MonadLastKnownHeader
       , ProgressHeader
       , ProgressHeaderTag
       , MonadProgressHeader
       , BlockRetrievalQueueTag
       , BlockRetrievalQueue
       , RecoveryHeaderTag
       , RecoveryHeader
       , MonadRecoveryHeader
       , ConnectedPeers(..)
       ) where

import           Universum

import qualified Control.Concurrent.STM   as STM
import           Control.Lens             (lens, makeLensesWith)
import           Data.Time.Clock          (UTCTime)
import           Ether.Internal           (HasLens (..))
import           System.Wlog              (LoggerConfig)

import           Pos.Block.Core           (BlockHeader)
import           Pos.Block.RetrievalQueue (BlockRetrievalQueue, BlockRetrievalQueueTag)
import           Pos.Block.Slog.Types     (HasSlogContext (..), HasSlogGState (..),
                                           SlogContext (..))
import           Pos.Communication.Types  (NodeId)
import           Pos.Core                 (HasPrimaryKey (..), Timestamp)
import           Pos.DHT.Real.Types       (KademliaDHTInstance)
import           Pos.Launcher.Param       (BaseParams (..), NodeParams (..))
import           Pos.Lrc.Context          (LrcContext)
import           Pos.Network.Types        (NetworkConfig (..))
import           Pos.Reporting.MemState   (HasLoggerConfig (..), HasReportServers (..),
                                           HasReportingContext (..),
                                           ReportingContext (..))
import           Pos.Shutdown             (HasShutdownContext (..), ShutdownContext (..))
import           Pos.Slotting             (HasSlottingVar (..), SlottingContextSum,
                                           SlottingData)
import           Pos.Ssc.Types            (HasSscContext (..), SscContext)
import           Pos.StateLock            (StateLock, StateLockMetrics)
import           Pos.Txp.Settings         (TxpGlobalSettings)
import           Pos.Update.Context       (UpdateContext)
import           Pos.Util.Timer           (Timer)
import           Pos.Util.UserSecret      (HasUserSecret (..), UserSecret)
import           Pos.Util.Util            (postfixLFields)

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

data LastKnownHeaderTag
type LastKnownHeader = TVar (Maybe BlockHeader)
type MonadLastKnownHeader ctx m
     = (MonadReader ctx m, HasLens LastKnownHeaderTag ctx LastKnownHeader)

data ProgressHeaderTag
type ProgressHeader = STM.TMVar BlockHeader
type MonadProgressHeader ctx m
     = (MonadReader ctx m, HasLens ProgressHeaderTag ctx ProgressHeader)

data RecoveryHeaderTag
type RecoveryHeader = STM.TMVar (NodeId, BlockHeader)
type MonadRecoveryHeader ctx m
     = (MonadReader ctx m, HasLens RecoveryHeaderTag ctx RecoveryHeader)

newtype ConnectedPeers = ConnectedPeers { unConnectedPeers :: STM.TVar (Set NodeId) }
newtype StartTime = StartTime { unStartTime :: UTCTime }

data SscContextTag

-- | NodeContext contains runtime context of node.
data NodeContext = NodeContext
    { ncSscContext          :: !SscContext
    -- @georgeee please add documentation when you see this comment
    , ncUpdateContext       :: !UpdateContext
    -- ^ Context needed for the update system
    , ncLrcContext          :: !LrcContext
    -- ^ Context needed for LRC
    , ncSlottingVar         :: !(Timestamp, TVar SlottingData)
    -- ^ Data necessary for 'MonadSlotsData'.
    , ncSlottingContext     :: !SlottingContextSum
    -- ^ Context needed for Slotting.
    , ncShutdownContext     :: !ShutdownContext
    -- ^ Context needed for Shutdown
    , ncSlogContext         :: !SlogContext
    -- ^ Context needed for Slog.
    , ncStateLock           :: !StateLock
    -- ^ A lock which manages access to shared resources.
    -- Stored hash is a hash of last applied block.
    , ncStateLockMetrics    :: !StateLockMetrics
    -- ^ A set of callbacks for 'StateLock'.
    , ncUserSecret          :: !(TVar UserSecret)
    -- ^ Secret keys (and path to file) which are used to send transactions
    , ncBlockRetrievalQueue :: !BlockRetrievalQueue
    -- ^ Concurrent queue that holds block headers that are to be
    -- downloaded.
    , ncRecoveryHeader      :: !RecoveryHeader
    -- ^ In case of recovery mode this variable holds the latest header hash
    -- we know about, and the node we're talking to, so we can do chained
    -- block requests. Invariant: this mvar is full iff we're in recovery mode
    -- and downloading blocks. Every time we get block that's more
    -- difficult than this one, we overwrite. Every time we process some
    -- blocks and fail or see that we've downloaded this header, we clean
    -- mvar.
    , ncLastKnownHeader     :: !LastKnownHeader
    -- ^ Header of last known block, generated by network (announcement of
    -- which reached us). Should be use only for informational purposes
    -- (status in Daedalus). It's easy to falsify this value.
    , ncProgressHeader      :: !ProgressHeader
    -- ^ Header of the last block that was downloaded in retrieving
    -- queue. Is needed to show smooth prorgess on the frontend.
    , ncLoggerConfig        :: !LoggerConfig
    -- ^ Logger config, as taken/read from CLI.
    , ncNodeParams          :: !NodeParams
    -- ^ Params node is launched with
    , ncStartTime           :: !StartTime
    -- ^ Time when node was started ('NodeContext' initialized).
    , ncTxpGlobalSettings   :: !TxpGlobalSettings
    -- ^ Settings for global Txp.
    , ncConnectedPeers      :: !ConnectedPeers
    -- ^ Set of peers that we're connected to.
    , ncNetworkConfig       :: !(NetworkConfig KademliaDHTInstance)
    -- ^ Timer for delaying sending keep-alive like packets to relay nodes until
    -- a specific duration after the last time a block was received has passed.
    , ncSubscriptionKeepAliveTimer :: !Timer
    }

makeLensesWith postfixLFields ''NodeContext

class HasNodeContext ctx where
    nodeContext :: Lens' ctx NodeContext

instance HasNodeContext NodeContext where
    nodeContext = identity

instance HasSscContext NodeContext where
    sscContext = ncSscContext_L

instance HasSlottingVar NodeContext where
    slottingTimestamp = ncSlottingVar_L . _1
    slottingVar = ncSlottingVar_L . _2

instance HasSlogContext NodeContext where
    slogContext = ncSlogContext_L

instance HasSlogGState NodeContext where
    slogGState = ncSlogContext_L . slogGState

instance HasLens SlottingContextSum NodeContext SlottingContextSum where
    lensOf = ncSlottingContext_L

instance HasLens ProgressHeaderTag NodeContext ProgressHeader where
    lensOf = ncProgressHeader_L

instance HasLens StateLock NodeContext StateLock where
    lensOf = ncStateLock_L

instance HasLens StateLockMetrics NodeContext StateLockMetrics where
    lensOf = ncStateLockMetrics_L

instance HasLens LastKnownHeaderTag NodeContext LastKnownHeader where
    lensOf = ncLastKnownHeader_L

instance HasShutdownContext NodeContext where
    shutdownContext = ncShutdownContext_L

instance HasLens UpdateContext NodeContext UpdateContext where
    lensOf = ncUpdateContext_L

instance HasUserSecret NodeContext where
    userSecret = ncUserSecret_L

instance HasLens RecoveryHeaderTag NodeContext RecoveryHeader where
    lensOf = ncRecoveryHeader_L

instance HasLens ConnectedPeers NodeContext ConnectedPeers where
    lensOf = ncConnectedPeers_L

instance HasLens BlockRetrievalQueueTag NodeContext BlockRetrievalQueue where
    lensOf = ncBlockRetrievalQueue_L

instance HasLens StartTime NodeContext StartTime where
    lensOf = ncStartTime_L

instance HasLens LrcContext NodeContext LrcContext where
    lensOf = ncLrcContext_L

instance HasLens TxpGlobalSettings NodeContext TxpGlobalSettings where
    lensOf = ncTxpGlobalSettings_L

instance {-# OVERLAPPABLE #-}
    HasLens tag NodeParams r =>
    HasLens tag NodeContext r
  where
    lensOf = ncNodeParams_L . lensOf @tag

instance HasReportServers NodeContext where
    reportServers = ncNodeParams_L . reportServers

instance HasLoggerConfig NodeContext where
    loggerConfig = ncLoggerConfig_L

instance HasPrimaryKey NodeContext where
    primaryKey = ncNodeParams_L . primaryKey

instance HasReportingContext NodeContext where
    reportingContext = lens getter (flip setter)
      where
        getter nc =
            ReportingContext
                (nc ^. reportServers)
                (nc ^. loggerConfig)
        setter rc =
            set reportServers (rc ^. reportServers) .
            set loggerConfig  (rc ^. loggerConfig)

instance HasLens (NetworkConfig KademliaDHTInstance) NodeContext (NetworkConfig KademliaDHTInstance) where
    lensOf = ncNetworkConfig_L
