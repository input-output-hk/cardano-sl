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
       , GenesisUtxo(..)
       , GenesisStakeholders(..)
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
       , BlkSemaphore(..)
       ) where

import           Universum

import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  (lens, makeLensesWith)
import           Data.Time.Clock               (UTCTime)
import           Ether.Internal                (HasLens (..))
import           System.Wlog                   (LoggerConfig)

import           Pos.Block.Core                (BlockHeader)
import           Pos.Block.RetrievalQueue      (BlockRetrievalQueue,
                                                BlockRetrievalQueueTag)
import           Pos.Block.Slog.Types          (HasSlogContext (..), SlogContext (..))
import           Pos.Communication.Types       (NodeId)
import           Pos.Core                      (GenesisStakeholders (..),
                                                HasPrimaryKey (..), HeaderHash, Timestamp)
import           Pos.Discovery                 (DiscoveryContextSum,
                                                HasDiscoveryContextSum (..))
import           Pos.Launcher.Param            (BaseParams (..), NodeParams (..),
                                                RelayParams (..))
import           Pos.Lrc.Context               (LrcContext)
import           Pos.Network.Types             (NetworkConfig (..))
import           Pos.Reporting.MemState        (HasLoggerConfig (..),
                                                HasReportServers (..),
                                                HasReportingContext (..),
                                                ReportingContext (..))
import           Pos.Security.Params           (SecurityParams)
import           Pos.Shutdown                  (HasShutdownContext (..),
                                                ShutdownContext (..))
import           Pos.Slotting                  (HasSlottingVar (..), SlottingContextSum,
                                                SlottingData)
import           Pos.Ssc.Class.Types           (HasSscContext (..), Ssc (SscNodeContext))
import           Pos.Txp.Settings              (TxpGlobalSettings)
import           Pos.Txp.Toil.Types            (GenesisUtxo (..))
import           Pos.Update.Context            (UpdateContext)
import           Pos.Update.Params             (UpdateParams)
import           Pos.Util.UserSecret           (HasUserSecret (..), UserSecret)
import           Pos.Util.Util                 (postfixLFields)

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

data LastKnownHeaderTag
type LastKnownHeader ssc = TVar (Maybe (BlockHeader ssc))
type MonadLastKnownHeader ssc ctx m
     = (MonadReader ctx m, HasLens LastKnownHeaderTag ctx (LastKnownHeader ssc))

data ProgressHeaderTag
type ProgressHeader ssc = STM.TMVar (BlockHeader ssc)
type MonadProgressHeader ssc ctx m
     = (MonadReader ctx m, HasLens ProgressHeaderTag ctx (ProgressHeader ssc))

data RecoveryHeaderTag
type RecoveryHeader ssc = STM.TMVar (NodeId, BlockHeader ssc)
type MonadRecoveryHeader ssc ctx m
     = (MonadReader ctx m, HasLens RecoveryHeaderTag ctx (RecoveryHeader ssc))

newtype ConnectedPeers = ConnectedPeers { unConnectedPeers :: STM.TVar (Set NodeId) }
newtype BlkSemaphore = BlkSemaphore { unBlkSemaphore :: MVar HeaderHash }
newtype StartTime = StartTime { unStartTime :: UTCTime }

data SscContextTag

-- | NodeContext contains runtime context of node.
data NodeContext ssc = NodeContext
    { ncSscContext          :: !(SscNodeContext ssc)
    -- @georgeee please add documentation when you see this comment
    , ncUpdateContext       :: !UpdateContext
    -- ^ Context needed for the update system
    , ncLrcContext          :: !LrcContext
    -- ^ Context needed for LRC
    , ncDiscoveryContext    :: !DiscoveryContextSum
    -- ^ Context needed for Discovery.
    , ncSlottingVar         :: !(Timestamp, TVar SlottingData)
    -- ^ Data necessary for 'MonadSlotsData'.
    , ncSlottingContext     :: !SlottingContextSum
    -- ^ Context needed for Slotting.
    , ncShutdownContext     :: !ShutdownContext
    -- ^ Context needed for Shutdown
    , ncSlogContext         :: !SlogContext
    -- ^ Context needed for Slog.
    , ncBlkSemaphore        :: !BlkSemaphore
    -- ^ Semaphore which manages access to block application.
    -- Stored hash is a hash of last applied block.
    , ncUserSecret          :: !(TVar UserSecret)
    -- ^ Secret keys (and path to file) which are used to send transactions
    , ncBlockRetrievalQueue :: !(BlockRetrievalQueue ssc)
    -- ^ Concurrent queue that holds block headers that are to be
    -- downloaded.
    , ncRecoveryHeader      :: !(RecoveryHeader ssc)
    -- ^ In case of recovery mode this variable holds the latest header hash
    -- we know about, and the node we're talking to, so we can do chained
    -- block requests. Invariant: this mvar is full iff we're in recovery mode
    -- and downloading blocks. Every time we get block that's more
    -- difficult than this one, we overwrite. Every time we process some
    -- blocks and fail or see that we've downloaded this header, we clean
    -- mvar.
    , ncLastKnownHeader     :: !(LastKnownHeader ssc)
    -- ^ Header of last known block, generated by network (announcement of
    -- which reached us). Should be use only for informational purposes
    -- (status in Daedalus). It's easy to falsify this value.
    , ncProgressHeader      :: !(ProgressHeader ssc)
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
    , ncNetworkConfig       :: !NetworkConfig
    }

makeLensesWith postfixLFields ''NodeContext

class HasNodeContext ssc ctx | ctx -> ssc where
    nodeContext :: Lens' ctx (NodeContext ssc)

instance HasNodeContext ssc (NodeContext ssc) where
    nodeContext = identity

instance HasSscContext ssc (NodeContext ssc) where
    sscContext = ncSscContext_L

instance HasDiscoveryContextSum (NodeContext ssc) where
    discoveryContextSum = ncDiscoveryContext_L

instance HasSlottingVar (NodeContext ssc) where
    slottingTimestamp = ncSlottingVar_L . _1
    slottingVar = ncSlottingVar_L . _2

instance HasSlogContext (NodeContext ssc) where
    slogContextL = ncSlogContext_L

instance HasLens SlottingContextSum (NodeContext ssc) SlottingContextSum where
    lensOf = ncSlottingContext_L

instance HasLens ProgressHeaderTag (NodeContext ssc) (ProgressHeader ssc) where
    lensOf = ncProgressHeader_L

instance HasLens BlkSemaphore (NodeContext ssc) BlkSemaphore where
    lensOf = ncBlkSemaphore_L

instance HasLens LastKnownHeaderTag (NodeContext ssc) (LastKnownHeader ssc) where
    lensOf = ncLastKnownHeader_L

instance HasShutdownContext (NodeContext ssc) where
    shutdownContext = ncShutdownContext_L

instance HasLens UpdateContext (NodeContext ssc) UpdateContext where
    lensOf = ncUpdateContext_L

instance HasUserSecret (NodeContext ssc) where
    userSecret = ncUserSecret_L

instance HasLens RecoveryHeaderTag (NodeContext ssc) (RecoveryHeader ssc) where
    lensOf = ncRecoveryHeader_L

instance HasLens ConnectedPeers (NodeContext ssc) ConnectedPeers where
    lensOf = ncConnectedPeers_L

instance HasLens BlockRetrievalQueueTag (NodeContext ssc) (BlockRetrievalQueue ssc) where
    lensOf = ncBlockRetrievalQueue_L

instance HasLens StartTime (NodeContext ssc) StartTime where
    lensOf = ncStartTime_L

instance HasLens LrcContext (NodeContext ssc) LrcContext where
    lensOf = ncLrcContext_L

instance HasLens TxpGlobalSettings (NodeContext ssc) TxpGlobalSettings where
    lensOf = ncTxpGlobalSettings_L

instance HasLens UpdateParams (NodeContext ssc) UpdateParams where
    lensOf = ncNodeParams_L . lensOf @UpdateParams

instance HasLens SecurityParams (NodeContext ssc) SecurityParams where
    lensOf = ncNodeParams_L . lensOf @SecurityParams

instance HasLens GenesisUtxo (NodeContext ssc) GenesisUtxo where
    lensOf = ncNodeParams_L . lensOf @GenesisUtxo

instance HasReportServers (NodeContext ssc) where
    reportServers = ncNodeParams_L . reportServers

instance HasLoggerConfig (NodeContext ssc) where
    loggerConfig = ncLoggerConfig_L

instance HasPrimaryKey (NodeContext ssc) where
    primaryKey = ncNodeParams_L . primaryKey

instance HasReportingContext (NodeContext ssc) where
    reportingContext = lens getter (flip setter)
      where
        getter nc =
            ReportingContext
                (nc ^. reportServers)
                (nc ^. loggerConfig)
        setter rc =
            set reportServers (rc ^. reportServers) .
            set loggerConfig  (rc ^. loggerConfig)

instance HasLens RelayParams (NodeContext scc) RelayParams where
    lensOf = ncNodeParams_L . lensOf @RelayParams
