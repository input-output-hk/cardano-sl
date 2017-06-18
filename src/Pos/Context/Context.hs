{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

-- | Runtime context of node.

module Pos.Context.Context
       ( NodeContextTag
       , MonadNodeContext
       , SscContextTag
       , MonadSscContext
       , NodeContext (..)
       , NodeParams(..)
       , BaseParams(..)
       , TxpGlobalSettings
       , GenesisUtxo(..)
       , GenesisLeaders(..)
       , StartTime(..)
       , LastKnownHeader
       , LastKnownHeaderTag
       , MonadLastKnownHeader
       , ProgressHeader
       , ProgressHeaderTag
       , MonadProgressHeader
       , BlockRetrievalQueueTag
       , BlockRetrievalQueue
       , MonadBlockRetrievalQueue
       , RecoveryHeaderTag
       , RecoveryHeader
       , MonadRecoveryHeader
       , ConnectedPeers(..)
       , BlkSemaphore(..)
       ) where

import           Universum

import           Control.Concurrent.STM        (TBQueue)
import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  (coerced, lens, makeLensesFor)
import           Data.Time.Clock               (UTCTime)
import qualified Ether
import           Ether.Internal                (HasLens (..))
import           Pos.Security.Params           (SecurityParams)
import           System.Wlog                   (LoggerConfig)

import           Pos.Block.Core                (BlockHeader)
import           Pos.Communication.Relay       (RelayPropagationQueue)
import           Pos.Communication.Relay.Types (RelayContext (..))
import           Pos.Communication.Types       (NodeId)
import           Pos.Core                      (HeaderHash, PrimaryKeyTag, SlotLeaders)
import           Pos.Crypto                    (SecretKey)
import           Pos.Discovery                 (DiscoveryContextSum)
import           Pos.Launcher.Param            (BaseParams (..), NodeParams (..))
import           Pos.Lrc.Context               (LrcContext)
import           Pos.Reporting.MemState        (ReportingContext (..), rcLoggingConfig,
                                                rcReportServers)
import           Pos.Shutdown.Types            (ShutdownContext (..))
import           Pos.Slotting                  (SlottingContextSum, SlottingVar)
import           Pos.Ssc.Class.Types           (MonadSscContext, Ssc (SscNodeContext),
                                                SscContextTag)
import           Pos.Txp.Settings              (TxpGlobalSettings)
import           Pos.Txp.Toil.Types            (Utxo)
import           Pos.Update.Context            (UpdateContext)
import           Pos.Update.Params             (UpdateParams)
import           Pos.Util.Chrono               (NE, NewestFirst)
import           Pos.Util.UserSecret           (UserSecret)

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

data NodeContextTag
type MonadNodeContext ssc = Ether.MonadReader NodeContextTag (NodeContext ssc)

data LastKnownHeaderTag
type LastKnownHeader ssc = TVar (Maybe (BlockHeader ssc))
type MonadLastKnownHeader ssc = Ether.MonadReader LastKnownHeaderTag (LastKnownHeader ssc)

data ProgressHeaderTag
type ProgressHeader ssc = STM.TMVar (BlockHeader ssc)
type MonadProgressHeader ssc = Ether.MonadReader ProgressHeaderTag (ProgressHeader ssc)

data BlockRetrievalQueueTag
type BlockRetrievalQueue ssc = TBQueue (NodeId, NewestFirst NE (BlockHeader ssc))
type MonadBlockRetrievalQueue ssc =
    Ether.MonadReader BlockRetrievalQueueTag (BlockRetrievalQueue ssc)

data RecoveryHeaderTag
type RecoveryHeader ssc = STM.TMVar (NodeId, BlockHeader ssc)
type MonadRecoveryHeader ssc = Ether.MonadReader RecoveryHeaderTag (RecoveryHeader ssc)

newtype GenesisUtxo = GenesisUtxo { unGenesisUtxo :: Utxo }
newtype GenesisLeaders = GenesisLeaders { unGenesisLeaders :: SlotLeaders }
newtype ConnectedPeers = ConnectedPeers { unConnectedPeers :: STM.TVar (Set NodeId) }
newtype BlkSemaphore = BlkSemaphore { unBlkSemaphore :: MVar HeaderHash }
newtype StartTime = StartTime { unStartTime :: UTCTime }


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
    , ncSlottingVar         :: !SlottingVar
    -- ^ Data necessary for 'MonadSlotsData'.
    , ncSlottingContext     :: !SlottingContextSum
    -- ^ Context needed for Slotting.
    , ncShutdownContext     :: !ShutdownContext
    -- ^ Context needed for Shutdown
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
    -- block requests. Invariant: this mvar is full iff we're more than
    -- 'recoveryHeadersMessage' blocks deep relatively to some valid header
    -- and we're downloading blocks. Every time we get block that's more
    -- difficult than this one, we overwrite. Every time we process some
    -- blocks and fail or see that we've downloaded this header, we clean
    -- mvar.
    , ncProgressHeader      :: !(ProgressHeader ssc)
    -- ^ Header of the last block that was downloaded in retrieving
    -- queue. Is needed to show smooth prorgess on the frontend.
    , ncInvPropagationQueue :: !RelayPropagationQueue
    -- ^ Queue is used in Relay framework,
    -- it stores inv messages for earlier received data.
    , ncLoggerConfig        :: !LoggerConfig
    -- ^ Logger config, as taken/read from CLI.
    , ncNodeParams          :: !NodeParams
    -- ^ Params node is launched with
    , ncSendLock            :: !(Maybe (MVar ()))
    -- ^ Exclusive lock for sending messages to other nodes
    -- (if Nothing, no lock used).
    , ncStartTime           :: !StartTime
    -- ^ Time when node was started ('NodeContext' initialized).
    , ncLastKnownHeader     :: !(LastKnownHeader ssc)
    -- ^ Header of last known block, generated by network (announcement of
    -- which reached us). Should be use only for informational purposes
    -- (status in Daedalus). It's easy to falsify this value.
    , ncTxpGlobalSettings   :: !TxpGlobalSettings
    -- ^ Settings for global Txp.
    , ncGenesisLeaders      :: !GenesisLeaders
    -- ^ Leaders of the first epoch
    , ncConnectedPeers      :: !ConnectedPeers
    -- ^ Set of peers that we're connected to.
    }

makeLensesFor
    [ ("ncUpdateContext", "ncUpdateContextL")
    , ("ncLrcContext", "ncLrcContextL")
    , ("ncDiscoveryContext", "ncDiscoveryContextL")
    , ("ncSlottingContext", "ncSlottingContextL")
    , ("ncSlottingVar", "ncSlottingVarL")
    , ("ncSscContext", "ncSscContextL")
    , ("ncNodeParams", "ncNodeParamsL")
    , ("ncInvPropagationQueue", "ncInvPropagationQueueL")
    , ("ncShutdownContext", "ncShutdownContextL")
    , ("ncLoggerConfig", "ncLoggerConfigL")
    , ("ncJLFile", "ncJLFileL")
    , ("ncUserSecret", "ncUserSecretL")
    , ("ncLastKnownHeader", "ncLastKnownHeaderL")
    , ("ncConnectedPeers", "ncConnectedPeersL")
    , ("ncProgressHeader", "ncProgressHeaderL")
    , ("ncBlockRetrievalQueue", "ncBlockRetrievalQueueL")
    , ("ncRecoveryHeader", "ncRecoveryHeaderL")
    , ("ncBlkSemaphore", "ncBlkSemaphoreL")
    , ("ncGenesisLeaders", "ncGenesisLeadersL")
    , ("ncStartTime", "ncStartTimeL")
    , ("ncTxpGlobalSettings", "ncTxpGlobalSettingsL")
    , ("ncShutdownNotifyQueue", "ncShutdownNotifyQueueL") ]
    ''NodeContext

makeLensesFor
    [ ("npUpdateParams", "npUpdateParamsL")
    , ("npSecurityParams", "npSecurityParamsL")
    , ("npSecretKey", "npSecretKeyL")
    , ("npReportServers", "npReportServersL")
    , ("npPropagation", "npPropagationL")
    , ("npCustomUtxo", "npCustomUtxoL") ]
    ''NodeParams

instance r ~ SscNodeContext ssc => HasLens SscContextTag (NodeContext ssc) r where
    lensOf = ncSscContextL

instance HasLens UpdateContext (NodeContext ssc) UpdateContext where
    lensOf = ncUpdateContextL

instance HasLens LrcContext (NodeContext ssc) LrcContext where
    lensOf = ncLrcContextL

instance HasLens DiscoveryContextSum (NodeContext ssc) DiscoveryContextSum where
    lensOf = ncDiscoveryContextL

instance HasLens SlottingVar (NodeContext ssc) SlottingVar where
    lensOf = ncSlottingVarL

instance HasLens SlottingContextSum (NodeContext ssc) SlottingContextSum where
    lensOf = ncSlottingContextL

instance HasLens ShutdownContext (NodeContext ssc) ShutdownContext where
    lensOf = ncShutdownContextL

instance HasLens NodeParams (NodeContext ssc) NodeParams where
    lensOf = ncNodeParamsL

instance HasLens (TVar UserSecret) (NodeContext ssc) (TVar UserSecret) where
    lensOf = ncUserSecretL

instance HasLens LastKnownHeaderTag (NodeContext ssc) (LastKnownHeader ssc) where
    lensOf = ncLastKnownHeaderL

instance HasLens ConnectedPeers (NodeContext ssc) ConnectedPeers where
    lensOf = ncConnectedPeersL

instance HasLens ProgressHeaderTag (NodeContext ssc) (ProgressHeader ssc) where
    lensOf = ncProgressHeaderL

instance HasLens BlockRetrievalQueueTag (NodeContext ssc) (BlockRetrievalQueue ssc) where
    lensOf = ncBlockRetrievalQueueL

instance HasLens RecoveryHeaderTag (NodeContext ssc) (RecoveryHeader ssc) where
    lensOf = ncRecoveryHeaderL

instance HasLens BlkSemaphore (NodeContext ssc) BlkSemaphore where
    lensOf = ncBlkSemaphoreL

instance HasLens StartTime (NodeContext ssc) StartTime where
    lensOf = ncStartTimeL

instance HasLens TxpGlobalSettings (NodeContext ssc) TxpGlobalSettings where
    lensOf = ncTxpGlobalSettingsL

instance HasLens LoggerConfig (NodeContext ssc) LoggerConfig where
    lensOf = ncLoggerConfigL

instance HasLens GenesisLeaders (NodeContext ssc) GenesisLeaders where
    lensOf = ncGenesisLeadersL

instance HasLens RelayPropagationQueue (NodeContext ssc) RelayPropagationQueue where
    lensOf = ncInvPropagationQueueL

-- Aux instances

instance HasLens NodeContextTag (NodeContext ssc) (NodeContext ssc) where
    lensOf = identity

instance HasLens UpdateParams (NodeContext ssc) UpdateParams where
    lensOf = lensOf @NodeParams . npUpdateParamsL

instance HasLens SecurityParams (NodeContext ssc) SecurityParams where
    lensOf = lensOf @NodeParams . npSecurityParamsL

instance HasLens PrimaryKeyTag (NodeContext ssc) SecretKey where
    lensOf = lensOf @NodeParams . npSecretKeyL

instance HasLens GenesisUtxo (NodeContext ssc) GenesisUtxo where
    lensOf = lensOf @NodeParams . npCustomUtxoL . coerced

instance HasLens ReportingContext (NodeContext ssc) ReportingContext where
    lensOf = lens getter (flip setter)
      where
        getter nc =
            ReportingContext
                (nc ^. lensOf @NodeParams . npReportServersL)
                (nc ^. lensOf @LoggerConfig)
        setter rc =
            set (lensOf @NodeParams . npReportServersL) (rc ^. rcReportServers) .
            set (lensOf @LoggerConfig) (rc ^. rcLoggingConfig)

instance HasLens RelayContext (NodeContext ssc) RelayContext where
    lensOf = lens getter (flip setter)
      where
        getter nc =
            RelayContext
                (nc ^. lensOf @NodeParams . npPropagationL)
                (nc ^. lensOf @RelayPropagationQueue)
        setter rc =
            set (lensOf @NodeParams . npPropagationL) (_rlyIsPropagation rc) .
            set (lensOf @RelayPropagationQueue) (_rlyPropagationQueue rc)
