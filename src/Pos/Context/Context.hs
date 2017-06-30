{-# LANGUAGE ExistentialQuantification #-}
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
       , GenesisStakes(..)
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

import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  (coerced, lens, makeLensesFor)
import           Data.Time.Clock               (UTCTime)
import qualified Ether
import           Ether.Internal                (HasLens (..))
import           System.Wlog                   (LoggerConfig)

import           Pos.Block.Core                (BlockHeader)
import           Pos.Block.RetrievalQueue      (BlockRetrievalQueue,
                                                BlockRetrievalQueueTag,
                                                MonadBlockRetrievalQueue)
import           Pos.Communication.Relay       (RelayPropagationQueue)
import           Pos.Communication.Relay.Types (RelayContext (..))
import           Pos.Communication.Types       (NodeId)
import           Pos.Core                      (GenesisStakes (..), HeaderHash,
                                                PrimaryKeyTag)
import           Pos.Crypto                    (SecretKey)
import           Pos.Discovery                 (DiscoveryContextSum)
import           Pos.ExecMode                  ((:::), modeContext)
import           Pos.Launcher.Param            (BaseParams (..), NodeParams (..))
import           Pos.Lrc.Context               (LrcContext)
import           Pos.Reporting.MemState        (ReportingContext (..), rcLoggingConfig,
                                                rcReportServers)
import           Pos.Security.Params           (SecurityParams)
import           Pos.Shutdown.Types            (ShutdownContext (..))
import           Pos.Slotting                  (SlottingContextSum, SlottingVar)
import           Pos.Ssc.Class.Types           (MonadSscContext, Ssc (SscNodeContext),
                                                SscContextTag)
import           Pos.Txp.Settings              (TxpGlobalSettings)
import           Pos.Txp.Toil.Types            (Utxo)
import           Pos.Update.Context            (UpdateContext)
import           Pos.Update.Params             (UpdateParams)
import           Pos.Util.UserSecret           (UserSecret)

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

data NodeContextTag
data LastKnownHeaderTag
type LastKnownHeader ssc = TVar (Maybe (BlockHeader ssc))
type MonadLastKnownHeader ssc = Ether.MonadReader LastKnownHeaderTag (LastKnownHeader ssc)

data ProgressHeaderTag
type ProgressHeader ssc = STM.TMVar (BlockHeader ssc)
type MonadProgressHeader ssc = Ether.MonadReader ProgressHeaderTag (ProgressHeader ssc)

data RecoveryHeaderTag
type RecoveryHeader ssc = STM.TMVar (NodeId, BlockHeader ssc)
type MonadRecoveryHeader ssc = Ether.MonadReader RecoveryHeaderTag (RecoveryHeader ssc)

newtype GenesisUtxo = GenesisUtxo { unGenesisUtxo :: Utxo }
newtype ConnectedPeers = ConnectedPeers { unConnectedPeers :: STM.TVar (Set NodeId) }
newtype BlkSemaphore = BlkSemaphore { unBlkSemaphore :: MVar HeaderHash }
newtype StartTime = StartTime { unStartTime :: UTCTime }

modeContext [d|
    -- NodeContext contains runtime context of node.
    data NodeContext ssc = NodeContext
        { ncSscContext          :: !(SscContextTag ::: SscNodeContext ssc)
        -- @georgeee please add documentation when you see this comment
        , ncUpdateContext       :: !(UpdateContext ::: UpdateContext)
        -- Context needed for the update system
        , ncLrcContext          :: !(LrcContext ::: LrcContext)
        -- Context needed for LRC
        , ncDiscoveryContext    :: !(DiscoveryContextSum ::: DiscoveryContextSum)
        -- Context needed for Discovery.
        , ncSlottingVar         :: !(SlottingVar ::: SlottingVar)
        -- Data necessary for 'MonadSlotsData'.
        , ncSlottingContext     :: !(SlottingContextSum ::: SlottingContextSum)
        -- Context needed for Slotting.
        , ncShutdownContext     :: !(ShutdownContext ::: ShutdownContext)
        -- Context needed for Shutdown
        , ncBlkSemaphore        :: !(BlkSemaphore ::: BlkSemaphore)
        -- Semaphore which manages access to block application.
        -- Stored hash is a hash of last applied block.
        , ncUserSecret          :: !(TVar UserSecret ::: TVar UserSecret)
        -- Secret keys (and path to file) which are used to send transactions
        , ncBlockRetrievalQueue :: !(BlockRetrievalQueueTag ::: BlockRetrievalQueue ssc)
        -- Concurrent queue that holds block headers that are to be
        -- downloaded.
        , ncRecoveryHeader      :: !(RecoveryHeaderTag ::: RecoveryHeader ssc)
        -- In case of recovery mode this variable holds the latest header hash
        -- we know about, and the node we're talking to, so we can do chained
        -- block requests. Invariant: this mvar is full iff we're more than
        -- 'recoveryHeadersMessage' blocks deep relatively to some valid header
        -- and we're downloading blocks. Every time we get block that's more
        -- difficult than this one, we overwrite. Every time we process some
        -- blocks and fail or see that we've downloaded this header, we clean
        -- mvar.
        , ncProgressHeader      :: !(ProgressHeaderTag ::: ProgressHeader ssc)
        -- Header of the last block that was downloaded in retrieving
        -- queue. Is needed to show smooth prorgess on the frontend.
        , ncInvPropagationQueue :: !(RelayPropagationQueue ::: RelayPropagationQueue)
        -- Queue is used in Relay framework,
        -- it stores inv messages for earlier received data.
        , ncLoggerConfig        :: !(LoggerConfig ::: LoggerConfig)
        -- Logger config, as taken/read from CLI.
        , ncNodeParams          :: !(NodeParams ::: NodeParams)
        -- Params node is launched with
        , ncStartTime           :: !(StartTime ::: StartTime)
        -- Time when node was started ('NodeContext' initialized).
        , ncLastKnownHeader     :: !(LastKnownHeaderTag ::: LastKnownHeader ssc)
        -- Header of last known block, generated by network (announcement of
        -- which reached us). Should be use only for informational purposes
        -- (status in Daedalus). It's easy to falsify this value.
        , ncTxpGlobalSettings   :: !(TxpGlobalSettings ::: TxpGlobalSettings)
        -- Settings for global Txp.
        , ncConnectedPeers      :: !(ConnectedPeers ::: ConnectedPeers)
        -- Set of peers that we're connected to.
        }
    |]

type MonadNodeContext ssc = Ether.MonadReader NodeContextTag (NodeContext ssc)

makeLensesFor
    [ ("npUpdateParams", "npUpdateParamsL")
    , ("npSecurityParams", "npSecurityParamsL")
    , ("npSecretKey", "npSecretKeyL")
    , ("npReportServers", "npReportServersL")
    , ("npPropagation", "npPropagationL")
    , ("npCustomUtxo", "npCustomUtxoL")
    , ("npGenesisStakes", "npGenesisStakesL") ]
    ''NodeParams

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

instance HasLens GenesisStakes (NodeContext ssc) GenesisStakes where
    lensOf = lensOf @NodeParams . npGenesisStakesL . coerced

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
