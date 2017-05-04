{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}

-- | Runtime context of node.

module Pos.Context.Context
       ( NodeContextTag
       , NodeContext (..)
       , ncPublicKey
       , ncPubKeyAddress
       , ncSystemStart
       , NodeParams(..)
       , BaseParams(..)
       , GenesisUtxo(..)
       ) where

import           Control.Concurrent.STM        (TBQueue)
import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  (coerced, lens, makeLensesFor)
import           Data.Kind                     (Type)
import           Data.Time.Clock               (UTCTime)
import           Ether.Internal                (HList (..), HasLens (..), Tags, TagsK)
import           System.Wlog                   (LoggerConfig)
import           Universum

import           Pos.Communication.Relay       (RelayInvQueue)
import           Pos.Communication.Relay.Types (RelayContext (..))
import           Pos.Communication.Types       (NodeId)
import           Pos.Crypto                    (PublicKey, toPublic)
import           Pos.Launcher.Param            (BaseParams (..), NodeParams (..))
import           Pos.Lrc.Context               (LrcContext)
import           Pos.Reporting.MemState        (ReportingContext (..), rcLoggingConfig,
                                                rcReportServers)
import           Pos.Shutdown.Types            (ShutdownContext (..))
import           Pos.Ssc.Class.Types           (Ssc (SscNodeContext))
import           Pos.Txp.Settings              (TxpGlobalSettings)
import           Pos.Txp.Toil.Types            (Utxo)
import           Pos.Types                     (Address, BlockHeader, HeaderHash,
                                                SlotLeaders, Timestamp, makePubKeyAddress)
import           Pos.Update.Context            (UpdateContext)
import           Pos.Update.Params             (UpdateParams)
import           Pos.Util.Chrono               (NE, NewestFirst)
import           Pos.Util.JsonLog              (JLFile)
import           Pos.Util.UserSecret           (UserSecret)

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

data NodeContextTag

-- | NodeContext contains runtime context of node.
data NodeContext ssc = NodeContext
    { ncJLFile              :: !JLFile
    -- @georgeee please add documentation when you see this comment
    , ncSscContext          :: !(SscNodeContext ssc)
    -- @georgeee please add documentation when you see this comment
    , ncUpdateContext       :: !UpdateContext
    -- ^ Context needed for the update system
    , ncLrcContext          :: !LrcContext
    -- ^ Context needed for LRC
    , ncBlkSemaphore        :: !(MVar HeaderHash)
    -- ^ Semaphore which manages access to block application.
    -- Stored hash is a hash of last applied block.
    , ncUserSecret          :: !(TVar UserSecret)
    -- ^ Secret keys (and path to file) which are used to send transactions
    , ncBlockRetrievalQueue :: !(TBQueue (NodeId, NewestFirst NE (BlockHeader ssc)))
    -- ^ Concurrent queue that holds block headers that are to be
    -- downloaded.
    , ncRecoveryHeader      :: !(STM.TMVar (NodeId, BlockHeader ssc))
    -- ^ In case of recovery mode this variable holds the latest header hash
    -- we know about, and the node we're talking to, so we can do chained
    -- block requests. Invariant: this mvar is full iff we're more than
    -- 'recoveryHeadersMessage' blocks deep relatively to some valid header
    -- and we're downloading blocks. Every time we get block that's more
    -- difficult than this one, we overwrite. Every time we process some
    -- blocks and fail or see that we've downloaded this header, we clean
    -- mvar.
    , ncProgressHeader      :: !(STM.TMVar (BlockHeader ssc))
    -- ^ Header of the last block that was downloaded in retrieving
    -- queue. Is needed to show smooth prorgess on the frontend.
    , ncInvPropagationQueue :: !RelayInvQueue
    -- ^ Queue is used in Relay framework,
    -- it stores inv messages for earlier received data.
    , ncLoggerConfig        :: !LoggerConfig
    -- ^ Logger config, as taken/read from CLI.
    , ncNodeParams          :: !NodeParams
    -- ^ Params node is launched with
    , ncShutdownFlag        :: !(TVar Bool)
    -- ^ If this flag is `True`, then workers should stop.
    , ncShutdownNotifyQueue :: !(TBQueue ())
    -- ^ A queue which is used to count how many workers have successfully
    -- terminated.
    , ncSendLock            :: !(Maybe (MVar ()))
    -- ^ Exclusive lock for sending messages to other nodes
    -- (if Nothing, no lock used).
    , ncStartTime           :: !UTCTime
    -- ^ Time when node was started ('NodeContext' initialized).
    , ncLastKnownHeader     :: !(TVar (Maybe (BlockHeader ssc)))
    -- ^ Header of last known block, generated by network (announcement of
    -- which reached us). Should be use only for informational purposes
    -- (status in Daedalus). It's easy to falsify this value.
    , ncTxpGlobalSettings   :: !TxpGlobalSettings
    -- ^ Settings for global Txp.
    , ncGenesisLeaders      :: !SlotLeaders
    -- ^ Leaders of the first epoch
    , ncConnectedPeers      :: !(STM.TVar (Set NodeId))
    -- ^ Set of peers that we're connected to.
    }

newtype GenesisUtxo = GenesisUtxo
    { unGenesisUtxo :: Utxo
    }

makeLensesFor
  [ ("ncUpdateContext", "ncUpdateContextL")
  , ("ncLrcContext", "ncLrcContextL")
  , ("ncNodeParams", "ncNodeParamsL")
  , ("ncInvPropagationQueue", "ncInvPropagationQueueL")
  , ("ncShutdownFlag", "ncShutdownFlagL")
  , ("ncLoggerConfig", "ncLoggerConfigL")
  , ("ncJLFile", "ncJLFileL")
  , ("ncShutdownNotifyQueue", "ncShutdownNotifyQueueL") ]
  ''NodeContext

makeLensesFor
  [ ("npUpdateParams", "npUpdateParamsL")
  , ("npReportServers", "npReportServersL")
  , ("npPropagation", "npPropagationL")
  , ("npCustomUtxo", "npCustomUtxoL") ]
  ''NodeParams

type instance TagsK (NodeContext ssc) =
  '[Type, Type, Type, Type, Type, Type, Type, Type, Type, Type]

return []

type (:::) = 'HCons

infixr :::

type instance Tags (NodeContext ssc) =
  NodeContextTag :::
  UpdateContext :::
  LrcContext :::
  NodeParams :::
  UpdateParams :::
  ReportingContext :::
  RelayContext :::
  ShutdownContext :::
  JLFile :::
  GenesisUtxo :::
  'HNil

instance HasLens NodeContextTag (NodeContext ssc) (NodeContext ssc) where
    lensOf = identity

instance HasLens UpdateContext (NodeContext ssc) UpdateContext where
    lensOf = ncUpdateContextL

instance HasLens LrcContext (NodeContext ssc) LrcContext where
    lensOf = ncLrcContextL

instance HasLens NodeParams (NodeContext ssc) NodeParams where
    lensOf = ncNodeParamsL

instance HasLens UpdateParams (NodeContext ssc) UpdateParams where
    lensOf = ncNodeParamsL . npUpdateParamsL

instance HasLens ReportingContext (NodeContext ssc) ReportingContext where
    lensOf = lens getter (flip setter)
      where
        getter nc =
            ReportingContext
                (nc ^. ncNodeParamsL . npReportServersL)
                (nc ^. ncLoggerConfigL)
        setter rc =
            set (ncNodeParamsL . npReportServersL) (rc ^. rcReportServers) .
            set ncLoggerConfigL (rc ^. rcLoggingConfig)

instance HasLens RelayContext (NodeContext ssc) RelayContext where
    lensOf = lens getter (flip setter)
      where
        getter nc =
            RelayContext
                (nc ^. ncNodeParamsL . npPropagationL)
                (nc ^. ncInvPropagationQueueL)
        setter rc =
            set (ncNodeParamsL . npPropagationL) (_rlyIsPropagation rc) .
            set ncInvPropagationQueueL (_rlyPropagationQueue rc)

instance HasLens ShutdownContext (NodeContext ssc) ShutdownContext where
    lensOf = lens getter (flip setter)
      where
        getter nc =
            ShutdownContext
                (nc ^. ncShutdownFlagL)
                (nc ^. ncShutdownNotifyQueueL)
        setter sc =
            set ncShutdownFlagL (_shdnIsTriggered sc) .
            set ncShutdownNotifyQueueL (_shdnNotifyQueue sc)

instance HasLens JLFile (NodeContext ssc) JLFile where
    lensOf = ncJLFileL

instance HasLens GenesisUtxo (NodeContext ssc) GenesisUtxo where
    lensOf = ncNodeParamsL . npCustomUtxoL . coerced

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

-- | Generate 'PublicKey' from 'SecretKey' of 'NodeContext'.
ncPublicKey :: NodeContext ssc -> PublicKey
ncPublicKey = toPublic . npSecretKey . ncNodeParams

-- | Generate 'Address' from 'SecretKey' of 'NodeContext'
ncPubKeyAddress :: NodeContext ssc -> Address
ncPubKeyAddress = makePubKeyAddress . ncPublicKey

ncSystemStart :: NodeContext __ -> Timestamp
ncSystemStart = npSystemStart . ncNodeParams
