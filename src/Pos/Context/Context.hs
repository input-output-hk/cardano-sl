{-# LANGUAGE TemplateHaskell #-}

-- | Runtime context of node.

module Pos.Context.Context
       ( LrcSyncData
       , NodeContext (..)
       , ncPublicKey
       , ncPubKeyAddress
       , ncGenesisLeaders
       , ncGenesisUtxo
       , NodeParams(..)
       , BaseParams(..)
       ) where

import qualified Control.Concurrent.STM           as STM
import           Control.Concurrent.STM.TBQueue   (TBQueue)
import           Data.Time.Clock                  (UTCTime)
import           Pos.Communication.Types.Protocol (NodeId)
import           System.Wlog                      (LoggerConfig)
import           Universum

import           Pos.Crypto                       (PublicKey, toPublic)
import           Pos.Genesis                      (genesisLeaders)
import           Pos.Launcher.Param               (BaseParams (..), NodeParams (..))
import           Pos.Slotting.Types               (SlottingState)
import           Pos.Ssc.Class.Types              (Ssc (SscNodeContext))
import           Pos.Types                        (Address, BlockHeader, EpochIndex,
                                                   HeaderHash, SlotLeaders, Utxo,
                                                   makePubKeyAddress)
import           Pos.Update.Poll.Types            (ConfirmedProposalState)
import           Pos.Util                         (NE, NewestFirst)
import           Pos.Util.UserSecret              (UserSecret)

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

-- | Data used for LRC syncronization. First value is __False__ iff
-- LRC is running now. Second value is last epoch for which we have
-- already computed LRC.
type LrcSyncData = (Bool, EpochIndex)

-- | NodeContext contains runtime context of node.
data NodeContext ssc = NodeContext
    { ncSlottingState       :: !(STM.TVar SlottingState)
    -- ^ Data needed for the slotting algorithm to work
    , ncJLFile              :: !(Maybe (MVar FilePath))
    -- @georgeee please add documentation when you see this comment
    , ncSscContext          :: !(SscNodeContext ssc)
    -- @georgeee please add documentation when you see this comment
    , ncBlkSemaphore        :: !(MVar HeaderHash)
    -- ^ Semaphore which manages access to block application.
    -- Stored hash is a hash of last applied block.
    , ncLrcSync             :: !(STM.TVar LrcSyncData)
    -- ^ Primitive for synchronization with LRC.
    , ncUserSecret          :: !(STM.TVar UserSecret)
    -- ^ Secret keys (and path to file) which are used to send transactions
    , ncBlockRetrievalQueue :: !(TBQueue (NodeId, NewestFirst NE (BlockHeader ssc)))
    -- ^ Concurrent queue that holds block headers that are to be
    -- downloaded.
    , ncRecoveryHeader      :: !(STM.TMVar (NodeId, BlockHeader ssc))
    -- ^ In case of recovery mode this variable holds the latest
    -- header hash we know about so we can do chained block
    -- requests. Invariant: this mvar is full iff we're more than
    -- 'recoveryHeadersMessage' blocks deep relatively to some valid
    -- header and we're downloading blocks. Every time we get block
    -- that's more difficult than this one, we overwrite. Every time
    -- we process some blocks and fail or see that we've downloaded
    -- this header, we clean mvar.
    , ncUpdateSemaphore     :: !(MVar ConfirmedProposalState)
    -- ^ A semaphore which is unlocked when update data is downloaded
    -- and ready to apply
    , ncLoggerConfig        :: !LoggerConfig
    -- ^ Logger config, as taken/read from CLI
    , ncNodeParams          :: !NodeParams
    -- ^ Params node is launched with
    , ncShutdownFlag        :: !(STM.TVar Bool)
    -- ^ If this flag is `True`, then workers should stop.
    , ncShutdownNotifyQueue :: !(TBQueue ())
    -- ^ A queue which is used to count how many workers have successfully
    -- terminated
    , ncSendLock            :: !(Maybe (MVar ()))
    -- ^ Exclusive lock for sending messages to other nodes
    -- (if Nothing, no lock used)
    , ncStartTime           :: !UTCTime
    -- ^ Time when node was started ('NodeContext' initialized).
    }

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

-- | Generate 'PublicKey' from 'SecretKey' of 'NodeContext'.
ncPublicKey :: NodeContext ssc -> PublicKey
ncPublicKey = toPublic . npSecretKey . ncNodeParams

-- | Generate 'Address' from 'SecretKey' of 'NodeContext'
ncPubKeyAddress :: NodeContext ssc -> Address
ncPubKeyAddress = makePubKeyAddress . ncPublicKey

ncGenesisUtxo :: NodeContext ssc -> Utxo
ncGenesisUtxo = npCustomUtxo . ncNodeParams

ncGenesisLeaders :: NodeContext ssc -> SlotLeaders
ncGenesisLeaders = genesisLeaders . ncGenesisUtxo
