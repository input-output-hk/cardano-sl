{-# LANGUAGE TemplateHaskell #-}

-- | Runtime context of node.

module Pos.Context.Context
       ( LrcSyncData
       , NodeContext (..)
       , ncPublicKey
       , ncPubKeyAddress
       ) where

import qualified Control.Concurrent.STM         as STM
import           Control.Concurrent.STM.TBQueue (TBQueue)
import           Pos.Communication.Protocol     (NodeId)
import           System.Wlog                    (LoggerConfig)
import           Universum

import           Pos.Crypto                     (PublicKey, SecretKey, toPublic)
import           Pos.Security.CLI               (AttackTarget, AttackType)
import           Pos.Slotting.Types             (SlottingState)
import           Pos.Ssc.Class.Types            (Ssc (SscNodeContext))
import           Pos.Types                      (Address, BlockHeader, EpochIndex,
                                                 HeaderHash, SlotLeaders, Timestamp (..),
                                                 Utxo, makePubKeyAddress)
import           Pos.Update.Poll.Types          (ConfirmedProposalState)
import           Pos.Util                       (NE, NewestFirst)
import           Pos.Util.UserSecret            (UserSecret)

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

-- | Data used for LRC syncronization. First value is __False__ iff
-- LRC is running now. Second value is last epoch for which we have
-- already computed LRC.
type LrcSyncData = (Bool, EpochIndex)

-- | NodeContext contains runtime context of node.
data NodeContext ssc = NodeContext
    { ncSystemStart         :: !Timestamp
    -- ^ Time when system started working.
    , ncSecretKey           :: !SecretKey
    -- ^ Secret key used for blocks creation.
    , ncGenesisUtxo         :: !Utxo
    -- ^ Genesis utxo
    , ncGenesisLeaders      :: !SlotLeaders
    -- ^ Leaders for 0-th epoch
    , ncSlottingState       :: !(STM.TVar SlottingState)
    -- ^ Data needed for the slotting algorithm to work
    , ncTimeLord            :: !Bool
    -- ^ Is time lord
    , ncJLFile              :: !(Maybe (MVar FilePath))
    , ncDbPath              :: !FilePath
    -- ^ Path to the database
    , ncSscContext          :: !(SscNodeContext ssc)
    , ncAttackTypes         :: ![AttackType]
    -- ^ Attack types used by malicious emulation
    , ncAttackTargets       :: ![AttackTarget]
    -- ^ Attack targets used by malicious emulation
    , ncPropagation         :: !Bool
    -- ^ Whether to propagate txs, ssc data, blocks to neighbors
    , ncBlkSemaphore        :: !(MVar HeaderHash)
    -- ^ Semaphore which manages access to block application.
    -- Stored hash is a hash of last applied block.
    , ncLrcSync             :: !(STM.TVar LrcSyncData)
    -- ^ Primitive for synchronization with LRC.
    , ncUserSecret          :: !(STM.TVar UserSecret)
    -- ^ Secret keys (and path to file) which are used to send transactions
    , ncKademliaDump        :: !FilePath
    -- ^ Path to kademlia dump file
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
    , ncUpdatePath          :: !FilePath
    -- ^ Path to update installer executable, downloaded by update system
    , ncUpdateWithPkg       :: !Bool
    -- ^ Whether to use installer update mechanism
    , ncUpdateServers       :: ![Text]
    -- ^ Update servers to download updates from
    , ncReportServers       :: ![Text]
    -- ^ Reporting servers' URLs
    , ncLoggerConfig        :: !LoggerConfig
    -- ^ Logger config, as taken/read from CLI
    }

-- | Generate 'PublicKey' from 'SecretKey' of 'NodeContext'.
ncPublicKey :: NodeContext ssc -> PublicKey
ncPublicKey = toPublic . ncSecretKey

-- | Generate 'Address' from 'SecretKey' of 'NodeContext'
ncPubKeyAddress :: NodeContext ssc -> Address
ncPubKeyAddress = makePubKeyAddress . ncPublicKey
