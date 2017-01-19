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
import           Data.Time.Units                (Microsecond)
import           Node                           (NodeId)
import           Universum

import           Pos.Crypto                     (PublicKey, SecretKey, toPublic)
import           Pos.Security.CLI               (AttackTarget, AttackType)
import           Pos.Ssc.Class.Types            (Ssc (SscNodeContext))
import           Pos.Types                      (Address, BlockHeader, EpochIndex,
                                                 HeaderHash, SlotId, SlotLeaders,
                                                 Timestamp (..), Utxo, makePubKeyAddress)
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
    , ncKademliaDump  :: !FilePath
    -- ^ Path to kademlia dump file
    , ncNtpData             :: !(STM.TVar (Microsecond, Microsecond))
    -- ^ Data for NTP Worker.
    -- First element is margin (difference between global time and local time)
    -- which we got from NTP server in last tme.
    -- Second element is time (local time) for which we got margin in last time.
    , ncNtpLastSlot         :: !(STM.TVar SlotId)
    -- ^ Slot which was returned from getCurrentSlot in last time
    , ncBlockRetrievalQueue :: !(TBQueue (NodeId, NonEmpty (BlockHeader ssc)))
    }

-- | Generate 'PublicKey' from 'SecretKey' of 'NodeContext'.
ncPublicKey :: NodeContext ssc -> PublicKey
ncPublicKey = toPublic . ncSecretKey

-- | Generate 'Address' from 'SecretKey' of 'NodeContext'
ncPubKeyAddress :: NodeContext ssc -> Address
ncPubKeyAddress = makePubKeyAddress . ncPublicKey
