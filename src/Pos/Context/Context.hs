{-# LANGUAGE TemplateHaskell #-}

-- | Runtime context of node.

module Pos.Context.Context
       ( NodeContext (..)
       , ncPublicKey
       , ncPubKeyAddress
       ) where

import qualified Control.Concurrent.STM as STM
import           Data.Time.Units        (Microsecond)
import           Universum

import           Pos.Crypto             (PublicKey, SecretKey, toPublic)
import           Pos.Security.Types     (AttackTarget, AttackType)
import           Pos.Ssc.Class.Types    (Ssc (SscNodeContext))
import           Pos.Types              (Address, HeaderHash, Richmen, SlotId,
                                         SlotLeaders, Timestamp (..), makePubKeyAddress)
import           Pos.Util.UserSecret    (UserSecret)

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

-- | NodeContext contains runtime context of node.
data NodeContext ssc = NodeContext
    { ncSystemStart   :: !Timestamp
    -- ^ Time when system started working.
    , ncSecretKey     :: !SecretKey
    -- ^ Secret key used for blocks creation.
    , ncTimeLord      :: !Bool
    -- ^ Is time lord
    , ncJLFile        :: !(Maybe (MVar FilePath))
    , ncDbPath        :: !FilePath
    -- ^ Path to the database
    , ncSscContext    :: !(SscNodeContext ssc)
    , ncAttackTypes   :: ![AttackType]
    -- ^ Attack types used by malicious emulation
    , ncAttackTargets :: ![AttackTarget]
    -- ^ Attack targets used by malicious emulation
    , ncPropagation   :: !Bool
    -- ^ Whether to propagate txs, ssc data, blocks to neighbors
    , ncBlkSemaphore  :: !(MVar (HeaderHash ssc))
    -- ^ Semaphore which manages access to block application.
    -- Stored hash is a hash of last applied block.
    , ncSscLeaders    :: !(MVar SlotLeaders)
    , ncSscRichmen    :: !(MVar Richmen)
    , ncUserSecret    :: !(STM.TVar UserSecret)
    -- ^ Secret keys (and path to file) which are used to send transactions

    , ncNtpData       :: !(STM.TVar (Microsecond, Microsecond))
    , ncNtpLastSlot   :: !(STM.TVar SlotId)
    }

-- | Generate 'PublicKey' from 'SecretKey' of 'NodeContext'.
ncPublicKey :: NodeContext ssc -> PublicKey
ncPublicKey = toPublic . ncSecretKey

-- | Generate 'Address' from 'SecretKey' of 'NodeContext'
ncPubKeyAddress :: NodeContext ssc -> Address
ncPubKeyAddress = makePubKeyAddress . ncPublicKey
