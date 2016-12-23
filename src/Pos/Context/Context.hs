{-# LANGUAGE TemplateHaskell #-}

-- | Runtime context of node.

module Pos.Context.Context
       ( NodeContext (..)

       , ncPublicKey
       , ncPubKeyAddress

       , ProxyCaches (..)
       , ncProxyMsgCache
       , ncProxyConfCache
       , defaultProxyCaches
       ) where

import qualified Control.Concurrent.STM as STM
import           Control.Lens           (makeLenses)
import qualified Data.HashMap.Strict    as HM
import           Data.Time.Clock        (UTCTime)
import           Universum

import           Pos.Crypto             (PublicKey, SecretKey, toPublic)
import           Pos.Security.Types     (AttackTarget, AttackType)
import           Pos.Ssc.Class.Types    (Ssc (SscNodeContext))
import           Pos.Types              (Address, HeaderHash, Participants, ProxySKEpoch,
                                         SlotLeaders, Timestamp (..), makePubKeyAddress)
import           Pos.Util.UserSecret    (UserSecret)

---------------------------------------------------------------------------
-- Delegation in-memory data
----------------------------------------------------------------------------

-- TODO FIXME HALP HALP!!
--
-- These caches should be moved to in-memory storage. Probably
-- NodeContext isn't the worst candidate for it, but this should be
-- thought about in more details anyway.
--
-- I won't move it anywhere out from NodeContext in order for this
-- hack not to settle. I still don't understand how to build up a
-- proper in-memory storage: I've tried to move all ProxyCached logic
-- to Context.Delegation but it appeared i can't use it because
-- WorkMode imports Context so i get import loop. I tried to use
-- classy lenses -- failed to: they don't cover MVar itself.

-- | Proxy cache storage. Is used to:
-- * Synchronize on rocks database
-- * Store caches needed to do propagation correctly
--   and answer confirmation requests
--
-- Maybe ncProxyCache should be LRU instead of hashmap, but that's not
-- urgent optimization idea.
data ProxyCaches = ProxyCaches
    { _ncProxyMsgCache  :: HashMap ProxySKEpoch UTCTime -- ^ PSK cache
    , _ncProxyConfCache :: HashMap ProxySKEpoch UTCTime -- ^ Confirmation cache
    } deriving Show

makeLenses ''ProxyCaches

defaultProxyCaches :: ProxyCaches
defaultProxyCaches = ProxyCaches HM.empty HM.empty

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

-- | NodeContext contains runtime context of node.
data NodeContext ssc = NodeContext
    { ncSystemStart   :: !Timestamp -- ^ Time when system started working.
    , ncSecretKey     :: !SecretKey -- ^ Secret key used for blocks creation.
    , ncTimeLord      :: !Bool      -- ^ Is time lord
    , ncJLFile        :: !(Maybe (MVar FilePath))
    , ncDbPath        :: !(Maybe FilePath) -- ^ Path to the database
    , ncSscContext    :: !(SscNodeContext ssc)
    , ncProxyCaches   :: !(MVar ProxyCaches) -- ^ Holds proxy caches
    , ncAttackTypes   :: ![AttackType] -- ^ Attack types used by malicious emulation
    , ncAttackTargets :: ![AttackTarget] -- ^ Attack targets used by malicious emulation
    , ncPropagation   :: !Bool -- ^ Whether to propagate txs, ssc data, blocks to neighbors
      -- | Semaphore which manages access to block application.
      -- Stored hash is a hash of last applied block.
    , ncBlkSemaphore  :: !(MVar (HeaderHash ssc))
    , ncSscLeaders    :: !(MVar SlotLeaders)
    , ncSscRichmen    :: !(MVar Participants)
    , ncUserSecret    :: !(STM.TVar UserSecret) -- ^ Secret keys (and path to file) which are used to send transactions
    }

-- | Generate 'PublicKey' from 'SecretKey' of 'NodeContext'.
ncPublicKey :: NodeContext ssc -> PublicKey
ncPublicKey = toPublic . ncSecretKey

-- | Generate 'Address' from 'SecretKey' of 'NodeContext'
ncPubKeyAddress :: NodeContext ssc -> Address
ncPubKeyAddress = makePubKeyAddress . ncPublicKey
