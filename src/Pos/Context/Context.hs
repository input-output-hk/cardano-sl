{-# LANGUAGE TemplateHaskell #-}
-- | Runtime context of node.

module Pos.Context.Context
       ( NodeContext (..)

       , ncPublicKey
       , ncPubKeyAddress

       , ProxyStorage (..)
       , ncProxySecretKeys
       , ncProxyCache
       , ncProxyConfCache
       , defaultProxyStorage
       ) where

import           Control.Lens        (makeLenses)
import qualified Data.HashMap.Strict as HM
import           Data.Time.Clock     (UTCTime)
import           Universum

import           Pos.Crypto          (ProxySecretKey, PublicKey, SecretKey, toPublic)
import           Pos.Ssc.Class.Types (Ssc (SscNodeContext))
import           Pos.Types           (Address, EpochIndex, HeaderHash, Timestamp (..),
                                      makePubKeyAddress)


-- TODO FIXME HALP HALP!!
-- Temporary hack, move to rocksdb storage as soon as it's stable!!!
-- I won't move it anywhere out from NodeContext in order for this
-- hack not to settle.
type PSK = ProxySecretKey (EpochIndex,EpochIndex)

data ProxyStorage = ProxyStorage
    { _ncProxySecretKeys :: [PSK] -- ^ Proxy sertificates that authorize us
    , _ncProxyCache      :: HashMap PSK UTCTime -- ^ Cache. (psk, time added).
    , _ncProxyConfCache  :: HashMap PSK UTCTime -- ^ Confirmation cache
    } deriving Show

makeLenses ''ProxyStorage

defaultProxyStorage :: ProxyStorage
defaultProxyStorage = ProxyStorage [] HM.empty HM.empty

----------------------------------------------------------------------------
-- Node Context
----------------------------------------------------------------------------

-- | NodeContext contains runtime context of node.
data NodeContext ssc = NodeContext
    { ncSystemStart  :: !Timestamp -- ^ Time when system started working.
    , ncSecretKey    :: !SecretKey -- ^ Secret key used for blocks creation.
    , ncTimeLord     :: !Bool      -- ^ Is time lord
    , ncJLFile       :: !(Maybe (MVar FilePath))
    , ncDbPath       :: !(Maybe FilePath) -- ^ Path to the database
    , ncSscContext   :: !(SscNodeContext ssc)
    , ncProxyStorage :: !(MVar ProxyStorage)
    , ncPropagation  :: !Bool -- ^ Whether to propagate txs, ssc data, blocks to neighbors
      -- | Semaphore which manages access to block application.
      -- Stored hash is a hash of last applied block.
    , ncBlkSemaphore :: !(MVar (HeaderHash ssc))
    }

-- | Generate 'PublicKey' from 'SecretKey' of 'NodeContext'.
ncPublicKey :: NodeContext ssc -> PublicKey
ncPublicKey = toPublic . ncSecretKey

-- | Generate 'Address' from 'SecretKey' of 'NodeContext'
ncPubKeyAddress :: NodeContext ssc -> Address
ncPubKeyAddress = makePubKeyAddress . ncPublicKey
