-- | Runtime context of node.

module Pos.Context.Context
       ( NodeContext (..)
       , ncPublicKey
       , ncPubKeyAddress
       ) where

import           Universum

import           Pos.Crypto          (PublicKey, SecretKey, toPublic)
import           Pos.Ssc.Class.Types (Ssc (SscNodeContext))
import           Pos.Types           (Address, HeaderHash, Timestamp (..),
                                      makePubKeyAddress)

-- | NodeContext contains runtime context of node.
data NodeContext ssc = NodeContext
    { -- | Time when system started working.
      ncSystemStart  :: !Timestamp
    , -- | Secret key used for blocks creation.
      ncSecretKey    :: !SecretKey
    , ncTimeLord     :: !Bool
    , ncJLFile       :: !(Maybe (MVar FilePath))
    , ncDbPath       :: !(Maybe FilePath)
    , ncSscContext   :: !(SscNodeContext ssc)
    , ncPropagation  :: !Bool              -- ^ Whether to propagate txs, ssc data, blocks to neighbors
    , -- | Semaphore which manages access to block application.
      -- Stored hash is a hash of last applied block.
      ncBlkSemaphore :: !(MVar (HeaderHash ssc))
    }

-- | Generate 'PublicKey' from 'SecretKey' of 'NodeContext'.
ncPublicKey :: NodeContext ssc -> PublicKey
ncPublicKey = toPublic . ncSecretKey

-- | Generate 'Address' from 'SecretKey' of 'NodeContext'
ncPubKeyAddress :: NodeContext ssc -> Address
ncPubKeyAddress = makePubKeyAddress . ncPublicKey
