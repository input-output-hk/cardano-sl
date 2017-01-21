{-# LANGUAGE UndecidableInstances #-}

-- | Types describing protocol messages related to Blocks.

module Pos.Block.Network.Types
       ( MsgGetHeaders (..)
       , MsgGetBlocks (..)
       , MsgHeaders (..)
       , MsgBlock (..)
       ) where

import qualified Data.ByteString.Char8 as BC
import           Universum

import           Node.Message          (Message (..), MessageName (..))
import           Pos.Ssc.Class.Types   (Ssc (SscPayload))
import           Pos.Types             (Block, BlockHeader, HeaderHash)
import           Pos.Util              (NE, NewestFirst)

-- | 'GetHeaders' message (see protocol specification).
data MsgGetHeaders = MsgGetHeaders
    { -- not guaranteed to be in any particular order
      mghFrom :: ![HeaderHash]
    , mghTo   :: !(Maybe HeaderHash)
    } deriving (Generic, Show, Eq)

instance Message MsgGetHeaders where
    messageName _ = MessageName $ BC.pack "GetHeaders"
    formatMessage _ = "GetHeaders"

-- | 'GetHeaders' message (see protocol specification).
data MsgGetBlocks = MsgGetBlocks
    { mgbFrom :: !HeaderHash
    , mgbTo   :: !HeaderHash
    } deriving (Generic, Show, Eq)

instance Message MsgGetBlocks where
    messageName _ = MessageName $ BC.pack "GetBlocks"
    formatMessage _ = "GetBlocks"

-- | 'Headers' message (see protocol specification).
newtype MsgHeaders ssc =
    MsgHeaders (NewestFirst NE (BlockHeader ssc))
    deriving (Generic, Show, Eq)

instance Message (MsgHeaders ssc) where
    messageName _ = MessageName $ BC.pack "BlockHeaders"
    formatMessage _ = "BlockHeaders"

-- | 'Block' message (see protocol specification).
newtype MsgBlock ssc =
    MsgBlock (Block ssc)
    deriving (Generic, Show)

deriving instance (Ssc ssc, Eq (SscPayload ssc)) => Eq (MsgBlock ssc)

instance Message (MsgBlock ssc) where
    messageName _ = MessageName $ BC.pack "Block"
    formatMessage _ = "Block"
