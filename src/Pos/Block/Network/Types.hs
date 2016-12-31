{-# LANGUAGE UndecidableInstances #-}

-- | Types describing protocol messages related to Blocks.

module Pos.Block.Network.Types
       ( MsgGetHeaders (..)
       , MsgGetBlocks (..)
       , MsgHeaders (..)
       , MsgBlock (..)
       ) where

import           Data.List.NonEmpty   (NonEmpty)
import           Universum

import           Control.TimeWarp.Rpc (Message (..), messageName')
import           Pos.Ssc.Class.Types  (Ssc (SscPayload))
import           Pos.Types            (Block, BlockHeader, HeaderHash)

-- | 'GetHeaders' message (see protocol specification).
data MsgGetHeaders ssc = MsgGetHeaders
    { mghFrom :: ![HeaderHash ssc]
    , mghTo   :: !(Maybe (HeaderHash ssc))
    } deriving (Generic, Show, Eq)

instance Typeable ssc => Message (MsgGetHeaders ssc) where
    messageName _ = "GetHeaders"
    formatMessage = messageName'

-- | 'GetHeaders' message (see protocol specification).
data MsgGetBlocks ssc = MsgGetBlocks
    { mgbFrom :: !(HeaderHash ssc)
    , mgbTo   :: !(HeaderHash ssc)
    } deriving (Generic, Show, Eq)

instance Typeable ssc => Message (MsgGetBlocks ssc) where
    messageName _ = "GetBlocks"
    formatMessage = messageName'

-- | 'Headers' message (see protocol specification).
newtype MsgHeaders ssc =
    MsgHeaders (NonEmpty (BlockHeader ssc))
    deriving (Generic, Show, Eq)

instance Typeable ssc => Message (MsgHeaders ssc) where
    messageName _ = "BlockHeaders"
    formatMessage = messageName'

-- | 'Block' message (see protocol specification).
newtype MsgBlock ssc =
    MsgBlock (Block ssc)
    deriving (Generic, Show)

deriving instance (Ssc ssc, Eq (SscPayload ssc)) => Eq (MsgBlock ssc)

instance Typeable ssc => Message (MsgBlock ssc) where
    messageName _ = "Block"
    formatMessage = messageName'
