{-# LANGUAGE UndecidableInstances #-}

-- | Types describing protocol messages related to Blocks.

module Pos.Block.Network.Types
       ( MsgGetHeaders (..)
       , MsgGetBlocks (..)
       , MsgHeaders (..)
       , MsgBlock (..)
       , InConv (..)
       ) where

import           Control.TimeWarp.Rpc  (Message (..), messageName')
import qualified Data.ByteString.Char8 as BC
import           Data.List.NonEmpty    (NonEmpty)
import           Data.Proxy            (Proxy)
import           Formatting            (sformat, stext, (%))
import qualified Message.Message       as M
import           Universum

import           Pos.Binary.Class      (Bi)
import           Pos.Ssc.Class.Types   (Ssc (SscPayload))
import           Pos.Types             (Block, BlockHeader, HeaderHash)

newtype InConv m = InConv { inConvMsg :: m }
    deriving (Generic, Show, Eq, Bi)

inConvUnproxy :: Proxy (InConv m) -> Proxy m
inConvUnproxy _ = Proxy

instance M.Message m => M.Message (InConv m) where
    messageName (inConvUnproxy -> p) = M.MessageName $ BC.pack "InConv " <> mName
      where
        M.MessageName mName = M.messageName p
    formatMessage InConv {..} = sformat ("InConv " % stext) $ M.formatMessage inConvMsg

-- | 'GetHeaders' message (see protocol specification).
data MsgGetHeaders ssc = MsgGetHeaders
    { mghFrom :: ![HeaderHash ssc]
    , mghTo   :: !(Maybe (HeaderHash ssc))
    } deriving (Generic, Show, Eq)

instance Typeable ssc => Message (MsgGetHeaders ssc) where
    messageName _ = "GetHeaders"
    formatMessage = messageName'

instance M.Message (MsgGetHeaders ssc) where
    messageName _ = M.MessageName $ BC.pack "GetHeaders"
    formatMessage _ = "GetHeaders"

-- | 'GetHeaders' message (see protocol specification).
data MsgGetBlocks ssc = MsgGetBlocks
    { mgbFrom :: !(HeaderHash ssc)
    , mgbTo   :: !(HeaderHash ssc)
    } deriving (Generic, Show, Eq)

instance Typeable ssc => Message (MsgGetBlocks ssc) where
    messageName _ = "GetBlocks"
    formatMessage = messageName'

instance M.Message (MsgGetBlocks ssc) where
    messageName _ = M.MessageName $ BC.pack "GetBlocks"
    formatMessage _ = "GetBlocks"

-- | 'Headers' message (see protocol specification).
newtype MsgHeaders ssc =
    MsgHeaders (NonEmpty (BlockHeader ssc))
    deriving (Generic, Show, Eq)

instance Typeable ssc => Message (MsgHeaders ssc) where
    messageName _ = "BlockHeaders"
    formatMessage = messageName'

instance M.Message (MsgHeaders ssc) where
    messageName _ = M.MessageName $ BC.pack "BlockHeaders"
    formatMessage _ = "BlockHeaders"

-- | 'Block' message (see protocol specification).
newtype MsgBlock ssc =
    MsgBlock (Block ssc)
    deriving (Generic, Show)

deriving instance (Ssc ssc, Eq (SscPayload ssc)) => Eq (MsgBlock ssc)

instance Typeable ssc => Message (MsgBlock ssc) where
    messageName _ = "Block"
    formatMessage = messageName'

instance M.Message (MsgBlock ssc) where
    messageName _ = M.MessageName $ BC.pack "Block"
    formatMessage _ = "Block"
