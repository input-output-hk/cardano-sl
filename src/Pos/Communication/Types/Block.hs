{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

-- | Types used for communication about Blocks.

module Pos.Communication.Types.Block
       ( MsgGetHeaders (..)
       , MsgGetBlocks (..)
       , MsgHeaders (..)
       , MsgBlock (..)

       , SendBlockHeader (..)
       , SendBlockchainPart (..)
       , RequestBlock (..)
       , RequestBlockchainPart (..)
       ) where

import           Data.List.NonEmpty   (NonEmpty)
import           Universum

import           Control.TimeWarp.Rpc (Message (..), messageName')
import           Pos.Ssc.Class.Types  (Ssc)
import           Pos.Types            (Block, BlockHeader, HeaderHash, MainBlockHeader)

----------------------------------------------------------------------------
-- Messages from modern protocol specification
----------------------------------------------------------------------------

-- | 'GetHeaders' message (see protocol specification).
data MsgGetHeaders ssc = MsgGetHeaders
    { mghFrom :: ![HeaderHash ssc]
    , mghTo   :: !(Maybe (HeaderHash ssc))
    } deriving (Generic)

instance Typeable ssc => Message (MsgGetHeaders ssc) where
    messageName _ = "GetHeaders"
    formatMessage = messageName'

-- | 'GetHeaders' message (see protocol specification).
data MsgGetBlocks ssc = MsgGetBlocks
    { mgbFrom :: ![HeaderHash ssc]
    , mgbTo   :: !(Maybe (HeaderHash ssc))
    } deriving (Generic)

instance Typeable ssc => Message (MsgGetBlocks ssc) where
    messageName _ = "GetBlocks"
    formatMessage = messageName'

-- | 'Headers' message (see protocol specification).
newtype MsgHeaders ssc =
    MsgHeaders (NonEmpty (BlockHeader ssc))
    deriving (Generic)

instance Typeable ssc => Message (MsgHeaders ssc) where
    messageName _ = "BlockHeaders"
    formatMessage = messageName'

-- | 'Block' message (see protocol specification).
newtype MsgBlock ssc =
    MsgBlock (Block ssc)
    deriving (Generic)

instance Typeable ssc => Message (MsgBlock ssc) where
    messageName _ = "Block"
    formatMessage = messageName'

----------------------------------------------------------------------------
-- Obsolete messages
----------------------------------------------------------------------------

-- | Message: some node has sent a BlockHeader.
data SendBlockHeader ssc =
    SendBlockHeader !(MainBlockHeader ssc)
    deriving (Generic)

-- | Message: some node has sent a part of blockchain
data SendBlockchainPart ssc =
    SendBlockchainPart ![Block ssc]
    deriving (Generic)

-- | Message: some node has requested a Block with given HeaderHash.
data RequestBlock ssc =
    RequestBlock !(HeaderHash ssc)
    deriving (Generic)

-- | Message: some node has requested a part of blockchain
data RequestBlockchainPart ssc = RequestBlockchainPart
    { rbFromBlock  :: !(Maybe (HeaderHash ssc))
    , rbUntilBlock :: !(Maybe (HeaderHash ssc))
    , rbCount      :: !(Maybe Word)
    } deriving (Generic)

instance Ssc ssc => Message (SendBlockHeader ssc) where
    messageName _ = "SendBlockHeader"
    formatMessage = messageName'

instance Ssc ssc => Message (SendBlockchainPart ssc) where
    messageName _ = "SendBlockchainPart"
    formatMessage = messageName'

instance Typeable ssc => Message (RequestBlock ssc) where
    messageName _ = "RequestBlock"
    formatMessage = messageName'

instance Typeable ssc => Message (RequestBlockchainPart ssc) where
    messageName _ = "RequestBlockchainPart"
    formatMessage = messageName'
