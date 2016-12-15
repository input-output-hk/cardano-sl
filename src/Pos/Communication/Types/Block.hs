{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for communication about Blocks.

module Pos.Communication.Types.Block
       ( SendBlock (..)
       , SendBlockHeader (..)
       , SendBlockchainPart (..)
       , RequestBlock (..)
       , RequestBlockchainPart (..)
       ) where

import           Universum

import           Control.TimeWarp.Rpc (Message (..), messageName')
import           Pos.Ssc.Class.Types  (Ssc)
import           Pos.Types            (Block, HeaderHash, MainBlockHeader)

-- | Message: some node has sent a Block.
data SendBlock ssc =
    SendBlock !(Block ssc)
    deriving (Generic)

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

instance Ssc ssc => Message (SendBlock ssc) where
    messageName _ = "SendBlock"
    formatMessage = messageName'

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
