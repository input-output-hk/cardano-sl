{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for communication about Blocks.

module Pos.Communication.Types.Block
       ( SendBlock (..)
       , SendBlockHeader (..)
       , RequestBlock (..)
       ) where

import           Data.Binary          (Binary)
import           Universum

import           Control.TimeWarp.Rpc (Message (..))
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

-- | Message: some node has requested a Block with given HeaderHash.
data RequestBlock ssc =
    RequestBlock !(HeaderHash ssc)
    deriving (Generic)

instance Ssc ssc => Binary (SendBlock ssc)
instance Ssc ssc => Binary (SendBlockHeader ssc)
instance Binary (RequestBlock ssc)

instance (Ssc ssc) => Message (SendBlock ssc) where
    messageName _ = "SendBlock"

instance (Ssc ssc) => Message (SendBlockHeader ssc) where
    messageName _ = "SendBlockHeader"

instance Typeable ssc => Message (RequestBlock ssc) where
    messageName _ = "RequestBlock"
