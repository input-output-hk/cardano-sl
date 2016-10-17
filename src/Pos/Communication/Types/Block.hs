{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for communication about Blocks.

module Pos.Communication.Types.Block
       ( SendBlock (..)
       , SendBlockHeader (..)
       , RequestBlock (..)
       ) where

import           Control.TimeWarp.Rpc (mkMessage, mkRequest')
import           Data.Binary          (Binary)
import           Data.MessagePack     (MessagePack)
import           Universum

import           Pos.Types            (Block, HeaderHash, MainBlockHeader)

-- | Message: some node has sent a Block.
data SendBlock =
    SendBlock !Block
    deriving (Generic)

-- | Message: some node has sent a BlockHeader.
data SendBlockHeader =
    SendBlockHeader !MainBlockHeader
    deriving (Generic)

-- | Message: some node has requested a Block with given HeaderHash.
data RequestBlock =
    RequestBlock !HeaderHash
    deriving (Generic)

instance Binary SendBlock
instance Binary SendBlockHeader
instance Binary RequestBlock

instance MessagePack SendBlock
instance MessagePack SendBlockHeader
instance MessagePack RequestBlock

-- Currently we use 'Void' as the “exception” type, this should be replaced.
mkMessage ''Void

mkRequest' ''SendBlock ''() ''Void
mkRequest' ''SendBlockHeader ''() ''Void
mkRequest' ''RequestBlock ''() ''Void
