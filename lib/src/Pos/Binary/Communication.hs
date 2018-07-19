{-# LANGUAGE BinaryLiterals #-}

-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication
    ( serializeMsgSerializedBlock
    , serializeMsgStreamBlock
    ) where

import           Universum

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Pos.Binary.Class (serialize, serialize')
import           Pos.Block.BHelpers ()
import           Pos.DB.Class (Serialized (..))
import           Pos.Network.Block.Types (MsgBlock (..),
                     MsgSerializedBlock (..), MsgStreamBlock (..))

-- TODO: move into each component

----------------------------------------------------------------------------
-- Blocks
----------------------------------------------------------------------------

-- Serialize `MsgSerializedBlock` with the property
-- ```
-- serialize (MsgBlock b) = serializeMsgSerializedBlock (MsgSerializedBlock $ serialize b)
-- ```
serializeMsgSerializedBlock :: MsgSerializedBlock -> BS.ByteString
serializeMsgSerializedBlock (MsgSerializedBlock b)   = "\x82\x0" <> unSerialized b
serializeMsgSerializedBlock (MsgNoSerializedBlock t) = serialize' (MsgNoBlock t)

-- Serialize `MsgSerializedBlock` with the property
-- ```
-- serialize (MsgStreamBlock b) = serializeMsgStreamBlock (MsgSerializedBlock $ serialize b)
-- ```
serializeMsgStreamBlock :: MsgSerializedBlock -> LBS.ByteString
serializeMsgStreamBlock (MsgSerializedBlock b)   = "\x82\x0" <> LBS.fromStrict (unSerialized b)
serializeMsgStreamBlock (MsgNoSerializedBlock t) = serialize (MsgStreamNoBlock t)
