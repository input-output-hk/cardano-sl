{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication
    ( serializeMsgSerializedBlock
    , serializeMsgStreamBlock
    ) where

import           Universum

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Pos.Binary.Class (Bi, serialize, serialize')
import           Pos.Chain.Block ()
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
serializeMsgSerializedBlock :: forall block . Bi block => MsgSerializedBlock block -> BS.ByteString
serializeMsgSerializedBlock (MsgSerializedBlock b)   = "\x82\x0" <> unSerialized b
serializeMsgSerializedBlock (MsgNoSerializedBlock t) = serialize' (MsgNoBlock t :: MsgBlock block)

-- Serialize `MsgSerializedBlock` with the property
-- ```
-- serialize (MsgStreamBlock b) = serializeMsgStreamBlock (MsgSerializedBlock $ serialize b)
-- ```
serializeMsgStreamBlock :: forall block . Bi block => MsgSerializedBlock block -> LBS.ByteString
serializeMsgStreamBlock (MsgSerializedBlock b)   = "\x82\x0" <> LBS.fromStrict (unSerialized b)
serializeMsgStreamBlock (MsgNoSerializedBlock t) = serialize (MsgStreamNoBlock t :: MsgStreamBlock block)
