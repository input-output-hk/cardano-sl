-- | Serialization of Block-related network messages.

module Pos.Binary.Block.Network () where

import           Universum

import           Pos.Binary.Class        (Bi (..))
import           Pos.Block.Network.Types (MsgBlock (..), MsgGetBlocks (..),
                                          MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Types               ()

instance Bi (MsgGetHeaders ssc) where
    put (MsgGetHeaders f t) = put f >> put t
    get = MsgGetHeaders <$> get <*> get

instance Bi (MsgGetBlocks ssc) where
    put (MsgGetBlocks f t) = put f >> put t
    get = MsgGetBlocks <$> get <*> get

instance Ssc ssc => Bi (MsgHeaders ssc) where
    put (MsgHeaders b) = put b
    get = MsgHeaders <$> get

instance Ssc ssc => Bi (MsgBlock ssc) where
    put (MsgBlock b) = put b
    get = MsgBlock <$> get
