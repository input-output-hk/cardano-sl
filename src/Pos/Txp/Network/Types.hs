-- | Types used for communication about Blocks.

module Pos.Txp.Network.Types
       ( TxMsgTag (..)
       , TxMsgContents (..)
       ) where

import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, (%))
import           Universum

import           Pos.Binary.Core     ()
import           Pos.Txp.Core        (TxAux (..), txaF)

data TxMsgTag = TxMsgTag deriving (Eq, Show)

instance Buildable TxMsgTag where
    build _ = "TxMsgTag"

-- | Data message. Can be used to send one transaction per message.
-- Transaction is sent with auxilary data.
newtype TxMsgContents = TxMsgContents
    { getTxMsgContents :: TxAux
    } deriving (Generic, Show, Eq)

instance Buildable TxMsgContents where
    build (TxMsgContents txAux) =
        bprint ("TxMsgContents { txAux ="%txaF%", .. }") txAux
