module Pos.Chain.Txp.TxMsg
       ( TxMsgContents (..)
       ) where

import           Universum

import           Formatting (bprint, build, (%))
import           Formatting.Buildable (Buildable (..))

-- | Data message. Can be used to send one transaction per message.
-- Transaction is sent with auxilary data.
newtype TxMsgContents tx = TxMsgContents
    { getTxMsgContents :: tx
    } deriving (Generic, Show, Eq)

instance Buildable tx => Buildable (TxMsgContents tx) where
    build (TxMsgContents tx) =
        bprint ("TxMsgContents { txAux ="%Formatting.build%", .. }") tx
