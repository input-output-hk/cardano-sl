{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for communication about Blocks.

module Pos.Txp.Types.Communication
       ( TxMsgTag (..)
       , TxMsgContents (..)
       ) where

import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, build, (%))
import           Universum

import           Pos.Types           (Tx, TxDistribution, TxWitness)
import           Pos.Util            (NamedMessagePart (..))

data TxMsgTag = TxMsgTag

instance NamedMessagePart TxMsgTag where
    nMessageName _     = "Tx tag"

instance Buildable TxMsgTag where
    build _ = "TxMsgTag"

-- | Data message. Can be used to send one transaction per message.
data TxMsgContents = TxMsgContents
    { dmTx           :: !Tx
    , dmWitness      :: !TxWitness
    , dmDistribution :: !TxDistribution
    } deriving (Generic)

instance NamedMessagePart TxMsgContents where
    nMessageName _     = "Tx contents"

instance Buildable TxMsgContents where
    build TxMsgContents {..} = bprint ("TxMsgContents { tx="%build%", .. }") dmTx
