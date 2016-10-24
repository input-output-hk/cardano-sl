{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for communication about Blocks.

module Pos.Communication.Types.Tx
       ( SendTx (..)
       , SendTxs (..)
       ) where

import           Control.TimeWarp.Rpc (Message (messageName))
import           Data.Binary          (Binary)
import           Data.MessagePack     (MessagePack)
import           Universum

import           Pos.Types            (Tx)

-- | Message: some node has sent a Transaction.
data SendTx =
    SendTx !Tx
    deriving (Generic)

instance Binary SendTx

instance MessagePack SendTx

data SendTxs =
    SendTxs ![Tx]
    deriving (Generic)

instance Binary SendTxs

instance MessagePack SendTxs

instance Message SendTx where
    messageName _ = "Send Tx"

instance Message SendTxs where
    messageName _ = "Send Txs"
