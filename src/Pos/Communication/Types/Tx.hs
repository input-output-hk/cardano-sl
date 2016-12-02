{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for communication about Blocks.

module Pos.Communication.Types.Tx
       ( SendTx (..)
       , SendTxs (..)
       ) where

import           Control.TimeWarp.Rpc (Message (..), messageName')
import           Data.Binary          (Binary)
import           Data.List.NonEmpty   (NonEmpty)
import           Data.MessagePack     (MessagePack)
import           Universum

import           Pos.Types            (Tx, TxWitness)

-- | Message: some node has sent a Transaction.
data SendTx =
    SendTx !Tx !TxWitness
    deriving (Generic)

instance Binary SendTx

instance MessagePack SendTx

-- | Message: some node has sent 'NonEmpty' list of 'Tx'.
data SendTxs =
    SendTxs !(NonEmpty (Tx, TxWitness))
    deriving (Generic)

instance Binary SendTxs

instance MessagePack SendTxs

instance Message SendTx where
    messageName _ = "Send Tx"
    formatMessage = messageName'

instance Message SendTxs where
    messageName _ = "Send Txs"
    formatMessage = messageName'
