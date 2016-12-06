{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for communication about Blocks.

module Pos.Txp.Types.Listener
       ( SendTx (..)
       , SendTxs (..)
       ) where

import           Control.TimeWarp.Rpc (Message (..), messageName')
import           Data.Binary          (Binary)
import           Data.List.NonEmpty   (NonEmpty)
import           Universum

import           Pos.Types            (Tx)

-- | Message: some node has sent a Transaction.
data SendTx =
    SendTx !Tx
    deriving (Generic)

instance Binary SendTx

-- | Message: some node has sent 'NonEmpty' list of 'Tx'.
data SendTxs =
    SendTxs !(NonEmpty Tx)
    deriving (Generic)

instance Binary SendTxs

instance Message SendTx where
    messageName _ = "Send Tx"
    formatMessage = messageName'

instance Message SendTxs where
    messageName _ = "Send Txs"
    formatMessage = messageName'
