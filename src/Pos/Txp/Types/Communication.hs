{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for communication about Blocks.

module Pos.Txp.Types.Communication
       ( TxInvMsg (..)
       , TxReqMsg (..)
       , TxDataMsg (..)
       ) where

import           Control.TimeWarp.Rpc (Message (..), messageName')
import           Data.List.NonEmpty   (NonEmpty)
import           Universum

import           Pos.Types            (Tx, TxDistribution, TxId, TxWitness)

----------------------------------------------------------------------------
-- Inventory, Request and Data messages
----------------------------------------------------------------------------

-- | Inventory message. Can be used to announce the fact that you have
-- some new local transactions.
data TxInvMsg = TxInvMsg
    { imTxs :: !(NonEmpty TxId)
    } deriving (Generic)

instance Message TxInvMsg where
    messageName _ = "Tx Inventory"
    formatMessage = messageName'

-- | Request message. Can be used to request transactions (ideally
-- transactions which were previously announced by inventory message).
data TxReqMsg = TxReqMsg
    { rmTxs :: !(NonEmpty TxId)
    } deriving (Generic)

instance Message TxReqMsg where
    messageName _ = "Tx Request"
    formatMessage = messageName'

-- | Data message. Can be used to send one transaction per message.
data TxDataMsg = TxDataMsg
    { dmTx           :: !Tx
    , dmWitness      :: !TxWitness
    , dmDistribution :: !TxDistribution
    } deriving (Generic)

instance Message TxDataMsg where
    messageName _ = "Tx Data"
    formatMessage = messageName'
