{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for communication about Blocks.

module Pos.Communication.Types.Tx
       ( SendTx (..)
       ) where

import           Control.TimeWarp.Rpc (Message (messageName), mkMessage)
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

-- Currently we use 'Void' as the “exception” type, this should be replaced.
mkMessage ''Void

instance Message SendTx where
    messageName _ = "Send Tx"
