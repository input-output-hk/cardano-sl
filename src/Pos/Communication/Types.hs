-- | Types used for communication.

module Pos.Communication.Types
       ( Node

       , Message (..)
       , displayMessage
       ) where

import           Formatting (int, sformat, (%))
import           Universum

import           Pos.Crypto (PublicKey, SecretKey)
import           Pos.Types  (Blockkk, Entry, NodeId, displayEntry)

{- |
A node is given:

* Its ID and keypair
* Total amount of nodes
* Public keys of other nodes
* A function to send messages

A node also provides a callback which can be used to send messages to the
node (and the callback knows who sent it a message).
-}
type Node m =
       NodeId                           -- node's ID
    -> (PublicKey, SecretKey)           -- node's keypair
    -> Int                              -- total amount of nodes
    -> Map NodeId PublicKey             -- other nodes' keys
    -> (NodeId -> Message -> m ())      -- function to send messages to nodes
    -> m (NodeId -> Message -> m ())    -- callback

----------------------------------------------------------------------------
-- Messages that nodes send to each other
----------------------------------------------------------------------------

data Message
    = MEntry Entry
    | MBlock Blockkk
    | MPing
    deriving (Eq)

displayMessage :: Message -> Text
displayMessage MPing       = "ping"
displayMessage (MEntry e)  = displayEntry e
displayMessage (MBlock es) = sformat ("block with "%int%" entries") (length es)
