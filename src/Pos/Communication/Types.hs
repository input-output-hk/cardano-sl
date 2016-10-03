-- | Types used for communication.

module Pos.Communication.Types
       ( Node

       , Message (..)
       , displayMessage
       ) where

import           Formatting (int, sformat, (%))
import           Universum

import           Pos.Crypto (PrivateKey, PublicKey)
import           Pos.Types  (Block, Entry, NodeId, displayEntry)

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
       NodeId
    -> (PublicKey, PrivateKey)
    -> Int
    -> Map NodeId PublicKey
    -> (NodeId -> Message -> m ())
    -> m (NodeId -> Message -> m ())

----------------------------------------------------------------------------
-- Messages that nodes send to each other
----------------------------------------------------------------------------

data Message
    = MEntry Entry
    | MBlock Block
    | MPing
    deriving (Eq, Ord, Show)

displayMessage :: Message -> Text
displayMessage MPing       = "ping"
displayMessage (MEntry e)  = displayEntry e
displayMessage (MBlock es) = sformat ("block with "%int%" entries") (length es)
