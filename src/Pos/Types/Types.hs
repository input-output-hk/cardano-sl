-- | Definitions of the most fundamental types.

module Pos.Types.Types
       ( NodeId (..)
       , node
       , TxIn (..)
       , TxOut (..)
       , Tx (..)
       , Entry (..)
       , Block

       , displayEntry
       ) where

import qualified Data.Text.Buildable as Buildable
import           Formatting          (Format, bprint, build, int, sformat, shown, (%))
import           Pos.Crypto          (Encrypted, Hash, Share)
import           Protolude           hiding (for, wait, (%))

----------------------------------------------------------------------------
-- Node
----------------------------------------------------------------------------

newtype NodeId = NodeId
    { getNodeId :: Int
    } deriving (Show, Eq, Ord, Enum)

instance Buildable.Buildable NodeId where
    build = bprint ("#"%int) . getNodeId

node :: Format r (NodeId -> r)
node = build

----------------------------------------------------------------------------
-- Transactions, blocks
----------------------------------------------------------------------------

-- | Transaction input
data TxIn = TxIn
    { txInHash  :: Hash -- ^ Which transaction's output is used
    , txInIndex :: Int -- ^ Index of the output in transaction's outputs
    } deriving (Eq, Ord, Show)

-- | Transaction output
data TxOut = TxOut {
    txOutValue :: Word64 }   -- ^ Output value
    deriving (Eq, Ord, Show)

-- | Transaction
data Tx = Tx
    { txInputs  :: [TxIn]
    , txOutputs :: [TxOut]
    , txHash    :: Hash -- ^ Hash of the transaction
    } deriving (Eq, Ord, Show)

-- | An entry in a block
data Entry

      -- | Transaction
    = ETx Tx

      -- | Hash of random string U that a node has committed to
    | EUHash NodeId Hash
      -- | An encrypted piece of secret-shared U that the first node sent to
      -- the second node (and encrypted with the second node's pubkey)
    | EUShare NodeId NodeId (Encrypted Share)
      -- | Leaders for a specific epoch
    | ELeaders Int [NodeId]

    deriving (Eq, Ord, Show)

-- | Block
type Block = [Entry]

displayEntry :: Entry -> Text
displayEntry (ETx tx) =
    "transaction " <> show tx
displayEntry (EUHash nid h) =
    sformat (node%"'s commitment = "%shown) nid h
displayEntry (EUShare n_from n_to share) =
    sformat (node%"'s share for "%node%" = "%build) n_from n_to share
displayEntry (ELeaders epoch leaders) =
    sformat ("leaders for epoch "%int%" = "%shown) epoch leaders
