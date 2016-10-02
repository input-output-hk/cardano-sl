{-# LANGUAGE DeriveGeneric #-}

-- | Definitions of the most fundamental types.

module Pos.Types.Types
       (
         NodeId (..)
       , nodeF

       , Coin (..)
       , coinF

       , Address (..)
       , addressF

       , TxIn (..)
       , TxOut (..)
       , Tx (..)
       , Entry (..)
       , Block

       , displayEntry
       ) where

import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Data.Word           (Word32)
import           Formatting          (Format, bprint, build, int, sformat, shown, (%))
import           Protolude           hiding (for, wait, (%))

import           Pos.Crypto          (Encrypted, Hash, Share)

----------------------------------------------------------------------------
-- Node. TODO: do we need it?
----------------------------------------------------------------------------

newtype NodeId = NodeId
    { getNodeId :: Int
    } deriving (Show, Eq, Ord, Enum)

instance Buildable NodeId where
    build = bprint ("#"%int) . getNodeId

nodeF :: Format r (NodeId -> r)
nodeF = build

----------------------------------------------------------------------------
-- Coin
----------------------------------------------------------------------------

-- | Coin is the least possible unit of currency.
newtype Coin = Coin
    { getCoin :: Int64
    } deriving (Num, Enum, Integral, Show, Ord, Real, Generic, Eq)

instance Buildable Coin where
    build = bprint (int%" coin(s)")

-- | Coin formatter which restricts type.
coinF :: Format r (Coin -> r)
coinF = build

----------------------------------------------------------------------------
-- Address
----------------------------------------------------------------------------

instance Buildable () where
    build () = "patak"  -- TODO: remove

-- | Address is where you can send coins.
newtype Address = Address
    { getAddress :: ()  -- ^ TODO
    } deriving (Show, Eq, Generic, Buildable, Ord)

addressF :: Format r (Address -> r)
addressF = build

----------------------------------------------------------------------------
-- Transaction
----------------------------------------------------------------------------

-- | Transaction input.
data TxIn = TxIn
    { txInHash  :: !Hash    -- ^ Which transaction's output is used
    , txInIndex :: !Word32  -- ^ Index of the output in transaction's outputs
    } deriving (Eq, Ord, Show, Generic)

-- | Transaction output.
data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    } deriving (Eq, Ord, Show, Generic)

-- | Transaction.
data Tx = Tx
    { txInputs  :: ![TxIn]   -- ^ Inputs of transaction.
    , txOutputs :: ![TxOut]  -- ^ Outputs of transaction.
    } deriving (Eq, Ord, Show, Generic)


----------------------------------------------------------------------------
-- Block. TODO: rework.
----------------------------------------------------------------------------

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
    sformat (nodeF%"'s commitment = "%shown) nid h
displayEntry (EUShare n_from n_to share) =
    sformat (nodeF%"'s share for "%nodeF%" = "%build) n_from n_to share
displayEntry (ELeaders epoch leaders) =
    sformat ("leaders for epoch "%int%" = "%shown) epoch leaders
