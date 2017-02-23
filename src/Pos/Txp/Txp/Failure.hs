-- | Txp failures.

module Pos.Txp.Txp.Failure
       ( TxpVerFailure (..)
       ) where

import           Data.Text.Buildable (Buildable (..))
import           Formatting          (stext, bprint, int, (%))
import           Universum

-- | Result of transaction processing
data TxpVerFailure
    = TxpKnown -- ^ Transaction is already in the storage (cache)
    | TxpInvalid !Text
    | TxpOverwhelmed -- ^ Local transaction storage is full -- can't accept more txs
    | TxpCantTopsort
    | TxpInvalidUndoLength !Int !Int
    deriving (Show, Eq)

instance Buildable TxpVerFailure where
    build TxpKnown =
        "transaction already is in the mem pool"
    build (TxpInvalid txt) =
        bprint ("txp internal error: "%stext) txt
    build TxpOverwhelmed =
        "max size of the mem pool is reached"
    build TxpCantTopsort =
        "transactions can't be topsored"
    build (TxpInvalidUndoLength ex rcv) =
        bprint ("length of transaction's inputs = "%int%
               " doesn't equal length of transaction's undo = "%int) ex rcv
