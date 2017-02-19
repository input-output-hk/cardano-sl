module Pos.Txp.Txp.Failure
    ( TxpVerFailure (..)
    ) where

import           Data.Text.Buildable (Buildable (..))
import           Universum

-- | Result of transaction processing
data TxpVerFailure
    = TxpKnown -- ^ Transaction is already in the storage (cache)
    | TxpInvalid !Text -- ^ Can't add transaction
    -- TODO extend it ^
    | TxpOverwhelmed -- ^ Local transaction storage is full -- can't accept more txs
    | TxpCantTopsort
    | TxpInvalidUndoLength !Int !Int
    deriving (Show, Eq)

instance Buildable TxpVerFailure where
    build = notImplemented
