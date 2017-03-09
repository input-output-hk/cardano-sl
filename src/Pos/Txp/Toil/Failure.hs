-- | Txp failures.

module Pos.Txp.Toil.Failure
       ( ToilVerFailure (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting           (bprint, build, int, stext, (%))
import           Serokell.Util.Verify (formatAllErrors)
import           Universum

import           Pos.Txp.Core         (TxIn)

-- | Result of transaction processing
data ToilVerFailure
    = ToilKnown -- ^ Transaction is already in the storage (cache)
    | ToilInvalid !Text
    | ToilOverwhelmed -- ^ Local transaction storage is full -- can't accept more txs
    | ToilCantTopsort
    | ToilInvalidUndoLength !Int
                            !Int
    | ToilNotUnspent !TxIn  -- ^ Tx input is not a known unspent input.
    | ToilOutGTIn { tInputSum  :: !Integer
                 ,  tOutputSum :: !Integer}
    | ToilInconsistentTxAux !Text
    | ToilInvalidOutputs !Text
    | ToilInvalidInputs ![Text]
    deriving (Show, Eq)

instance Buildable ToilVerFailure where
    build ToilKnown =
        "transaction already is in the mem pool"
    build (ToilInvalid txt) =
        bprint stext txt
    build ToilOverwhelmed =
        "max size of the mem pool is reached"
    build ToilCantTopsort =
        "transactions can't be topsored"
    build (ToilInvalidUndoLength ex rcv) =
        bprint ("length of transaction's inputs = "%int%
               " doesn't equal length of transaction's undo = "%int) ex rcv
    build (ToilNotUnspent txId) =
        bprint ("input is not a known unspent input: "%build) txId
    build (ToilOutGTIn {..}) =
        bprint ("sum of outputs is greater than sum of inputs ("%int%" < "%int%")")
        tInputSum tOutputSum
    build (ToilInconsistentTxAux msg) =
        bprint ("TxAux is inconsistent: "%stext) msg
    build (ToilInvalidOutputs msg) =
        bprint ("outputs are invalid: "%stext) msg
    build (ToilInvalidInputs msg) =
        bprint ("inputs are invalid: "%stext) $ formatAllErrors msg
