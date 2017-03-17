-- | Txp failures.

module Pos.Txp.Toil.Failure
       ( ToilVerFailure (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting                 (bprint, build, int, stext, (%))
import           Serokell.Data.Memory.Units (Byte, memory)
import           Serokell.Util.Verify       (formatAllErrors)
import           Universum

import           Pos.Core                   (HeaderHash)
import           Pos.Txp.Core               (TxIn)

-- | Result of transaction processing
data ToilVerFailure
    = ToilKnown -- ^ Transaction is already in the storage (cache)
    | ToilTipsMismatch { ttmOldTip :: !HeaderHash
                       , ttmNewTip :: !HeaderHash}
    | ToilOverwhelmed -- ^ Local transaction storage is full -- can't accept more txs
    | ToilNotUnspent !TxIn -- ^ Tx input is not a known unspent input.
    | ToilOutGTIn { tInputSum  :: !Integer
                 ,  tOutputSum :: !Integer}
    | ToilInconsistentTxAux !Text
    | ToilInvalidOutputs !Text  -- [CSL-814] TODO: make it more informative
    | ToilInvalidInputs ![Text] -- [CSL-814] TODO: make it more informative
    | ToilTooLargeTx { ttltSize  :: !Byte
                     , ttltLimit :: !Byte}
    deriving (Show, Eq)

instance Buildable ToilVerFailure where
    build ToilKnown =
        "transaction already is in the mem pool"
    build (ToilTipsMismatch oldTip newTip) =
        bprint ("tips mismatch, old tip is "%build%", new one is"%build)
        oldTip newTip
    build ToilOverwhelmed =
        "max size of the mem pool is reached"
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
    build (ToilTooLargeTx {..}) =
        bprint ("transaction's size exceeds limit "%
                "("%memory%" > "%memory%")") ttltSize ttltLimit
