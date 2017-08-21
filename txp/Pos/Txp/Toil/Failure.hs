-- | Toil failures.

module Pos.Txp.Toil.Failure
       ( ToilVerFailure (..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting                 (bprint, build, int, shown, stext, (%))
import           Serokell.Data.Memory.Units (Byte, memory)

import           Pos.Core                   (HeaderHash, TxFeePolicy, addressDetailedF)
import           Pos.Data.Attributes        (UnparsedFields)
import           Pos.Txp.Core               (TxIn, TxInWitness, TxOut (..))
import           Pos.Txp.Toil.Types         (TxFee)

-- | Result of transaction processing
data ToilVerFailure
    = ToilKnown -- ^ Transaction is already in the storage (cache)
    | ToilTipsMismatch { ttmOldTip :: !HeaderHash
                       , ttmNewTip :: !HeaderHash}
    | ToilSlotUnknown
    | ToilOverwhelmed !Byte -- ^ Local transaction storage is full --
                            -- can't accept more txs. Current limit is attached.
    | ToilNotUnspent !TxIn -- ^ Tx input is not a known unspent input.
    | ToilOutGTIn { tInputSum  :: !Integer
                  , tOutputSum :: !Integer}
    | ToilInconsistentTxAux !Text
    | ToilInvalidOutputs !Text  -- [CSL-814] TODO: make it more informative
    | ToilInvalidInput !Word32 !Text -- [CSL-814] TODO: make it more informative
    | ToilWitnessDoesntMatch !Word32 !TxIn !TxOut !TxInWitness
    | ToilTooLargeTx { ttltSize  :: !Byte
                     , ttltLimit :: !Byte}
    | ToilInvalidMinFee { timfPolicy :: !TxFeePolicy
                        , timfReason :: !Text
                        , timfSize   :: !Byte }
    | ToilInsufficientFee { tifPolicy :: !TxFeePolicy
                          , tifFee    :: !TxFee
                          , tifMinFee :: !TxFee
                          , tifSize   :: !Byte }
    | ToilUnknownAttributes !UnparsedFields
    | ToilBootInappropriate !Text
    | ToilRepeatedInput
    deriving (Show, Eq)

instance Exception ToilVerFailure

instance Buildable ToilVerFailure where
    build ToilKnown =
        "transaction already is in the mem pool"
    build (ToilTipsMismatch dbTip localTip) =
        bprint ("tips mismatch, tip from DB is "%build%", local tip is "%build)
        dbTip localTip
    build ToilSlotUnknown =
        "can't process, current slot is unknown"
    build (ToilOverwhelmed limit) =
        bprint ("max size of the mem pool is reached which is "%memory) limit
    build (ToilNotUnspent txId) =
        bprint ("input is not a known unspent input: "%build) txId
    build (ToilOutGTIn {..}) =
        bprint ("sum of outputs is greater than sum of inputs ("%int%" < "%int%")")
        tInputSum tOutputSum
    build (ToilInconsistentTxAux msg) =
        bprint ("TxAux is inconsistent: "%stext) msg
    build (ToilInvalidOutputs msg) =
        bprint ("outputs are invalid: "%stext) msg
    build (ToilInvalidInput i msg) =
        bprint ("input #"%int%" is invalid: "%stext) i msg
    build (ToilWitnessDoesntMatch i txIn txOut@TxOut {..} witness) =
        bprint ("input #"%int%"'s witness doesn't match address "%
                "of corresponding output:\n"%
                "  input: "%build%"\n"%
                "  output spent by this input: "%build%"\n"%
                "  address details: "%addressDetailedF%"\n"%
                "  witness: "%build)
            i txIn txOut txOutAddress witness
    build (ToilTooLargeTx {..}) =
        bprint ("transaction's size exceeds limit "%
                "("%memory%" > "%memory%")") ttltSize ttltLimit
    build (ToilInvalidMinFee {..}) =
        bprint (build%" generates invalid minimal fee on a "%
                "transaction of size "%memory%", reason: "%stext)
            timfPolicy
            timfSize
            timfReason
    build (ToilInsufficientFee {..}) =
        bprint ("transaction of size "%memory%" does not adhere to "%
                build%"; it has fee "%build%" but needs "%build)
            tifSize
            tifPolicy
            tifFee
            tifMinFee
    build (ToilUnknownAttributes uf) =
        bprint ("transaction has unknown attributes: "%shown) uf
    build (ToilBootInappropriate msg) =
        bprint ("transaction is not suitable for boot era: "%stext) msg
    build ToilRepeatedInput =
        "transaction tries to spent an unspent input more than once"
