{-# LANGUAGE DeriveAnyClass #-}

-- | Toil failures.

module Pos.Txp.Toil.Failure
       ( ToilVerFailure (..)
       , WitnessVerFailure (..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, build, int, shown, stext, (%))
import           Serokell.Data.Memory.Units (Byte, memory)
import           Serokell.Util (listJson)

import           Pos.Core (Address, HeaderHash, ScriptVersion, TxFeePolicy, addressDetailedF)
import           Pos.Core.Txp (TxIn, TxInWitness, TxOut (..))
import           Pos.Data.Attributes (UnparsedFields)
import           Pos.Script (PlutusError)
import           Pos.Txp.Toil.Types (TxFee)

----------------------------------------------------------------------------
-- ToilVerFailure
----------------------------------------------------------------------------

-- | Result of transaction processing
data ToilVerFailure
    = ToilKnown -- ^ Transaction is already in the storage (cache)
    | ToilTipsMismatch { ttmOldTip :: !HeaderHash
                       , ttmNewTip :: !HeaderHash}
    | ToilSlotUnknown
    | ToilOverwhelmed !Int -- ^ Local transaction storage is full --
                            -- can't accept more txs. Current limit is attached.
    | ToilNotUnspent !TxIn -- ^ Tx input is not a known unspent input.
    | ToilOutGreaterThanIn { tInputSum  :: !Integer
                           , tOutputSum :: !Integer}
    | ToilInconsistentTxAux !Text
    | ToilInvalidOutputs !Text  -- [CSL-1628] TODO: make it more informative
    | ToilUnknownInput !Word32 !TxIn

    -- | The witness can't be used to justify spending an output – either
    --     * it has a wrong type, e.g. PKWitness for a script address, or
    --     * it has the right type but doesn't match the address, e.g. the
    --       hash of key in PKWitness is not equal to the address.
    | ToilWitnessDoesntMatch { twdmInputIndex  :: !Word32
                             , twdmInput       :: !TxIn
                             , twdmSpentOutput :: !TxOut
                             , twdmWitness     :: !TxInWitness }

    -- | The witness could in theory justify spending an output, but it
    -- simply isn't valid (the signature doesn't pass validation, the
    -- validator–redeemer pair produces 'False' when executed, etc).
    | ToilInvalidWitness { tiwInputIndex :: !Word32
                         , tiwWitness    :: !TxInWitness
                         , tiwReason     :: !WitnessVerFailure }

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
    | ToilNonBootstrapDistr !(NonEmpty Address)
    | ToilRepeatedInput
    deriving (Show, Eq)

instance Exception ToilVerFailure

instance Buildable ToilVerFailure where
    build ToilKnown =
        "transaction already is in the mem pool"
    build (ToilTipsMismatch dbTip localTip) =
        bprint ("Something is bad with this node, tips mismatch, "%
                "tip from DB is "%build%", local tip is "%build)
        dbTip localTip
    build ToilSlotUnknown =
        "can't process, current slot is unknown"
    build (ToilOverwhelmed limit) =
        bprint ("max size of the mem pool is reached which is "%shown) limit
    build (ToilNotUnspent txId) =
        bprint ("input is not a known unspent input: "%build) txId
    build (ToilOutGreaterThanIn {..}) =
        bprint ("sum of outputs is greater than sum of inputs ("%int%" < "%int%")")
        tInputSum tOutputSum
    build (ToilInconsistentTxAux msg) =
        bprint ("TxAux is inconsistent: "%stext) msg
    build (ToilInvalidOutputs msg) =
        bprint ("outputs are invalid: "%stext) msg
    build (ToilWitnessDoesntMatch i txIn txOut@TxOut {..} witness) =
        bprint ("input #"%int%"'s witness doesn't match address "%
                "of corresponding output:\n"%
                "  input: "%build%"\n"%
                "  output spent by this input: "%build%"\n"%
                "  address details: "%addressDetailedF%"\n"%
                "  witness: "%build)
            i txIn txOut txOutAddress witness
    build (ToilInvalidWitness i witness reason) =
        bprint ("input #"%int%"'s witness doesn't pass verification:\n"%
                "  witness: "%build%"\n"%
                "  reason: "%build)
            i witness reason
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
    build (ToilNonBootstrapDistr addresses) =
        bprint ("we are in bootstrap era, but some addresses have distribution"%
                " which is not 'BootstrapEraDistr': "%listJson) addresses
    build ToilRepeatedInput =
        "transaction tries to spend an unspent input more than once"
    build (ToilUnknownInput inpId txIn) =
       bprint ("vtcVerifyAllIsKnown is True, but the input #"%int%" "%build%" is unknown") inpId txIn

----------------------------------------------------------------------------
-- WitnessVerFailure
----------------------------------------------------------------------------

-- | Result of checking a witness.
data WitnessVerFailure
    -- | The signature of a 'PKWitness' doesn't pass validation
    = WitnessWrongSignature
    -- | Validator and redeemer script versions don't match
    | WitnessScriptVerMismatch ScriptVersion ScriptVersion
    -- | Don't know how to handle script version
    | WitnessUnknownScriptVer ScriptVersion
    -- | Plutus error (e.g. exhausted execution steps, redeemer script
    -- returning 'False', etc)
    | WitnessScriptError PlutusError
    -- | Don't know how to handle this witness type
    | WitnessUnknownType Word8
    deriving (Show, Eq, Generic, NFData)

instance Buildable WitnessVerFailure where
    build WitnessWrongSignature =
        bprint "the signature in the witness doesn't pass validation"
    build (WitnessScriptVerMismatch val red) =
        bprint ("validator and redeemer script versions don't match: "%
                "validator version = "%build%", script version = "%build) val red
    build (WitnessUnknownScriptVer ver) =
        bprint ("unknown/unhandleable script version: "%build) ver
    build (WitnessScriptError err) =
        bprint ("error when executing scripts: "%build) err
    build (WitnessUnknownType t) =
        bprint ("unknown witness type: "%build) t
