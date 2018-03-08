-- | Binary serialization of core Txp types.

module Pos.Binary.Core.Txp
       (
       ) where

import           Universum

import qualified Data.ByteString.Lazy as LBS

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), decodeKnownCborDataItem,
                                   decodeListLenCanonical, decodeUnknownCborDataItem,
                                   deriveSimpleBi, encodeKnownCborDataItem, encodeListLen,
                                   encodeUnknownCborDataItem, enforceSize, matchSize)
import           Pos.Binary.Core.Address ()
import           Pos.Binary.Merkle ()
import qualified Pos.Core.Common as Common
import qualified Pos.Core.Txp as T

----------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------

instance Bi T.TxIn where
    encode T.TxInUtxo{..} =
        encodeListLen 2 <>
        encode (0 :: Word8) <>
        encodeKnownCborDataItem (txInHash, txInIndex)
    encode (T.TxInUnknown tag bs) =
        encodeListLen 2 <>
        encode tag <>
        encodeUnknownCborDataItem (LBS.fromStrict bs)
    decode = do
        enforceSize "TxIn" 2
        tag <- decode @Word8
        case tag of
            0 -> uncurry T.TxInUtxo <$> decodeKnownCborDataItem
            _ -> T.TxInUnknown tag  <$> decodeUnknownCborDataItem

deriveSimpleBi ''T.TxOut [
    Cons 'T.TxOut [
        Field [| T.txOutAddress :: Common.Address |],
        Field [| T.txOutValue   :: Common.Coin    |]
    ]]

deriveSimpleBi ''T.TxOutAux [
    Cons 'T.TxOutAux [
        Field [| T.toaOut   :: T.TxOut |]
    ]]

instance Bi T.Tx where
    encode tx = encodeListLen 3
                <> encode (T._txInputs tx)
                <> encode (T._txOutputs tx)
                <> encode (T._txAttributes tx)
    decode = do
        enforceSize "Tx" 3
        T.UncheckedTx <$> decode <*> decode <*> decode

instance Bi T.TxInWitness where
    encode input = case input of
        T.PkWitness key sig         ->
            encodeListLen 2 <>
            encode (0 :: Word8) <>
            encodeKnownCborDataItem (key, sig)
        T.ScriptWitness val red     ->
            encodeListLen 2 <>
            encode (1 :: Word8) <>
            encodeKnownCborDataItem (val, red)
        T.RedeemWitness key sig     ->
            encodeListLen 2 <>
            encode (2 :: Word8) <>
            encodeKnownCborDataItem (key, sig)
        T.UnknownWitnessType tag bs ->
            encodeListLen 2 <>
            encode tag <>
            encodeUnknownCborDataItem (LBS.fromStrict bs)
    decode = do
        len <- decodeListLenCanonical
        tag <- decode @Word8
        case tag of
            0 -> do
                matchSize len "TxInWitness.PkWitness" 2
                uncurry T.PkWitness <$> decodeKnownCborDataItem
            1 -> do
                matchSize len "TxInWitness.ScriptWitness" 2
                uncurry T.ScriptWitness <$> decodeKnownCborDataItem
            2 -> do
                matchSize len "TxInWitness.RedeemWitness" 2
                uncurry T.RedeemWitness <$> decodeKnownCborDataItem
            _ -> do
                matchSize len "TxInWitness.UnknownWitnessType" 2
                T.UnknownWitnessType tag <$> decodeUnknownCborDataItem

instance Bi T.TxSigData where
    encode (T.TxSigData {..}) = encode txSigTxHash
    decode = T.TxSigData <$> decode

deriveSimpleBi ''T.TxAux [
    Cons 'T.TxAux [
        Field [| T.taTx           :: T.Tx             |],
        Field [| T.taWitness      :: T.TxWitness      |]
    ]]

instance Bi T.TxProof where
    encode proof =  encodeListLen 3
                 <> encode (T.txpNumber proof)
                 <> encode (T.txpRoot proof)
                 <> encode (T.txpWitnessesHash proof)
    decode = do
        enforceSize "TxProof" 3
        T.TxProof <$> decode <*>
                      decode <*>
                      decode

instance Bi T.TxPayload where
    encode T.UncheckedTxPayload {..} = encode $ zip (toList _txpTxs) _txpWitnesses
    decode = T.mkTxPayload <$> decode
