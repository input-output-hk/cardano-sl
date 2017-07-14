-- | Binary serialization of core Txp types.

module Pos.Binary.Txp.Core
       (
       ) where

import           Universum

import           Pos.Binary.Class   (Bi (..), Cons (..), Field (..), deriveSimpleBi, enforceSize, encodeListLen,
                                     decodeListLen, matchSize, deserialize', serialize')
import           Pos.Binary.Core    ()
import           Pos.Binary.Merkle  ()
import qualified Pos.Core.Types     as T
import           Pos.Crypto.Hashing (Hash)
import qualified Pos.Txp.Core.Types as T

----------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------

instance Bi T.TxIn where
  encode txIn = encodeListLen 2 <> encode (T.txInHash txIn) <> encode (T.txInIndex txIn)
  decode = do
    enforceSize "TxIn" 2
    T.TxIn <$> decode <*> decode

deriveSimpleBi ''T.TxOut [
    Cons 'T.TxOut [
        Field [| T.txOutAddress :: T.Address |],
        Field [| T.txOutValue   :: T.Coin    |]
    ]]

deriveSimpleBi ''T.TxOutAux [
    Cons 'T.TxOutAux [
        Field [| T.toaOut   :: T.TxOut             |],
        Field [| T.toaDistr :: T.TxOutDistribution |]
    ]]

instance Bi T.Tx where
  encode tx =  encodeListLen 3
            <> encode (T._txInputs tx)
            <> encode (T._txOutputs tx)
            <> encode (T._txAttributes tx)
  decode = do
    enforceSize "Tx" 3
    res <- T.mkTx <$> decode <*> decode <*> decode
    case res of
      Left e   -> fail e
      Right tx -> pure tx

instance Bi T.TxInWitness where
  encode input = case input of
    T.PkWitness key sig         -> encodeListLen 2 <> encode (0 :: Word8)
                                                   <> encode (serialize' (key, sig))
    T.ScriptWitness val red     -> encodeListLen 2 <> encode (1 :: Word8)
                                                   <> encode (serialize' (val, red))
    T.RedeemWitness key sig     -> encodeListLen 2 <> encode (2 :: Word8)
                                                   <> encode (serialize' (key, sig))
    T.UnknownWitnessType tag bs -> encodeListLen 2 <> encode tag
                                                   <> encode bs
  decode = do
    len <- decodeListLen
    tag <- decode @Word8
    case tag of
      0 -> do
        matchSize len "TxInWitness.PkWitness" 2
        uncurry T.PkWitness . deserialize' <$> decode
      1 -> do
        matchSize len "TxInWitness.ScriptWitness" 2
        uncurry T.ScriptWitness . deserialize' <$> decode
      2 -> do
        matchSize len "TxInWitness.ScriptWitness" 2
        uncurry T.RedeemWitness . deserialize' <$> decode
      _ -> do
        matchSize len "TxInWitness.UnknownWitnessType" 2
        T.UnknownWitnessType tag <$> decode

instance Bi T.TxDistribution where
  encode = encode . T.getTxDistribution
  decode = T.TxDistribution <$> decode

deriveSimpleBi ''T.TxSigData [
    Cons 'T.TxSigData [
        Field [| T.txSigInput     :: T.TxIn                  |],
        Field [| T.txSigOutsHash  :: Hash (NonEmpty T.TxOut) |],
        Field [| T.txSigDistrHash :: Hash T.TxDistribution   |]
    ]]

deriveSimpleBi ''T.TxAux [
    Cons 'T.TxAux [
        Field [| T.taTx           :: T.Tx             |],
        Field [| T.taWitness      :: T.TxWitness      |],
        Field [| T.taDistribution :: T.TxDistribution |]
    ]]

instance Bi T.TxProof where
  encode proof =  encodeListLen 4
               <> encode (T.txpNumber proof)
               <> encode (T.txpRoot proof)
               <> encode (T.txpWitnessesHash proof)
               <> encode (T.txpDistributionsHash proof)
  decode = do
    enforceSize "TxProof" 4
    T.TxProof <$> decode <*>
                  decode <*>
                  decode <*>
                  decode

instance Bi T.TxPayload where
  encode T.UnsafeTxPayload{..} =
    encode $ zip3 (toList _txpTxs) _txpWitnesses _txpDistributions
  decode = do
    res <- T.mkTxPayload <$> decode
    case res of
      Left e    -> fail e
      Right txP -> pure txP
