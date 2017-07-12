-- | Binary serialization of core Txp types.

module Pos.Binary.Txp.Core
       (
       ) where

import           Universum

import           Pos.Binary.Class   (Bi (..), Cons (..), Field (..), PokeWithSize,
                                     UnsignedVarInt (..), convertToSizeNPut,
                                     deriveSimpleBi, getBytes, getWithLength, getWord8,
                                     label, labelS, putBytesS, putField, putS,
                                     putWithLengthS, putWord8S)
import qualified Pos.Binary.Cbor    as Cbor
import           Pos.Binary.Core    ()
import           Pos.Binary.Merkle  ()
import qualified Pos.Core.Types     as T
import           Pos.Crypto.Hashing (Hash)
import qualified Pos.Txp.Core.Types as T

----------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------

instance Bi T.TxIn where
    sizeNPut = labelS "TxIn" $
        putField T.txInHash <>
        putField (UnsignedVarInt . T.txInIndex)
    get = label "TxIn" $
        T.TxIn <$> get <*> (getUnsignedVarInt <$> get)

instance Cbor.Bi T.TxIn where
  encode txIn = Cbor.encodeListLen 2 <> Cbor.encode (T.txInHash txIn) <> Cbor.encode (T.txInIndex txIn)
  decode = do
    Cbor.enforceSize "TxIn" 2
    T.TxIn <$> Cbor.decode <*> Cbor.decode

deriveSimpleBi ''T.TxOut [
    Cons 'T.TxOut [
        Field [| T.txOutAddress :: T.Address |],
        Field [| T.txOutValue   :: T.Coin    |]
    ]]

Cbor.deriveSimpleBi ''T.TxOut [
    Cbor.Cons 'T.TxOut [
        Cbor.Field [| T.txOutAddress :: T.Address |],
        Cbor.Field [| T.txOutValue   :: T.Coin    |]
    ]]

deriveSimpleBi ''T.TxOutAux [
    Cons 'T.TxOutAux [
        Field [| T.toaOut   :: T.TxOut             |],
        Field [| T.toaDistr :: T.TxOutDistribution |]
    ]]

Cbor.deriveSimpleBi ''T.TxOutAux [
    Cbor.Cons 'T.TxOutAux [
        Cbor.Field [| T.toaOut   :: T.TxOut             |],
        Cbor.Field [| T.toaDistr :: T.TxOutDistribution |]
    ]]

instance Bi T.Tx where
    sizeNPut = labelS "Tx" $
        putField T._txInputs <>
        putField T._txOutputs <>
        putField T._txAttributes
    get = label "Tx" $ do
        ins <- get
        outs <- get
        attrs <- get
        T.mkTx ins outs attrs

instance Cbor.Bi T.Tx where
  encode tx =  Cbor.encodeListLen 3
            <> Cbor.encode (T._txInputs tx)
            <> Cbor.encode (T._txOutputs tx)
            <> Cbor.encode (T._txAttributes tx)
  decode = do
    Cbor.enforceSize "Tx" 3
    res <- T.mkTx <$> Cbor.decode <*> Cbor.decode <*> Cbor.decode
    case res of
      Left e   -> fail e
      Right tx -> pure tx

instance Bi T.TxInWitness where
    sizeNPut = labelS "TxInWitness" $ convertToSizeNPut $ \case
        -- It's important that we use 'putWithTag' for all branches.
        T.PkWitness key sig ->
            putWithTag 0 $ putS key <> putS sig
        T.ScriptWitness val red ->
            putWithTag 1 $ putS val <> putS red
        T.RedeemWitness key sig ->
            putWithTag 2 $ putS key <> putS sig
        T.UnknownWitnessType t bs ->
            -- It's important that it's 'putBytesS' and not just 'putS'.
            putWithTag t $ putBytesS bs
      where
        -- | Put tag, then length of X, then X itself
        putWithTag :: Word8 -> PokeWithSize () -> PokeWithSize ()
        putWithTag t x = putWord8S t <> putWithLengthS x

    get = label "TxInWitness" $ do
        tag <- getWord8
        getWithLength $ \len -> case tag of
            0 -> T.PkWitness <$> get <*> get
            1 -> T.ScriptWitness <$> get <*> get
            2 -> T.RedeemWitness <$> get <*> get
            t -> T.UnknownWitnessType t <$> getBytes (fromIntegral len)

instance Cbor.Bi T.TxInWitness where
  encode input = case input of
    T.PkWitness key sig         -> Cbor.encodeListLen 3 <> Cbor.encode (0 :: Word8)
                                                        <> Cbor.encode (Cbor.serialize' (key, sig))
    T.ScriptWitness val red     -> Cbor.encodeListLen 3 <> Cbor.encode (1 :: Word8)
                                                        <> Cbor.encode (Cbor.serialize' (val, red))
    T.RedeemWitness key sig     -> Cbor.encodeListLen 3 <> Cbor.encode (2 :: Word8)
                                                        <> Cbor.encode (Cbor.serialize' (key, sig))
    T.UnknownWitnessType tag bs -> Cbor.encodeListLen 2 <> Cbor.encode tag
                                                        <> Cbor.encode bs
  decode = do
    len <- Cbor.decodeListLen
    tag <- Cbor.decode @Word8
    case tag of
      0 -> do
        Cbor.matchSize len "TxInWitness.PkWitness" 3
        uncurry T.PkWitness . Cbor.deserialize' <$> Cbor.decode
      1 -> do
        Cbor.matchSize len "TxInWitness.ScriptWitness" 3
        uncurry T.ScriptWitness . Cbor.deserialize' <$> Cbor.decode
      2 -> do
        Cbor.matchSize len "TxInWitness.ScriptWitness" 3
        uncurry T.ScriptWitness . Cbor.deserialize' <$> Cbor.decode
      _ -> do
        Cbor.matchSize len "TxInWitness.UnknownWitnessType" 2
        T.UnknownWitnessType tag <$> Cbor.decode

instance Bi T.TxDistribution where
    sizeNPut = labelS "TxDistribution" $ putField f
      where
        f (T.TxDistribution ds) =
            if all null ds then Left (UnsignedVarInt (length ds))
            else Right ds
    get = label "TxDistribution" $ T.TxDistribution <$> parseDistribution
      where
        parseDistribution =
            get >>= \case
                Left (UnsignedVarInt n) ->
                    maybe (fail "get@TxDistribution: empty distribution") pure $
                    nonEmpty $ replicate n []
                Right ds -> pure ds

instance Cbor.Bi T.TxDistribution where
  encode = Cbor.encode . T.getTxDistribution
  decode = T.TxDistribution <$> Cbor.decode

deriveSimpleBi ''T.TxSigData [
    Cons 'T.TxSigData [
        Field [| T.txSigInput     :: T.TxIn                  |],
        Field [| T.txSigOutsHash  :: Hash (NonEmpty T.TxOut) |],
        Field [| T.txSigDistrHash :: Hash T.TxDistribution   |]
    ]]

Cbor.deriveSimpleBi ''T.TxSigData [
    Cbor.Cons 'T.TxSigData [
        Cbor.Field [| T.txSigInput     :: T.TxIn                  |],
        Cbor.Field [| T.txSigOutsHash  :: Hash (NonEmpty T.TxOut) |],
        Cbor.Field [| T.txSigDistrHash :: Hash T.TxDistribution   |]
    ]]

deriveSimpleBi ''T.TxAux [
    Cons 'T.TxAux [
        Field [| T.taTx           :: T.Tx             |],
        Field [| T.taWitness      :: T.TxWitness      |],
        Field [| T.taDistribution :: T.TxDistribution |]
    ]]

Cbor.deriveSimpleBi ''T.TxAux [
    Cbor.Cons 'T.TxAux [
        Cbor.Field [| T.taTx           :: T.Tx             |],
        Cbor.Field [| T.taWitness      :: T.TxWitness      |],
        Cbor.Field [| T.taDistribution :: T.TxDistribution |]
    ]]

instance Bi T.TxProof where
    sizeNPut = labelS "TxProof" $
        putField (UnsignedVarInt . T.txpNumber) <>
        putField T.txpRoot <>
        putField T.txpWitnessesHash <>
        putField T.txpDistributionsHash
    get = label "TxProof" $ do
        txpNumber <- getUnsignedVarInt <$> get
        txpRoot <- get
        txpWitnessesHash <- get
        txpDistributionsHash <- get
        return T.TxProof {..}

instance Cbor.Bi T.TxProof where
  encode proof =  Cbor.encodeListLen 4
               <> Cbor.encode (T.txpNumber proof)
               <> Cbor.encode (T.txpRoot proof)
               <> Cbor.encode (T.txpWitnessesHash proof)
               <> Cbor.encode (T.txpDistributionsHash proof)
  decode = do
    Cbor.enforceSize "TxProof" 4
    T.TxProof <$> Cbor.decode <*>
                  Cbor.decode <*>
                  Cbor.decode <*>
                  Cbor.decode

instance Bi T.TxPayload where
    sizeNPut = labelS "TxPayload" $
        putField (\T.UnsafeTxPayload {..} ->
                 zip3 (toList _txpTxs) _txpWitnesses _txpDistributions)
    get = label "TxPayload" $ T.mkTxPayload =<< get

instance Cbor.Bi T.TxPayload where
  encode T.UnsafeTxPayload{..} =
    Cbor.encode $ zip3 (toList _txpTxs) _txpWitnesses _txpDistributions
  decode = do
    res <- T.mkTxPayload <$> Cbor.decode
    case res of
      Left e    -> fail e
      Right txP -> pure txP
