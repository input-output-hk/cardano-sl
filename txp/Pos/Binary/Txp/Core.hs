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
    sizeNPut = labelS "Tx" $
        putField T._txInputs <>
        putField T._txOutputs <>
        putField T._txAttributes
    get = label "Tx" $ do
        ins <- get
        outs <- get
        attrs <- get
        T.mkTx ins outs attrs

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

instance Bi T.TxPayload where
    sizeNPut = labelS "TxPayload" $
        putField (\T.UnsafeTxPayload {..} ->
                 zip3 (toList _txpTxs) _txpWitnesses _txpDistributions)
    get = label "TxPayload" $ T.mkTxPayload =<< get
