-- | Binary serialization of core Txp types.

module Pos.Binary.Txp.Core
       (
       ) where

import           Universum

import           Pos.Binary.Class   (Bi (..), PokeWithSize, UnsignedVarInt (..),
                                     convertToSizeNPut, getWithLength, getWord8, label,
                                     pokeWithSize, putField, putWithLengthS, putWord8S)
import           Pos.Binary.Core    ()
import           Pos.Binary.Merkle  ()
import qualified Pos.Txp.Core.Types as T

----------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------

instance Bi T.TxIn where
    sizeNPut = putField T.txInHash <> putField (UnsignedVarInt . T.txInIndex)
    get = label "TxIn" $ T.TxIn <$> get <*> (getUnsignedVarInt <$> get)

instance Bi T.TxOut where
    sizeNPut = putField T.txOutAddress <> putField T.txOutValue
    get = label "TxOut" $ T.TxOut <$> get <*> get

instance Bi T.TxOutAux where
    sizeNPut = putField T.toaOut <> putField T.toaDistr
    get = label "TxOutAux" $ T.TxOutAux <$> get <*> get

instance Bi T.Tx where
    sizeNPut = putField T._txInputs
            <> putField T._txOutputs
            <> putField T._txAttributes
    get = label "Tx" $ do
        ins <- get
        outs <- get
        attrs <- get
        T.mkTx ins outs attrs

-- TODO CSL-1122
instance Bi T.TxInWitness where
    sizeNPut = convertToSizeNPut f
      where
        withLen :: Bi a => a -> PokeWithSize ()
        withLen = putWithLengthS . pokeWithSize

        f :: T.TxInWitness -> PokeWithSize ()
        f (T.PkWitness key sig) =
            putWord8S 0 <> withLen (key, sig)
        f (T.ScriptWitness val red) =
            putWord8S 1 <> withLen (val, red)
        f (T.RedeemWitness key sig) =
            putWord8S 2 <> withLen (key, sig)
        f (T.UnknownWitnessType t bs) =
            pokeWithSize @Word8 t <> pokeWithSize bs
    get = label "TxInWitness" $ do
        tag <- getWord8
        case tag of
            0 -> uncurry T.PkWitness <$> getWithLength get
            1 -> uncurry T.ScriptWitness <$> getWithLength get
            2 -> uncurry T.RedeemWitness <$> getWithLength get
            t -> getWithLength (T.UnknownWitnessType t <$> get)

instance Bi T.TxDistribution where
    sizeNPut = putField f
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

instance Bi T.TxSigData where
    sizeNPut =
        putField T.txSigInput
     <> putField T.txSigOutsHash
     <> putField T.txSigDistrHash
    get = label "TxSigData" $ do
        txSigInput     <- get
        txSigOutsHash  <- get
        txSigDistrHash <- get
        return T.TxSigData{..}

instance Bi T.TxAux where
    sizeNPut =
        putField T.taTx
     <> putField T.taWitness
     <> putField T.taDistribution
    get = label "DataMsg TxMsgContents" $ T.TxAux <$> get <*> get <*> get

instance Bi T.TxProof where
    sizeNPut =
        putField (UnsignedVarInt . T.txpNumber)
     <> putField T.txpRoot
     <> putField T.txpWitnessesHash
     <> putField T.txpDistributionsHash
    get = label "TxProof" $ do
      txpNumber <- getUnsignedVarInt <$> get
      txpRoot <- get
      txpWitnessesHash <- get
      txpDistributionsHash <- get
      return T.TxProof {..}

instance Bi T.TxPayload where
    sizeNPut =
        putField (\T.UnsafeTxPayload {..} ->
                 zip3 (toList _txpTxs) _txpWitnesses _txpDistributions)
    get = T.mkTxPayload =<< get
