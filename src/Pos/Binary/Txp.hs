-- | Binary serialization of Txp types

module Pos.Binary.Txp () where

import           Universum

import           Data.Binary.Get    (getWord8, label)
import           Data.Binary.Put    (putByteString, putWord8)

import           Pos.Binary.Class   (Bi (..), UnsignedVarInt (..), getRemainingByteString,
                                     getWithLength, putWithLength)
import           Pos.Binary.Core    ()
import           Pos.Binary.Merkle  ()
import qualified Pos.Txp.Core.Types as T

instance Bi T.TxIn where
    put (T.TxIn hash index) = put hash >> put (UnsignedVarInt index)
    get = label "TxIn" $ T.TxIn <$> get <*> (getUnsignedVarInt <$> get)

instance Bi T.TxOut where
    put (T.TxOut addr coin) = put addr >> put coin
    get = label "TxOut" $ T.TxOut <$> get <*> get

instance Bi T.TxOutAux where
    put (T.TxOutAux out distr) = put out >> put distr
    get = label "TxOutAux" $ T.TxOutAux <$> get <*> get

instance Bi T.Tx where
    put (T.UnsafeTx ins outs attrs) = put ins >> put outs >> put attrs
    get = label "Tx" $ do
        ins <- get
        outs <- get
        attrs <- get
        T.mkTx ins outs attrs

instance Bi T.TxInWitness where
    put (T.PkWitness key sig) = do
        putWord8 0
        putWithLength (put key >> put sig)
    put (T.ScriptWitness val red) = do
        putWord8 1
        putWithLength (put val >> put red)
    put (T.RedeemWitness key sig) = do
        putWord8 2
        putWithLength (put key >> put sig)
    put (T.UnknownWitnessType t bs) = do
        putWord8 t
        putWithLength (putByteString bs)
    get = label "TxInWitness" $ do
        tag <- getWord8
        case tag of
            0 -> getWithLength (T.PkWitness <$> get <*> get)
            1 -> getWithLength (T.ScriptWitness <$> get <*> get)
            2 -> getWithLength (T.RedeemWitness <$> get <*> get)
            t -> getWithLength (T.UnknownWitnessType t <$>
                                getRemainingByteString)

instance Bi T.TxDistribution where
    put (T.TxDistribution ds) =
        put $
        if all null ds
            then Left (UnsignedVarInt (length ds))
            else Right ds
    get = label "TxDistribution" $ T.TxDistribution <$> parseDistribution
      where
        parseDistribution =
            get >>= \case
                Left (UnsignedVarInt n) ->
                    maybe (fail "get@TxDistribution: empty distribution") pure $
                    nonEmpty $ replicate n []
                Right ds -> pure ds

instance Bi T.TxProof where
    put (T.TxProof {..}) = do
        put (UnsignedVarInt txpNumber)
        put txpRoot
        put txpWitnessesHash
        put txpDistributionsHash
    get = do
      txpNumber <- getUnsignedVarInt <$> get
      txpRoot <- get
      txpWitnessesHash <- get
      txpDistributionsHash <- get
      return T.TxProof {..}

instance Bi T.TxPayload where
    put (T.UnsafeTxPayload {..}) =
        put $ zip3 (toList _txpTxs) _txpWitnesses _txpDistributions
    get = T.mkTxPayload =<< get
